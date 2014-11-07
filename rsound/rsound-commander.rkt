#lang racket/base

;; other than the special-case "diagnose.rkt", this is the
;; only file in rsound that depends on portaudio, so
;; all of the dependencies come through here.

;; portaudio provides a ton of stuff, but only a few of
;; them are used here, and they're listed explicitly 
;; in the require:

(require (only-in portaudio 
                  pa-maybe-initialize
                  pa-terminate-completely
                  s16vec-play
                  stream-play
                  stream-play/unsafe
                  s16vec-record
                  host-api
                  all-host-apis)
         racket/contract
         (only-in ffi/unsafe cpointer? ptr-set! _sint16 cast _pointer)
         ffi/vector
         ffi/unsafe/custodian
         racket/unsafe/ops
         racket/async-channel)

;; this file uses the term "signal" to refer exclusively to ( -> Real) 
;; procedures; it doesn't handle the network form.

(provide
 (contract-out
  (buffer-play (-> s16vector?
                   exact-integer?
                   (or/c false? exact-integer?)
                   nonnegative-real?
                   void?))
  [signal->signal/block/unsafe
   (-> procedure? signal/block/unsafe/c)]
  [signal/16->signal/block/unsafe
   (-> procedure? signal/block/unsafe/c)]
  [signal/block-play
   (-> signal/block/unsafe/c 
       nonnegative-real? 
       (or/c nonnegative-real? false?) 
       (-> nonnegative-real?))]
  [signal/block-play/unsafe (-> signal/block/unsafe/c nonnegative-real? (or/c nonnegative-real? false?) 
                                (-> nonnegative-real?))]
  [stop-playing (-> void?)]
  [channels exact-nonnegative-integer?])
 s16vec-record)

(define nat? exact-nonnegative-integer?)
(define frames? nat?)

(define signal/block/unsafe/c
  (-> cpointer?
      frames?
      void?))

(define signal-block/c
  (-> procedure?
      ;; don't want to impose a per-frame contract check:
      #;(-> nat? ;; index
          nat? ;; value
          void?)
      frames?
      void?))

(define (false? x) (eq? x #f))

(define (nonnegative-real? n)
  (and (real? n) (not (negative? n))))

;; channels... don't change this, unless 
;; you also change the copying-callback (and all of 
;; the portaudio package)
(define channels 2)

;; this wrapper just discards its argument, to fit the API for
;; register-custodian shutdown
(define (pa-terminate-completely-caller dc)
  (pa-terminate-completely))

;; make sure that pa-terminate gets called.
(define unregister-ptr
  (register-custodian-shutdown #f pa-terminate-completely-caller))

;; initialize portaudio
(pa-maybe-initialize)

;; NOTE: there's a certain amount of peculiar conservatism here;
;; in principle, it should be possible to use pa-initialize and
;; pa-terminate, rather than pa-maybe-initialize and pa-terminate-completely,
;; since each terminate should be paired with one initialize.
;; I believe that what I'm doing here is less likely to cause problems.


;; STOPPING PLAYBACK
(define (stop-playing)
  (call-all-stop-thunks))

;; this channel's events are thunks that kill playback
(define live-stream-channel (make-async-channel))

;; drain the live-stream-channel, calling each thunk.
(define (call-all-stop-thunks)
  (define maybe-thunk (async-channel-try-get live-stream-channel))
  (when maybe-thunk
    (maybe-thunk)
    (call-all-stop-thunks)))

;; a wrapper for portaudio's s16vec-play, that
;; saves a stopper in the global channel
(define (buffer-play s16vec start finish sample-rate)
  (define stop-sound (s16vec-play s16vec start finish sample-rate))
  (async-channel-put 
   live-stream-channel
   (lambda () (stop-sound))))

;; a wrapper for portaudio's signal/block-play, that
;; uses the default buffer size and saves a stopper
;; in the global channel
(define (signal/block-play block-filler sample-rate buffer-time)
  (define actual-buffer-time (or buffer-time default-buffer-time))
  (define stream-play-result (stream-play block-filler actual-buffer-time sample-rate))
  (define stream-time (car stream-play-result))
  (define stop-sound (caddr stream-play-result))
  (async-channel-put 
   live-stream-channel
   (lambda () (stop-sound)))
  stream-time)

;; a wrapper for portaudio's signal/block-play/unsafe, that
;; uses the default buffer size and saves a stopper
;; in the global channel
(define (signal/block-play/unsafe block-filler sample-rate buffer-time)
  (define actual-buffer-time (or buffer-time default-buffer-time))
  (define stream-play-result (stream-play/unsafe block-filler actual-buffer-time sample-rate))
  (define stream-time (car stream-play-result))
  (define stop-sound (caddr stream-play-result))
  (async-channel-put 
   live-stream-channel
   (lambda () (stop-sound)))
  stream-time)

;; given a function that produces reals, produces a signal/block/unsafe;
;; that is, a function that can fill a full buffer on
;; each call.
(define (signal->signal/block/unsafe sample-maker)
  (define (signal/block/unsafe ptr frames)
    (define s16vec
      (cast ptr _pointer (_s16vector o frames)))
    (for ([frame (in-range 0 frames)])
      (define sample (real->s16 (sample-maker)))
      (define sample-num (* frame channels))
      (unsafe-s16vector-set! s16vec sample-num sample)
      (unsafe-s16vector-set! s16vec (add1 sample-num) sample)))
  signal/block/unsafe)

;; given a function that produces s16s, produce a signal/block/unsafe.
;; use unsafe-16vector-set! for *MASSIVE SPEEDUP* (about 10x)
(define (signal/16->signal/block/unsafe sample-maker)
  (define (signal/block/unsafe ptr frames)
    (define s16vec (cast ptr
                         _pointer
                         (_s16vector o frames)))
    (for ([frame (in-range 0 frames)])
      (define sample (sample-maker))
      (define sample-num (* frame channels))
      (unsafe-s16vector-set! s16vec sample-num sample)
      (unsafe-s16vector-set! s16vec (add1 sample-num) sample)))
  signal/block/unsafe)

;; set default buffer time
(define default-buffer-time 
  (case (system-type)
    [(windows) 0.06]
    [(macosx unix) 0.05]))

;; set default API on windows to be WASAPI, if it's legal:
(case (system-type)
  [(windows) (cond [(member 'paWASAPI (all-host-apis)) 
                    (host-api 'paWASAPI)]
                   [else (void)])]
  [else (void)])


;; CONVERSIONS

(define s16max 32767)
(define -s16max (- s16max))
(define s16max/i (exact->inexact 32767))

(define (s16->real x)
  (/ (exact->inexact x) s16max/i))

(define (real->s16 x)
  (min s16max (max -s16max (inexact->exact (round (* s16max/i x))))))