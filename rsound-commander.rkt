#lang racket

(require (planet clements/portaudio:3:1)
         (only-in ffi/unsafe cpointer? ptr-set! _sint16)
         ffi/vector
         racket/async-channel)

(define (nonnegative-real? n)
  (and (real? n) (not (negative? n))))

(provide/contract (buffer-play (-> s16vector?
                                   exact-integer?
                                   (or/c false? exact-integer?)
                                   nonnegative-real?
                                   void?))
                  #;(buffer-loop (-> cpointer?
                                   frames?
                                   nonnegative-real?
                                   void?))
                  [signal->signal/block/unsafe
                   (-> procedure? procedure?)]
                  [signal/16->signal/block/unsafe
                   (-> procedure? procedure?)]
                  [signal/block-play 
                   (-> procedure? nonnegative-real? 
                       (or/c nonnegative-real? false?) 
                       (-> nonnegative-real?))]
                  [signal/block-play/unsafe (-> procedure? nonnegative-real? (or/c nonnegative-real? false?) 
                                                (-> nonnegative-real?))]
                  [stop-playing (-> void?)]
                  [channels exact-nonnegative-integer?])



;; channels... don't change this, unless 
;; you also change the copying-callback.
(define channels 2)


;; STOPPING PLAYBACK
(define (stop-playing)
  (call-all-stop-thunks))

;; this channel's events are thunks that kill playback
(define live-stream-channel (make-async-channel))

;; drain the live-stream-channel, calling each thunk.
(define (call-all-stop-thunks)
  (match (async-channel-try-get live-stream-channel)
    [#f (void)]
    [thunk (thunk)
           (call-all-stop-thunks)]))

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
  (match-define (list stream-time stats stop-sound)
    (stream-play block-filler actual-buffer-time sample-rate))
  (async-channel-put 
   live-stream-channel
   (lambda () (stop-sound)))
  stream-time)

;; a wrapper for portaudio's signal/block-play/unsafe, that
;; uses the default buffer size and saves a stopper
;; in the global channel
(define (signal/block-play/unsafe block-filler sample-rate buffer-time)
  (define actual-buffer-time (or buffer-time default-buffer-time))
  (match-define (list stream-time stats stop-sound)
    (stream-play/unsafe block-filler actual-buffer-time sample-rate))
  (async-channel-put 
   live-stream-channel
   (lambda () (stop-sound)))
  stream-time)

;; given a function that produces reals, produces a signal/block/unsafe;
;; that is, a function that can fill a full buffer on
;; each call.
(define (signal->signal/block/unsafe sample-maker)
  (define (signal/block/unsafe ptr frames)
    (for ([frame (in-range 0 frames)])
      (define sample (real->s16 (sample-maker)))
      (define sample-num (* frame channels))
      (ptr-set! ptr _sint16 sample-num sample)
      (ptr-set! ptr _sint16 (add1 sample-num) sample)))
  signal/block/unsafe)

;; given a function that produces s16s, produce a signal/block/unsafe.
(define (signal/16->signal/block/unsafe sample-maker)
  (define (signal/block/unsafe ptr frames)
    (for ([frame (in-range 0 frames)])
      (define sample (sample-maker))
      (define sample-num (* frame channels))
      (ptr-set! ptr _sint16 sample-num sample)
      (ptr-set! ptr _sint16 (add1 sample-num) sample)))
  signal/block/unsafe)

;; set default buffer time
(define default-buffer-time 
  (case (system-type)
    [(windows) 0.06]
    [(macosx unix) 0.05]))

;; set default API on windows to be WASAPI....
(case (system-type)
  [(windows) (host-api 'paWASAPI)]
  [else #f])


;; CONVERSIONS

(define s16max 32767)
(define -s16max (- s16max))
(define s16max/i (exact->inexact 32767))

(define (s16->real x)
  (/ (exact->inexact x) s16max/i))

(define (real->s16 x)
  (min s16max (max -s16max (inexact->exact (round (* s16max/i x))))))