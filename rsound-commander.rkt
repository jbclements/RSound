#lang racket

(require (planet clements/portaudio:1:4)
         (only-in ffi/unsafe cpointer? ptr-set! _sint16)
         ffi/vector
         racket/async-channel)

(define (nonnegative-integer? n) 
  (and (exact-integer? n)
       (<= 0 n)))

(define (positive-integer? n)
  (and (exact-integer? n)
       (< 0 n)))

(define frames? nonnegative-integer?)
(define sample-rate? positive-integer?)

(provide/contract (buffer-play (-> s16vector?
                                   sample-rate?
                                   void?))
                  #;(buffer-loop (-> cpointer?
                                   frames?
                                   sample-rate?
                                   void?))
                  [signal->signal/block/unsafe
                   (-> procedure? procedure?)]
                  (signal/block-play (-> any/c sample-rate? void?))
                  (signal/block-play/unsafe (-> any/c sample-rate? void?))
                  (stop-playing (-> void?))
                  [channels positive-integer?])


;; given an s16-vector containing interlaced 2-channel samples,
;; play it back at the current sample rate.
(define (buffer-play s16vec sample-rate)
  (pa-maybe-initialize)
  (define sndinfo-record (make-sndplay-record s16vec))
  (define stream (open-rsound-stream copying-callback
                                     sndinfo-record
                                     sample-rate))
  (pa-set-stream-finished-callback stream copying-info-free)
  (pa-start-stream stream)
  (async-channel-put
   live-stream-channel
   (lambda () (pa-stop-stream stream))))

;; channels... don't change this, unless 
;; you also change the copying-callback.
(define channels 2)

(define (open-rsound-stream callback closure-info-ptr sample-rate)
  (define sample-rate/i (exact->inexact sample-rate))
  (pa-open-default-stream
   0             ;; input channels
   channels      ;; output channels
   'paInt16      ;; sample format
   sample-rate/i ;; sample rate
   256           ;;frames-per-buffer
   callback      ;; callback (NULL means just wait for data)
   closure-info-ptr))


;; check-below-threshold : rsound threshhold -> (void)
;; signals an error if any sample is above the threshold
;; IRRELEVANT IN THE S16INT WORLD
#;(define (check-below-threshold buffer frames threshold)
  (for ([i (in-range (* channels frames))])
    (when (> (ptr-ref buffer _float i) threshold)
      (error 'check-below-threshold "sound contains samples above threshold ~s."
             threshold))))

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


(define (signal/block-play block-filler sample-rate)
  (match-define (list stream-time stop-sound)
    (stream-play block-filler default-buffer-frames sample-rate))
  (async-channel-put 
   live-stream-channel
   (lambda () (stop-sound))))

(define (signal/block-play/unsafe block-filler sample-rate)
  (match-define (list stream-time stop-sound)
    (stream-play/unsafe block-filler default-buffer-frames sample-rate))
  (async-channel-put 
   live-stream-channel
   (lambda () (stop-sound))))

(define (signal->signal/block/unsafe signal)
  (define (signal/block/unsafe ptr frames idx)
    (define base-t (* frames idx))
    (for ([frame (in-range 0 frames)]
          [t (in-range base-t (+ base-t frames))])
      (define sample (real->s16 (signal t)))
      (define sample-num (* frame channels))
      (ptr-set! ptr _sint16 sample-num sample)
      (ptr-set! ptr _sint16 (add1 sample-num) sample)))
  signal/block/unsafe)


(define default-buffer-frames 1024)

;; CONVERSIONS

(define s16max 32767)
(define -s16max (- s16max))
(define s16max/i (exact->inexact 32767))

(define (s16->real x)
  (/ (exact->inexact x) s16max/i))

(define (real->s16 x)
  (min s16max (max -s16max (inexact->exact (round (* s16max/i x))))))