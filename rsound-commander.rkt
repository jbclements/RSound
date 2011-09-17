#lang racket

(require (planet clements/rack-portaudio)
         (only-in ffi/unsafe cpointer?)
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
                  (signal-play (-> any/c ;; don't want to slow down calls to the signal
                                   sample-rate?
                                   void?))
                  #;(signal/block-play (-> any/c
                                         sample-rate?
                                         void?))
                  (stop-playing (-> void?))
                  [channels positive-integer?])

(define (signal-play fun sample-rate)
  (error 'signal-play "not working now"))

;; given an s16-vector containing interlaced 2-channel samples,
;; play it back at the current sample rate.
(define (buffer-play s16vec sample-rate)
  (pa-maybe-initialize)
  (define sndinfo-record (make-sndplay-record s16vec))
  (define stream (open-rsound-stream copying-callback
                                     sndinfo-record
                                     sample-rate))
  (pa-start-stream stream)
  (async-channel-put 
   live-stream-channel
   (lambda () (stop-sound sndinfo-record))))

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



