#lang racket

(require (planet clements/rack-portaudio)
         (only-in ffi/unsafe cpointer?)
         ffi/vector)

(define (frames? n) 
  (and (exact-integer? n)
       (<= 0 n)))
(define (sample-rate? n)
  (and (exact-integer? n)
       (< 0 n)))

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
                  #;(stop-playing (-> void?)))

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
  (pa-start-stream stream))

;; channels... don't change this, unless 
;; you also change the copying-callback.
(define channels 2)

(define (open-rsound-stream callback closure-info-ptr sample-rate)
    (pa-open-default-stream
     0             ;; input channels
     channels      ;; output channels
     'paInt16      ;; sample format
     sample-rate   ;; sample rate
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
