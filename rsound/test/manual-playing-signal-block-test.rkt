#lang racket

(require "../main.rkt"
         "../private/s16vector-add.rkt"
         ffi/vector
         ffi/unsafe
         rackunit
         )

(define (print/flush str)
  (display str)
  (flush-output))

(define src-buf-len 10000)
(define simple-tone
  (make-tone 440 0.2 src-buf-len))

(define data-ptr (s16vector->cpointer (rsound-data simple-tone)))
;; it comes out even on this many samples:
;; not sure how to compute this automatically yet with sample-rate changing,
;; want smallest multiple of sr/440 that's an integer, but still bigger
;; than 5000 or so.
(define sine-wave-len 9600 #;8820)
(define half-wave-len (/ sine-wave-len 2))
;; tests won't work unless 
(check-= (rs-ith/left simple-tone half-wave-len) 0.0 0.01)
(check-= (rs-ith/right simple-tone half-wave-len) 0.0 0.01)


(module+ main

  (define ts empty)
(define lens empty)

(define t 0)
(define (simple-signal/block/s16 pointer frames)
  (set! ts (cons t ts))
  (define sample (* t CHANNELS))
  (define samples (* frames CHANNELS))
  (define copy-source (modulo sample sine-wave-len))
  (if (< (* CHANNELS src-buf-len) (+ copy-source samples))
      (error "sample buffer only goes to ~s, needed ~s"
             src-buf-len (+ copy-source samples))
      (begin
        (memset pointer 0 (* s16-size samples))
        (s16buffer-add!/c pointer
                          (ptr-add data-ptr (* s16-size copy-source))
                          samples)))
  (set! t (+ t frames)))


(print/flush "playing signal for 3 seconds\n")
(signal/block-play/unsafe simple-signal/block/s16 44100)
(sleep 3)
(stop)

  (print/flush "playing signal at 48K for 3 seconds\n")
(signal/block-play/unsafe simple-signal/block/s16 48000)
(sleep 3)
(stop)

(print/flush "playing signal for 3 seconds w/ explicit buffer-time\n")
(set! t 0)
(signal/block-play/unsafe simple-signal/block/s16 44100 #:buffer-time 0.1)
(sleep 3)
(stop))
