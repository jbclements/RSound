#lang racket

(require "../main.rkt"
         "../private/s16vector-add.rkt"
         ffi/vector
         ffi/unsafe
         rackunit
         )

(define src-buf-len 10000)
(define simple-tone
  (make-tone 440 0.2 src-buf-len))

(define data-ptr (s16vector->cpointer (rsound-data simple-tone)))
;; it comes out even on this many samples:
(define sine-wave-len 8820)
(check-= (rs-ith/left simple-tone 4410) 0.0 0.01)
(check-= (rs-ith/right simple-tone 4410) 0.0 0.01)

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



(printf "playing signal for 3 seconds\n")
(signal/block-play/unsafe simple-signal/block/s16 44100)
(sleep 3)
(stop)

(printf "playing signal for 3 seconds w/ explicit buffer-time\n")
(set! t 0)
(signal/block-play/unsafe simple-signal/block/s16 44100 #:buffer-time 0.1)
(sleep 3)
(stop)
