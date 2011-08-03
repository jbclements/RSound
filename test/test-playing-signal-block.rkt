#lang racket

(require (planet clements/rsound)
         (planet clements/rsound/private/s16vector-add)
         ffi/vector
         ffi/unsafe
         rackunit
         )

(define src-buf-len (* 44100 3))
(define simple-tone
  (make-tone 440 0.2 src-buf-len 44100))

(define channels 2)
(define s16-size 2)

(define data-ptr (s16vector->cpointer (rsound-data simple-tone)))
;; it comes out even on this many samples:
(define sine-wave-len 8820)
(check-= (rsound-ith/left simple-tone 4410) 0.0 0.01)
(check-= (rsound-ith/right simple-tone 4410) 0.0 0.01)

(define ts empty)
(define lens empty)

(define (simple-signal/block/s16 pointer t len)
  (define sample (* t channels))
  (define sample-len (* len channels))
  (define copy-source sample #;(modulo sample sine-wave-len))
  (if (< (* channels src-buf-len) (+ copy-source sample-len))
      (error "sample buffer only goes to ~s, needed ~s"
             src-buf-len (+ copy-source sample-len))
      (begin
        (memset pointer 0 (* s16-size sample-len))
        (s16buffer-add!/c pointer
                          (ptr-add data-ptr (* s16-size copy-source))
                          sample-len))))



(signal/block-play simple-signal/block/s16 44100)
(sleep 2)
(stop-playing)
