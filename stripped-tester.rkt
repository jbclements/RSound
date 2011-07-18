#lang racket

;; trying to diagnose a core dump on linux; this doesn't
;; use any of the DrRacket stuff.

(require ffi/unsafe
         ffi/vector
         "private/portaudio.rkt"
         "rsound.rkt"
         "util.rkt")

(define short-tone
  (make-tone 440 0.5 8000 44100))

;; try initializing a whole bunch...
(for ([i (in-range 1)])
  (rsound-play short-tone)
  (sleep 1 ))
(printf "well, we're still here...\n")
(sleep 5)





#|

(define channels 2)

(define twopi (* 2 pi))

(define max-s16 #x7fff)

(define (sine-wave pitch sample-rate volume)
  (let ([scalar (* twopi pitch)])
    (lambda (i)
      (let ([t (/ i sample-rate)])
        (* max-s16 (* volume (sin (* scalar t))))))))

(define (make-block frames f)
  (let* ([cblock (make-s16vector (* 4 channels frames))])
    (for ([i (in-range frames)])
      (let* ([offset (* 2 i)]
             [sample (inexact->exact (round (f i)))])
        (s16vector-set! cblock offset        sample)
        (s16vector-set! cblock (add1 offset) sample)))
    cblock))

(define the-block (make-block 44100 (sine-wave 440 44100 0.25)))

(play-buffer the-block 44100 44100)

|#