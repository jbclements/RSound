#lang racket

(require rackunit
         plot
         "filter.rkt"
         "filter-typed.rkt")

(provide (except-out (all-defined-out)
                      twopi i))

(define i (sqrt -1))
(define twopi (* 2 pi))

;; draw a plot of the frequency response from min-freq to max-freq:
(define (response-plot poly min-freq max-freq)
  (plot (line (lambda (x) ((response/mag poly) x)))
        #:x-min min-freq
        #:x-max max-freq
        #:width 600))

 ; max-freq

;; plot the frequency response of a low-pass filter
(define (lpf-response scale min-freq max-freq)
  (define fun 
    (coefficient-sets->poly
     '(1 4 6 4 1)
     (lpf-coefficients scale)))
  (response-plot fun min-freq max-freq))






