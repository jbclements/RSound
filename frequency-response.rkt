#lang racket

(require rackunit
         plot
         "filter.rkt")

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




