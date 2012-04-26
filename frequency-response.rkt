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
    (response/mag 
     (coefficients->poly 
      (cons 1.0 (map (lambda (x) (* x -1.0))
                     (lpf-coefficients 1.0))))))
  (plot (line (lambda (x) (fun x)))
        #:x-min min-freq
        #:x-max max-freq
        #:width 600))






