#lang racket

(require rackunit
         plot
         "filter.rkt"
         "filter-typed.rkt"
         racket/flonum)

(provide (contract-out [response-plot (->* ((-> number? number?) number? number?)
                                           (#:db boolean?)
                                           any)]
                       [lpf-response-plot (->* (number? number? number?)
                                               (#:db boolean?)
                                               any)]))

(define i (sqrt -1))
(define twopi (* 2 pi))

;; draw a plot of the frequency response from min-freq to max-freq:
(define (response-plot poly min-freq max-freq #:db [db? #t])
  (plot (line (lambda (x) ((response/mag poly db?) x)))
        #:x-min min-freq
        #:x-max max-freq
        #:width 600))

 ; max-freq

;; plot the frequency response of a low-pass filter
(define (lpf-response-plot scale min-freq max-freq #:db [db? #t])
  (match-define (vector fir-taps iir-taps gain) (lpf-tap-vectors scale))
  (define fir-terms (map (lambda (x) (* x gain)) (cons 1.0 (flvector->list fir-taps))))
  (define iir-terms (cons 1.0 (map (lambda (x) (* -1.0 x)) (flvector->list iir-taps))))
  (define fun 
    (coefficient-sets->poly
     fir-terms
     iir-terms))
  #;(define fun 
    (coefficient-sets->poly
     '(1 4 6 4 1)
     (lpf-coefficients scale)))
  (response-plot fun min-freq max-freq #:db db?))

(define (flvector->list f)
  (for/list ([v (in-flvector f)]) v))





