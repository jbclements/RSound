#lang racket/base

(require rackunit
         plot
         "filter.rkt"
         "filter-typed.rkt"
         racket/flonum
         (only-in racket/math pi)
         (only-in racket/match match match-define)
         racket/contract)

(provide (contract-out [response-plot (->* ((-> number? number?) number? number?)
                                           (#:db boolean?)
                                           any)]
                       [lpf-response-plot (->* (number? number? number?)
                                               (#:db boolean?)
                                               any)]))

(define i (sqrt -1))
(define e (exp 1.0))
(define twopi (* 2 pi))

;; draw a plot of the frequency response from min-freq to max-freq:
(define (response-plot poly min-freq max-freq #:db [db? #t])
  (plot (line (response/mag poly db?))
        #:x-min min-freq
        #:x-max max-freq
        #:y-min MIN-DB
        #:width 600))

 ; max-freq

;; plot the frequency response of a low-pass filter
;; frequencies in Hz, assuming 44.1K sample rate. 
;; min-freq and max-freq just control what portion of the
;; frequency domain is displayed. The #:db flag controls whether
;; the result is shown in decibels
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

(define cutoff-theta (* pi (/ 3600 22050)))

(lpf-response-plot cutoff-theta 0 22050 #:db #t)

;; compensate for gain at 0 Hz by adding a multiplier
(define (zero-normalize fun)
  (define gain (fun 1.0))
  (lambda (z) (/ (fun z) gain)))


(response-plot (zero-normalize (lambda (z) (/ 1 (- z 0.9)))) 0 22050)
(response-plot (zero-normalize (lambda (z) (/ (+ z 1) (- z 0.9)))) 0 22050)
(define r1 (* 0.90 (exp (* i 6/16 pi))))
(define r2 (* 0.90 (exp (* -1 i 6/16 pi))))
(define r3 (exp (* i 9/16 pi)))
(define r4 (exp (* -1 i 9/16 pi)))
(response-plot (zero-normalize (lambda (z) (/ (* (+ z 1)) (* (- z 0.95))))) 0 22050)