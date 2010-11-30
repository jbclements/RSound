#lang racket

(require rackunit
         plot)

(provide (except-out (all-defined-out)
                      twopi i))

(define sample-rate/fr (make-parameter 44100))

(define i (sqrt -1))
(define twopi (* 2 pi))


;; frequency response, given input frequency in Hz
(define (response/raw poly)
  (define sr-inv (/ 1 (sample-rate/fr)))
  (lambda (omega)
    (let ([z (exp (* i sr-inv twopi omega))])
      (poly z))))

;; compute the magnitude of the response in decibels
(define (response/mag poly)
  (compose 
   (lambda (x) 
     ;; log-based, cap at -100 db, square to get power
     #;(max -100 (* 10 (/ (log (expt (magnitude x) 2)) (log 10))))
     ;; equivalent to:
     (* 10 (/ (log (max 1e-6 (magnitude x))) (/ (log 10) 2))))
   (response/raw poly)))

;; draw a plot of the frequency response from min-freq to max-freq:
(define (response-plot poly dbrel min-freq max-freq)
  (plot (line (lambda (x)
                (- ((response/mag poly) x) dbrel)))
        #:x-min min-freq
        #:x-max max-freq
        #:y-max 0
        #:y-min -100
        #:width 600))

 ; max-freq



;; given a set of poles, compute the corresponding
;; polynomial
(define (roots->poly iir-poles)
  (coefficients->poly (roots->coefficients iir-poles)))

;; given a set of poles, compute the corresponding
;; IIR feedback coefficients.
(define (roots->coefficients iir-poles)
  (let ([neg-poles (map - iir-poles)])
  (reverse
   (for/list ([exponent (in-range (add1 (length neg-poles)))])
     (sum-of (map product-of (all-but-n exponent neg-poles)))))))

;; sum-of : (listof number) -> number
(define (sum-of l) (foldl + 0 l))

;; product-of : (listof number) -> number
(define (product-of l) (foldl * 1 l))

;; all-but-n : ways of choosing all but 'n' elements of the list
(define (all-but-n n l)
  (cond [(= n 0) (list l)]
        [(= n (length l)) (list empty)]
        [else (append (all-but-n (- n 1) (rest l))
                      (map (lambda (x)
                             (cons (first l) x))
                           (all-but-n n (rest l))))]))

;; given a set of polynomial coefficients, return
;; the corresponding polynomial
(define ((coefficients->poly coefficients) x)
  (for/fold ([so-far 0])
    ([coefficient (in-list coefficients)])
    (+ coefficient (* so-far x))))



;; given a list of poles and a list of zeros in z-space, 
;; return the corresponding transfer function.
(define (poles&zeros->fun poles zeros)
  (coefficient-sets->fun (roots->coefficients poles)
                         (roots->coefficients zeros)))

;; given a list of feedback coefficients and a list of feedforward
;; coefficients, return the corresponding transfer function
(define (coefficient-sets->fun fb-coefficients ff-coefficients)
  (let ([feedback-poly (coefficients->poly fb-coefficients)]
        [feedforward-poly (coefficients->poly ff-coefficients)])
    (lambda (x)
      (/ (feedforward-poly x)
         (feedback-poly x)))))


