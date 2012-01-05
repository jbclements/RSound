#lang typed/racket

;; the part of filter.rkt that can easily be implemented using 
;; typed racket

(require/typed "rsound.rkt"
               [default-sample-rate (-> Real)])


(define i (sqrt -1))
(define twopi (* 2 pi))

(define-type Poly (Complex -> Complex))
(define-type Z-Plane-Points (Listof Complex))
(define-type Zeros Z-Plane-Points)
(define-type Poles Z-Plane-Points)
(define-type Coefficients (Listof Real))
(define-type Frequency Real)

;; poly : a transfer function
;; coefficients : a list of coefficients to use in a transfer function
;; roots / zeros : places where the transfer function is zero
;; poles : places where the transfer function is infinite

;; frequency response, given input frequency in Hz
(: response/raw (Poly -> Frequency -> Complex))
(define (response/raw poly)
  (define sr-inv (/ 1 (default-sample-rate)))
  (lambda (omega)
    (let ([z (exp (* i sr-inv twopi omega))])
      (poly z))))

;; struggling with the type here...
;; compute the magnitude of the response in decibels
(: response/mag (Poly -> Frequency -> Real))
(define ((response/mag poly) frequency)
  (define mag (magnitude ((response/raw poly) frequency)))
  ;; log-based, cap at -100 db, square to get power
  #;(max -100 (* 10 (/ (log (expt mag 2)) (log 10))))
  ;; equivalent to:
  (* 10 (/ (ann (log (ann (max 1e-6 mag)
                          Positive-Real))
                Real)
           (/ (log 10) 2))))

;; given a set of zeros, compute the corresponding
;; polynomial
(: roots->poly (Zeros -> Poly))
(define (roots->poly roots)
  (coefficients->poly (roots->coefficients roots)))

;; given a set of polynomial coefficients, return
;; the corresponding polynomial
(: coefficients->poly (Coefficients -> Poly))
(define ((coefficients->poly coefficients) x)
  (for/fold ([so-far 0])
    ([coefficient (in-list coefficients)])
    (+ coefficient (* so-far x))))

;; given a set of poles or zeros, compute the corresponding
;; IIR feedback coefficients.
(: roots->coefficients (Z-Plane-Points -> Coefficients))
(define (roots->coefficients z-plane-points)
  (let ([neg-points (map - z-plane-points)])
  (reverse
   (for/list ([exponent (in-range (add1 (length neg-points)))])
     (sum-of (map product-of (all-but-n exponent neg-points)))))))


;; sum-of : (listof number) -> number
(: sum-of ((Listof Complex) -> Complex))
(define (sum-of l) (foldl + 0 l))

;; product-of : (listof number) -> number
(: product-of ((Listof Complex) -> Complex))
(define (product-of l) (foldl * 1 l))

;; all-but-n : ways of choosing all but 'n' elements of the list
(: all-but-n (All (T) (Natural (Listof T) -> (Listof (Listof T)))))
(define (all-but-n n l)
  (cond [(= n 0) (list l)]
        [(= n (length l)) (list '())]
        [else (define drop-this-one 
                (all-but-n (- n 1) (cdr l)))
              (define keep-this-one
                (map (lambda (x)
                             (cons (car l) x))
                           (all-but-n n (cdr l))))
              (append drop-this-one keep-this-one)]))

