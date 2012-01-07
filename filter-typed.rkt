#lang typed/racket

;; the part of filter.rkt that can easily be implemented using 
;; typed racket

(require/typed "rsound.rkt"
               [default-sample-rate (-> Real)])

(provide response/raw
         response/mag
         roots->poly
         coefficients->poly
         roots->coefficients
         real-part/ck
         lpf-coefficients)


(: i Inexact-Complex)
(define i (exact->inexact (sqrt -1)))
(define twopi (* 2 pi))

(define-type Poly (Inexact-Complex -> Inexact-Complex))
(define-type Z-Plane-Points (Listof Inexact-Complex))
(define-type Zeros Z-Plane-Points)
(define-type Poles Z-Plane-Points)
(define-type Coefficients (Listof Inexact-Real))
(define-type Frequency Positive-Inexact-Real)
(define-type Signal (Nonnegative-Fixnum -> Inexact-Real))

;; poly : a transfer function
;; coefficients : a list of coefficients to use in a transfer function
;; roots / zeros : places where the transfer function is zero
;; poles : places where the transfer function is infinite

;; frequency response, given input frequency in Hz
(: response/raw (Poly -> Frequency -> Inexact-Complex))
(define (response/raw poly)
  (define sr-inv (exact->inexact
                  (/ 1 (default-sample-rate))))
  (lambda (omega)
    (define z (exp (* i sr-inv twopi omega)))
    (poly z)))

;; struggling with the type here...
;; compute the magnitude of the response in decibels
(: response/mag (Poly -> Frequency -> Real))
(define ((response/mag poly) frequency)
  (define mag (exact->inexact
               (magnitude ((response/raw poly) frequency))))
  ;; log-based, cap at -100 db, square to get power
  #;(max -100 (* 10 (/ (log (expt mag 2)) (log 10))))
  ;; equivalent to:
  (* 10 (/ (ann (log (ann (max 1.0e-6 mag)
                          Positive-Inexact-Real))
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
  (for/fold: ([so-far : Inexact-Complex 0.0+0.0i])
    ([coefficient (in-list coefficients)])
    (+ coefficient (* so-far x))))

;; given a list of poles and a list of zeros in z-space, 
;; return the corresponding transfer function.
(: poles&zeros->poly (Poles Zeros -> Poly))
(define (poles&zeros->poly poles zeros)
  (coefficient-sets->poly (roots->coefficients poles)
                          (roots->coefficients zeros)))

;; given a list of feedback coefficients and a list of feedforward
;; coefficients, return the corresponding transfer function
(: coefficient-sets->poly (Coefficients Coefficients -> Poly))
(define (coefficient-sets->poly fb-coefficients ff-coefficients)
  (let ([feedback-poly (coefficients->poly fb-coefficients)]
        [feedforward-poly (coefficients->poly ff-coefficients)])
    (lambda (x)
      (/ (feedforward-poly x)
         (feedback-poly x)))))

;; given a set of poles or zeros, compute the corresponding
;; IIR feedback coefficients.
(: roots->coefficients (Z-Plane-Points -> Coefficients))
(define (roots->coefficients z-plane-points)
  (let ([neg-points (map - z-plane-points)])
  (reverse
   (for/list: ([exponent : Exact-Nonnegative-Integer
                         (ann
                          (in-range (ann (add1 (length neg-points))
                                         Nonnegative-Fixnum))
                          (Sequenceof Nonnegative-Fixnum))])
     (real-part/ck
      (sum-of (map product-of (all-but-n exponent neg-points))))))))


;; given a scale, produce a 4-pole chebyshev low-pass filter, returning
;; iir coefficients
(: lpf-coefficients (Inexact-Real -> Coefficients))
(define (lpf-coefficients scale)
  (define s-poles (map (lambda: ([x : Inexact-Complex])
                         (* scale x))
                       chebyshev-s-poles))
  (define z-poles (map s-space->z-space s-poles))
  (roots->coefficients z-poles))

;; constants in the 4-pole chebyshev low-pass filter:
(: chebyshev-s-poles Poles)
(define chebyshev-s-poles
  (let ()
    ;; how many poles (more poles is more computationally intensive)
    (define num-poles 4)
    ;; higher epsilon gives sharper drop but more ripple in passband
    (define epsilon 1.0)
    ;; the left half of the poles *in s-space*:
    (define left-half
      (for/list: : (Listof Inexact-Complex) 
        ([m (in-range num-poles)])
        (* i (cos (+ (* (/ 1 num-poles) 
                        (acos (/ i epsilon))) 
                     (/ (* pi m) num-poles))))))
    left-half))



;; fir-filter : (listof (list/c delay amplitude)) -> signal -> signal
;; filter the input signal using the delay values and amplitudes given for an FIR filter
(: fir-filter ((Listof (List Nonnegative-Fixnum Inexact-Real)) -> Signal -> Signal))
(define (fir-filter params)
  (match params
    [`((,delays ,amplitudes) ...)
     (lambda: ([signal : Signal])
       ;; enough to hold delayed and current, rounded up to next power of 2:
       (define max-delay
         (up-to-power-of-two (+ 1 (apply max delays))))
       ;; set up buffer to delay the signal
       (: delay-buf (Vectorof Inexact-Real))
       (define delay-buf (make-vector max-delay 0.0))
       (define next-idx 0)
       ;; ugh... we must be called sequentially:
       (define last-t -1)
       (: delays/t (Listof Nonnegative-Integer))
       (define delays/t 
         (cond [(andmap exact-nonnegative-integer? delays)
                delays]
               [(error 'impossible "Make TR happy")]))
       (: amplitudes/t (Listof Inexact-Real))
       (define amplitudes/t
         (cond [(andmap inexact-real? amplitudes)
                amplitudes]
               [(error 'impossible "Make TR happy")]))
       (lambda (t)
         (unless (= t (add1 last-t))
           (error 
            'fir-filter 
            "called with t=~s, expecting t=~s. Sorry about that limitation." 
                  t
                  (add1 last-t)))
         (let ([this-val (signal t)])
           (begin
             (vector-set! delay-buf next-idx this-val)
             (define result
               (for/fold:  
                ([sum : Inexact-Real 0.0])
                ([d (in-list delays/t)]
                 [a (in-list amplitudes/t)])
                (+ sum 
                   (* a 
                      (vector-ref
                       delay-buf
                       (modulo (- next-idx d) max-delay))))))
             (set! last-t (add1 last-t))
             (set! next-idx (modulo (add1 next-idx) max-delay))
             result))))]
    [other (raise-type-error 'fir-filter "(listof (list number number))" 0 params)]))




;; convert s-space value to z-space value
(: s-space->z-space (Inexact-Complex -> Inexact-Complex))
(define (s-space->z-space pole) (exp pole))


;; sum-of : (listof number) -> number
(: sum-of ((Listof Inexact-Complex) -> Inexact-Complex))
(define (sum-of l) (foldl + 0.0+0.0i l))

;; product-of : (listof number) -> number
(: product-of ((Listof Inexact-Complex) -> Inexact-Complex))
(define (product-of l) (foldl * 1.0+0.0i l))

;; all-but-n : ways of choosing all but 'n' elements of the list
(: all-but-n (All (T) (Natural (Listof T) -> (Listof (Listof T)))))
(define (all-but-n n l)
  (cond [(= n 0) (list l)]
        [(= n (length l)) (list '())]
        [else (define drop-this-one 
                (all-but-n (- n 1) (cdr l)))
              (define keep-this-one
                (map (lambda: ([x : (Listof T)])
                       (cons (car l) x))
                     (all-but-n n (cdr l))))
              (append drop-this-one keep-this-one)]))

;; given a number, check that it's close to real, return the
;; real number
(: real-part/ck (Inexact-Complex -> Inexact-Real))
(define (real-part/ck i)
  (define angl (angle i))
  (define wrapped-angle (cond [(< angl (- (/ pi 2))) (+ angl (* 2 pi))]
                              [else angl]))
  (cond [(< (abs wrapped-angle) angle-epsilon) (magnitude i)]
        [(< (abs (- pi wrapped-angle)) angle-epsilon)
         (- (magnitude i))]
        [else (error 'tidy-imag "angle ~s of complex number ~s is not close to zero or pi." wrapped-angle i)]))
(define angle-epsilon 1e-5)

;; pick the next largest (or equal) power of 2
(: up-to-power-of-two (Positive-Fixnum -> Exact-Positive-Integer))
(define (up-to-power-of-two n)
  (define log-2 (log 2))
  (define log-2-n (log (max n 1)))
  (cond [(<= log-2-n 0) (error 'up-to-power-of-two
                               "impossible, make TR happy.")]
        [(<= log-2 0) (error 'up-to-power-of-two
                            "impossible, make TR happy.")]
        [else
         (expt 2 (ceiling
                  (inexact->exact
                   (/ log-2-n log-2))))]))





(: my-signal Signal)
(define my-signal
  (lambda (x) (sin (exact->inexact
                    (* 2.0 pi (/ 1.0 44100.0) 400.0 x)))))

;; time test for FIR filter
(define sig2 ((fir-filter `((1 0.287) (4 -0.2987))) my-signal))

(: vec-len Nonnegative-Fixnum)
(define vec-len 1000)
(: m (Vectorof Inexact-Real))
(define m (make-vector vec-len 0.0))
(printf "filling vector with filtered signal")
(begin
  (for: ([i : (Sequenceof Nonnegative-Fixnum)
            (ann (in-range (ann vec-len Nonnegative-Fixnum))
                 (Sequenceof Nonnegative-Fixnum))])
        (vector-set! m i (sig2 i)))
  13)