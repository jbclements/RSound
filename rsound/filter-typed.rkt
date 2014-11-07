#lang typed/racket/base

;; the part of filter.rkt that can easily be implemented using 
;; typed racket

(require racket/flonum
         racket/match)

(require/typed "common.rkt"
               [default-sample-rate (-> Real)])

(require/typed racket/math 
               [pi Real])

(provide response/raw
         response/mag
         roots->poly
         coefficients->poly
         poles&zeros->poly
         coefficient-sets->poly
         roots->coefficients
         chebyshev-s-poles
         chebyshev-z-poles
         s-space->z-space
         real-part/ck
         lpf-coefficients
         lpf-tap-vectors
         up-to-power-of-two
         all-but-n
         product-of
         sum-of
         num-poles
         zeros-at-negative-one
         MIN-DB)

;; mag->db uses this as the floor:
(: MIN-DB Inexact-Real)
(define MIN-DB -120.0)

(: i Complex)
(define i (exact->inexact (sqrt -1)))
(define twopi (* 2 pi))

(define-type Poly (Complex -> Complex))
(define-type Z-Plane-Points (Listof Complex))
(define-type Zeros Z-Plane-Points)
(define-type Poles Z-Plane-Points)
(define-type Coefficients (Listof Real))
(define-type Frequency Nonnegative-Real)
(define-type Signal (-> Real))
(define-type Network1 (Flonum -> Flonum))

;; poly : a transfer function
;; coefficients : a list of coefficients to use in a transfer function
;; roots / zeros : places where the transfer function is zero
;; poles : places where the transfer function is infinite

;; frequency response, given input frequency in Hz
(: response/raw (Poly -> Frequency -> Complex))
(define (response/raw poly)
  (define sr-inv (exact->inexact
                  (/ 1 (default-sample-rate))))
  (lambda (omega)
    (define z (exp (* i sr-inv twopi omega)))
    (poly z)))

;; struggling with the type here...
;; compute the magnitude of the response in decibels (or just amplitude)
(: response/mag (Poly Boolean -> Frequency -> Real))
(define ((response/mag poly db?) frequency)
  (define mag (exact->inexact
               (magnitude ((response/raw poly) frequency))))
  ;; equivalent to:
  ;; FIXME: ensure-positive not required in later versions...
  (cond [db? (mag->db mag)]
        [else mag]))

;; a constant computed from MIN-DB to ensure that mag->db has a 
;; floor.
(: mag-floor Positive-Inexact-Real)
(define mag-floor 
  (let ()
    ;; jump through hoops to make through it's not zero:
    (define mag-floor (exact->inexact (expt 10 (/ MIN-DB 20))))
    (cond [(= 0.0 mag-floor) 
           (error 'mag-floor "mag-floor came out to be zero")]
          [else mag-floor])))

;; convert a magnitude into decibels. Put the floor at MIN-DB.
(: mag->db (Nonnegative-Inexact-Real -> Real))
(define (mag->db mag)
  (* 20 (/ (log (max mag-floor mag))
           (log 10))))

;; given a set of zeros, compute the corresponding
;; polynomial
(: roots->poly (Zeros -> Poly))
(define (roots->poly roots)
  (coefficients->poly (roots->coefficients roots)))

;; given a set of polynomial coefficients, return
;; the corresponding polynomial
(: coefficients->poly (Coefficients -> Poly))
(define ((coefficients->poly coefficients) x)
  (for/fold: ([so-far : Complex 0.0+0.0i])
    ([coefficient (in-list coefficients)])
    (+ coefficient (* so-far x))))

;; given a list of poles and a list of zeros in z-space, 
;; return the corresponding transfer function.
(: poles&zeros->poly (Poles Zeros -> Poly))
(define (poles&zeros->poly poles zeros)
  (coefficient-sets->poly (roots->coefficients zeros)
                          (roots->coefficients poles)))

;; given a list of feed-forward coefficients and a list of feedback
;; coefficients, return the corresponding transfer function
;; accepts the coefficients of the transfer function; don't clip
;; off the first one or negate the latter coefficients of the feedback terms.
(: coefficient-sets->poly (Coefficients Coefficients -> Poly))
(define (coefficient-sets->poly ff-coefficients fb-coefficients)
  (let ([feedback-poly (coefficients->poly fb-coefficients)]
        [feedforward-poly (coefficients->poly ff-coefficients)])
    (lambda (x)
      (/ (feedforward-poly x)
         (feedback-poly x)))))

;; given a set of poles or zeros, compute the corresponding
;; IIR feedback coefficients.
(: roots->coefficients (Z-Plane-Points -> (Listof Flonum)))
(define (roots->coefficients z-plane-points)
  (let ([neg-points (map - z-plane-points)])
  (reverse
   (for/list: ([exponent : Exact-Nonnegative-Integer
                         (ann
                          (in-range 0
                                    (add1 (length neg-points)))
                          (Sequenceof Nonnegative-Fixnum))])
     (real-part/ck
      (sum-of (map product-of (all-but-n exponent neg-points))))))))


;; given a cutoff in radians, produce a 4-pole chebyshev low-pass filter, returning
;; iir coefficient
(: lpf-coefficients (Real -> (Listof Flonum)))
(define (lpf-coefficients scale)
  (roots->coefficients (chebyshev-z-poles scale)))

;; given a cutoff in radians, produce the poles in z-space of a 4-pole chebyshev
;; low-pass filter. The 'scale' is a cutoff frequency, in radians.
(: chebyshev-z-poles (Real -> (Listof Complex)))
(define (chebyshev-z-poles scale)
  (define s-poles (map (lambda: ([x : Complex])
                         (* scale x))
                       chebyshev-s-poles))
  (map s-space->z-space s-poles))

;; given an angle in radians representing frequency, 
;; produce a vector containing fir and iir tap mults
;; and gain
(: lpf-tap-vectors (Real -> (Vector FlVector FlVector Real)))
(define (lpf-tap-vectors theta)
  (define coefficients (lpf-coefficients theta))
  (define gain (/ (apply + coefficients)
                  zeros-at-negative-one/sum))
  (define tap-multipliers 
    (apply flvector 
           (map (lambda: ([x : Flonum]) (* x -1.0))
                (cdr coefficients))))
  (vector zeros-at-negative-one/flvec tap-multipliers gain))

;; how many poles (more poles is more computationally intensive)
(define num-poles 4)

;; without the zeros at -1.
(define no-zeros (list 1.0 0.0 0.0 0.0 0.0))
(define no-zeros/sum 1.0)
(define no-zeros/flvec (apply flvector (cdr no-zeros)))
;; default-fir coefficients
;; this puts four zeros at -1 in the Z-plane => nyquist freq.
(define zeros-at-negative-one (list 1.0 4.0 6.0 4.0 1.0))
(define zeros-at-negative-one/sum (apply + zeros-at-negative-one))
(define zeros-at-negative-one/flvec (apply flvector (cdr zeros-at-negative-one)))

;; constants in the 4-pole chebyshev low-pass filter:
(: chebyshev-s-poles Poles)
(define chebyshev-s-poles
  (let ()
    ;; higher epsilon gives sharper drop but more ripple in passband
    (define epsilon 0.5)
    ;; the left half of the poles *in s-space*:
    (define left-half
      (for/list: : (Listof Complex) 
        ([m (in-range num-poles)])
        (* i (cos (+ (* (/ 1 num-poles) 
                        (acos (/ i epsilon))) 
                     (/ (* pi m) num-poles))))))
    left-half))



(define-predicate float? Float)

;; fir-filter : (listof (list/c delay amplitude)) -> Network
;; filter the input signal using the delay values and amplitudes given for an FIR filter
(: fir-filter ((Listof (List Index Real)) -> Network1))
(define (fir-filter params)
  (match params
    [`((,#{delays : (Listof Nonnegative-Fixnum)} ,amplitudes) ...)
     ;; enough to hold delayed and current, rounded up to next power of 2:
     (: max-delay Index)
     (define max-delay
       (up-to-power-of-two (+ 1 (apply max delays))))
     (define wraparound-limit (- max-delay 1))
     (: wraparound (Index -> Index))
     ;; this one only counts up by one, wraps down:
     (define (wraparound idx)
       (cond [(<= wraparound-limit idx) 0]
             [else (ensure-index (add1 idx))]))
     ;; set up buffer to delay the signal
     (: delay-buf (Vectorof Float))
     (define delay-buf (make-vector max-delay 0.0))
     (: next-idx Index)
     (define next-idx 0)
     (: delays/t (Listof Nonnegative-Integer))
     (define delays/t 
       (cond [(andmap exact-nonnegative-integer? delays)
              delays]
             [(error 'impossible "Make TR happy")]))
     (define amplitudes/real
       (cond [(andmap real? amplitudes)
              amplitudes]
             [(error 'impossible "Make TR happy")]))
     (: amplitudes/t (Listof Float))
     (define amplitudes/t 
       (map (ann real->double-flonum (Real -> Flonum)) amplitudes/real))
     
     (lambda: ([this-val : Float])
       (vector-set! delay-buf next-idx this-val)
       (define result
         (for/fold:  
             ([sum : Float 0.0])
           ([d (in-list delays/t)]
            [a (in-list amplitudes/t)])
           (define offset-idx (- next-idx d))
           (define wrapped
             (cond [(< offset-idx 0) 
                    (ensure-index (+ offset-idx max-delay))]
                   [(<= max-delay offset-idx) 
                    (ensure-index (- offset-idx max-delay))]
                   [else offset-idx]))
           (+ sum 
              (* a (vector-ref delay-buf wrapped)))))
       (set! next-idx (wraparound next-idx))
       result)]
    [other (raise-type-error 'fir-filter "(listof (list number number))" 0 params)]))


;; check that a value is an Index, signal an error otherwise
(: ensure-index (Integer -> Index))
(define (ensure-index n)
  (cond [(index? n) n]
        [else (raise-type-error 'ensure-index "Index" 0 n)]))


;; convert s-space value to z-space value
;; yes! Bilinear transform seems to work.
(: s-space->z-space (Complex -> Complex))
(define (s-space->z-space pole) 
  (/ (+ 1.0 (/ pole 2.0))
     (- 1.0 (/ pole 2.0))))


;; sum-of : (listof number) -> number
(: sum-of ((Listof Complex) -> Complex))
(define (sum-of l) (foldl + 0.0+0.0i l))

;; product-of : (listof number) -> number
(: product-of ((Listof Complex) -> Complex))
(define (product-of l) (foldl * 1.0+0.0i l))

;; all-but-n : ways of choosing all but 'n' elements of the list
(: all-but-n (All (T) (Natural (Listof T) -> (Listof (Listof T)))))
(define (all-but-n n-in l)
  (define n (ann n-in Integer))
  (cond [(<= n 0) (list l)]
        [(= n (length l)) (list '())]
        [else 
         (define drop-this-one 
           (all-but-n (- n 1 ) (cdr l)))
         (define keep-this-one
           (map (lambda: ([x : (Listof T)])
                  (cons (car l) x))
                (all-but-n n (cdr l))))
         (append drop-this-one keep-this-one)]))

(: foo (Integer -> Natural))
(define (foo n)
  (cond [(<= n 0) 13]
        [else
         (ann (- n 1) Natural)]))

;; given a number, check that it's close to real, return the
;; real number
(: real-part/ck (Complex -> Flonum))
(define (real-part/ck i)
  (define angl (angle i))
  (define wrapped-angle (cond [(< angl (- (/ pi 2))) (+ angl (* 2 pi))]
                              [else angl]))
  (cond [(< (abs wrapped-angle) angle-epsilon) 
         (real->double-flonum (magnitude i))]
        [(< (abs (- pi wrapped-angle)) angle-epsilon)
         (- (real->double-flonum (magnitude i)))]
        [else (error 'real-part/ck "angle ~s of complex number ~s is not close to zero or pi." wrapped-angle i)]))
(define angle-epsilon 1e-5)

;; pick the next largest (or equal) power of 2
(: up-to-power-of-two (Positive-Integer -> Index))
(define (up-to-power-of-two n)
  (define log-2 (log 2))
  (define log-2-n (log (max n 1)))
  (cond [(<= log-2-n 0) (error 'up-to-power-of-two
                               "impossible, make TR happy.")]
        [(<= log-2 0) (error 'up-to-power-of-two
                             "impossible, make TR happy.")]
        [else
         (ensure-index
          (expt 2 (ceiling
                   (inexact->exact
                    (/ log-2-n log-2)))))]))





;(: my-signal Signal)
;(define my-signal
;  (lambda (x) (sin (exact->inexact
;                    (* 2.0 pi (/ 1.0 44100.0) 400.0 x)))))


;; time test for FIR filter

#|(: sig2 Signal)
(define sig2 ((fir-filter `((1 0.287) (4 -0.2987))) my-signal))

(: vec-len Nonnegative-Fixnum)
(define vec-len 1000)
(: m (Vectorof Inexact-Real))
(define m (make-vector vec-len 0.0))
(printf "filling vector with filtered signal")
(begin
  (for: ([i : Nonnegative-Fixnum
            (ann (in-range (ann vec-len Nonnegative-Fixnum))
                 (Sequenceof Nonnegative-Fixnum))])
        (vector-set! m i (sig2 i)))
  13)|#


;;filter-typed.rkt:68:14: Type Checker: Expected Positive-Inexact-Real, but got (U Positive-Inexact-Real Inexact-Real-Nan) in: n