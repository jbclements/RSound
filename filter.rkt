#lang racket/base

(require (only-in racket pi)
         racket/match
         "rsound.rkt"
         racket/flonum)

(provide (except-out (all-defined-out)
                      twopi i))

(define i (sqrt -1))
(define twopi (* 2 pi))


;; frequency response, given input frequency in Hz
(define (response/raw poly)
  (define sr-inv (/ 1 (default-sample-rate)))
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

;; given a number, check that it's close to real, return the
;; real number
(define (tidy-imag i)
  (define angl (angle i))
  (define wrapped-angle (cond [(< angl (- (/ pi 2))) (+ angl (* 2 pi))]
                              [else angl]))
  (cond [(< (abs wrapped-angle) angle-epsilon) (magnitude i)]
        [(< (abs (- pi wrapped-angle)) angle-epsilon)
         (- (magnitude i))]
        [else (error 'tidy-imag "angle ~s of complex number ~s is not close to zero or pi." wrapped-angle i)]))
(define angle-epsilon 1e-5)


;; sum-of : (listof number) -> number
(define (sum-of l) (foldl + 0 l))

;; product-of : (listof number) -> number
(define (product-of l) (foldl * 1 l))

;; all-but-n : ways of choosing all but 'n' elements of the list
(define (all-but-n n l)
  (cond [(= n 0) (list l)]
        [(= n (length l)) (list '())]
        [else (append (all-but-n (- n 1) (cdr l))
                      (map (lambda (x)
                             (cons (car l) x))
                           (all-but-n n (cdr l))))]))

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

;; constants in the 4-pole chebyshev low-pass filter:
(define chebyshev-s-poles
  (let ()
    ;; how many poles (more poles is more computationally intensive)
    (define num-poles 4)
    ;; higher epsilon gives sharper drop but more ripple in passband
    (define epsilon 1.0)
    
    ;; the left half of the poles *in s-space*:
    (define left-half
      (for/list ([m (in-range num-poles)])
        (* i (cos (+ (* (/ 1 num-poles) 
                        (acos (/ i epsilon))) 
                     (/ (* pi m) num-poles))))))
    left-half))


;; convert s-space value to z-space value
(define (s-space->z-space pole) (exp pole))
  
;; given a scale, produce a 4-pole chebyshev low-pass filter
(define (lpf-coefficients scale)
  (define s-poles (map (lambda (x) (* scale x)) chebyshev-s-poles))
  (define z-poles (map s-space->z-space s-poles))
  (map tidy-imag (cdr (roots->coefficients z-poles))))




;; FIR filters

;; fir-filter : (listof (list/c delay amplitude)) -> signal -> signal
;; filter the input signal using the delay values and amplitudes given for an FIR filter
(define (fir-filter params)
  (match params
    [`((,delays ,amplitudes) ...)
     (unless (andmap (lambda (d) (and (exact-integer? d) (<= 0 d))) delays)
       (raise-type-error 'fir-filter "exact integer delays greater than zero" 0 params))
     (unless (andmap real? amplitudes)
       (raise-type-error 'fir-filter "real number amplitudes" 0 params))
     (lambda (signal)
       ;; enough to hold delayed and current, rounded up to next power of 2:
       (let* ([max-delay (up-to-power-of-two (+ 1 (apply max delays)))]
              ;; set up buffer to delay the signal
              [delay-buf (make-vector max-delay 0.0)]
              [next-idx 0]
              ;; ugh... we must be called sequentially:
              [last-t -1])
         (lambda (t)
           (unless (= t (add1 last-t))
             (error 'fir-filter "called with t=~s, expecting t=~s. Sorry about that limitation." 
                    t
                    (add1 last-t)))
           (let ([this-val (signal t)])
             (begin
               (vector-set! delay-buf next-idx this-val)
               (define result
                 (for/fold ([sum 0])
                   ([d (in-list delays)]
                    [a (in-list amplitudes)])
                   (+ sum (* a (vector-ref delay-buf (modulo (- next-idx d) max-delay))))))
               (set! last-t (add1 last-t))
               (set! next-idx (modulo (add1 next-idx) max-delay))
               result)))))]
    [other (raise-type-error 'fir-filter "(listof (list number number))" 0 params)]))

(define (up-to-power-of-two n)
  (inexact->exact (expt 2 (ceiling (/ (log (max n 1)) (log 2))))))



;; IIR filters

;; iir-filter : (listof (list/c delay amplitude)) -> signal -> signal
;; filter the input signal using the delay values and amplitudes given for an IIR filter
;; the only difference here is that we put the final result in the delay line, rather than
;; the input signal.
(define (iir-filter params)
  (match params
    [`((,delays ,amplitudes) ...)
     (unless (andmap (lambda (d) (and (exact-integer? d) (< 0 d))) delays)
       (raise-type-error 'iir-filter "exact integer delays greater than zero" 0 params))
     (unless (andmap real? amplitudes)
       (raise-type-error 'iir-filter "real number amplitudes" 0 params))
     (lambda (signal)
       (let* ([max-delay (up-to-power-of-two (+ 1 (apply max delays)))]
              ;; set up buffer to delay the signal
              [delay-buf (make-vector max-delay 0.0)]
              [next-idx 0]
              ;; ugh... we must be called sequentially:
              [last-t -1])
         (lambda (t)
           (unless (= t (add1 last-t))
             (error 'fir-filter "called with t=~s, expecting t=~s. Sorry about that limitation." 
                    t
                    (add1 last-t)))
           (let* ([this-val (signal t)]
                  [new-val (for/fold ([sum this-val])
                                     ([d (in-list delays)]
                                      [a (in-list amplitudes)])
                                     (+ sum (* a (vector-ref delay-buf (modulo (- next-idx d) max-delay)))))])
             (begin0
               new-val
               (vector-set! delay-buf next-idx new-val)
               (set! last-t (add1 last-t))
               (set! next-idx (modulo (add1 next-idx) max-delay)))))))]
    [other (raise-type-error 'iir-filter "(listof (list number number))" 0 params)]))

;; lti-filter : rsound (listof (list/c number? number?)) (listof (list/c number? number?)) -> rsound
;; given coefficients for an FIR and an IIR filter, apply
;; the given filter to the sound.
#;(define (lti-filter gain fir-coefficients iir-coefficients)
  (unless (real? gain)
    (raise-type-error 'lti-filter "real number" 0 gain fir-coefficients
                      iir-coefficients))
  (unless (and (list? fir-coefficients)
               (andmap (lambda (x) (and (list? x)
                                        (= (length x) 2)
                                        (nonnegative-integer? (car x))
                                        (real? (cadr x))))
                       fir-coefficients))
    (raise-type-error 'lti-filter "list of delays and coefficients" 1 
                      snd fir-coefficients iir-coefficients))
  (unless (and (list? iir-coefficients)
               (andmap (lambda (x) (and (list? x)
                                        (= (length x) 2)
                                        (nonnegative-integer? (first x))
                                        (real? (second x))))
                       iir-coefficients))
    (raise-type-error 'lti-filter "list of delays and coefficients" 2
                      snd fir-coefficients iir-coefficients))
  ;; must normalize, include gain...
  (define the-fir (fir-filter fir-coefficients))
  (define the-iir (iir-filter iir-coefficients))
  (signals->rsound (rsound-frames snd)
                   (the-iir (the-fir (rsound->signal/left snd)))
                   (the-iir (the-fir (rsound->signal/right snd)))))

;; we want to be able to change the filter dynamically...
(define (dynamic-lti-signal param-signal input-tap-len output-tap-len
                            input-signal)
  (define input-buf-len (max 1 input-tap-len))
  (define output-buf-len (max 1 output-tap-len))
  ;; enough to hold delayed and current, rounded up to next power of 2:
  (define saved-input-buf (make-flvector input-buf-len))
  (define saved-output-buf (make-flvector output-buf-len))
  (define next-idx 0)
  ;; ugh... we must be called sequentially:
  (define last-t -1)
  (lambda (t)
    (unless (= t (add1 last-t))
      (error 'fir-filter "called with t=~s, expecting t=~s. Sorry about that limitation." 
             t
             (add1 last-t)))
    (define-values (fir-terms iir-terms gain) (param-signal t))
    (unless (and (flvector? fir-terms)
                 (= (flvector-length fir-terms)
                    input-tap-len))
      (error 'dynamic-lti-signal 
             "expected vector of length ~s for fir-terms, got ~s"
             input-tap-len fir-terms))
    (unless (and (flvector? iir-terms)
                 (= (flvector-length iir-terms)
                    output-tap-len))
      (error 'dynamic-lti-signal 
             "expected vector of length ~s for iir-terms, got ~s"
             output-tap-len iir-terms))
    (define fir-sum
      (for/fold ([sum 0.0])
        ([i (in-range input-tap-len)])
        (fl+ sum
             (fl* (flvector-ref fir-terms i)
                  (flvector-ref saved-input-buf 
                                (modulo (- t i 1) input-buf-len))))))
    (define iir-sum
      (for/fold ([sum 0.0])
        ([i (in-range output-tap-len)])
        (fl+ sum
             (fl* (flvector-ref iir-terms i)
                  (flvector-ref saved-output-buf 
                                (modulo (- t i 1) output-buf-len))))))
    (define next-val (fl* gain (exact->inexact (input-signal t))))
    (flvector-set! saved-input-buf (modulo t input-buf-len) next-val)
    (define output-val (fl+ next-val (fl+ fir-sum iir-sum)))
    (flvector-set! saved-output-buf (modulo t output-buf-len) output-val)
    (set! last-t (add1 last-t))
    output-val))

(define max-scale-val 3.0)
(define min-scale-val 0.00)
(define perceptible-interval 0.01)
(define coefficient-table (make-vector (inexact->exact
                                        (floor 
                                         (/ (- max-scale-val
                                               min-scale-val) 
                                            perceptible-interval))) 
                                       #f))

(define (dynamic-lpf scale-signal input-signal)
  (dynamic-lti-signal
   (lambda (t)
     (define scale (scale-signal t))
     (when (not (<= min-scale-val scale max-scale-val))
       (error 'dynamic-lpf "scale value ~s not between ~s and ~s"
              scale
              min-scale-val
              max-scale-val))
     (define table-index (inexact->exact
                          (round
                           (/ (- scale min-scale-val)
                              perceptible-interval))))
     (define tap-mults
       (match (vector-ref coefficient-table table-index)
         [#f (define coefficients (lpf-coefficients scale))
             (define new-table-entry 
               (apply flvector 
                      (map (lambda (x) (* x -1.0))
                           coefficients)))
             (vector-set! coefficient-table table-index new-table-entry)
             new-table-entry]
         [other other]))
     (define gain (+ 1.0 (fl- 0.0 (flvector-sum tap-mults))))
     (values (flvector)
             tap-mults
             gain))
   0 4
   input-signal))

(define (flvector-sum vec)
  (for/fold ([sum 0.0]) ([f (in-flvector vec)]) (fl+ sum f)))

;; it looks like 1/100 is close enough not to notice. This
;; is totally a guess on my part


