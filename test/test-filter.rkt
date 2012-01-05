#lang racket


(require "../rsound.rkt"
         "../filter.rkt"
         rackunit
         rackunit/text-ui
         racket/flonum)

(define i (sqrt -1))
(define msr mono-signal->rsound)
;; there could be a *lot* of good tests here...

(run-tests
(test-suite "filter tests"
(let ()

  (check-equal? (tidy-imag -0.9283+2.2938792e-17i) -0.9283)
  (check-equal? (tidy-imag -0.9283-2.2938792e-17i) -0.9283)
  (check-equal? (tidy-imag 0.9283+2.2938792e-17i) 0.9283)
  
  
  (define (test-sig t) (/ t 500))
  
  (define fir-terms (flvector 0.8 0.2))
  (define iir-terms (flvector 0.3))
  (define (param-maker t)
    (values fir-terms
            iir-terms
            0.7))
  (let ()
    (define snd (msr 10 (dynamic-lti-signal param-maker 2 1 test-sig)))
    (check-equal? (rs-ith/left snd 0) 0.0)
    (check-= (rs-ith/left snd 1) 0.0014 1e-5)
    
    (check-= (rs-ith/left snd 2) (+ (* 0.7 (/ 2 500))
                                    (* 0.7 (* 0.8 0.002)) 
                                    (* 0.3 0.0014))
             1e-5)
    3)
  
  (check-not-exn (lambda () (msr 10 (dynamic-lti-signal 
                                     (lambda (t)
                                       (values (flvector) (flvector) 1.0))
                                     0 0 test-sig))))
  (check-not-exn
   (lambda () (msr 20 (lpf/dynamic (lambda (x) 0.1) test-sig))))
  
  ;; should be the identity on a dc signal (after a while)
  (let ()
  (define tap-mult-vec (flvector -0.1 -0.2 -0.3 -0.4))
  (check-=
   (rs-ith/right
    (msr 200
         (dynamic-lti-signal
          (lambda (t)
            (values (flvector)
                    tap-mult-vec
                    2.0))
          0 4
          (lambda (t) 0.75)))
    199)
   0.75
   1e-3))
  
  ;; same test with lpf:
  (check-= (rs-ith/left
            (msr 200 (lpf/dynamic (lambda (x) 0.34) (lambda (x) 0.5)))
            199)
           0.5
           1e-3)
  
  
  ;; regression testing:
  (check-= (rs-ith/left
            (msr 20 (lpf/dynamic (lambda (x) 0.1) (lambda (t)
                                                    (/ t 20))))
            19)
           0.0218
           1e-4)
  

  ;; OLD TESTS:
  
  ;; FIR-FILTER

(define (mush x) (/ (round (* x s16max)) s16max))

(let* ([my-filter (fir-filter '((0 1.0) (13 0.2) (5 0.1)))]
       [test-sound (mono-signal->rsound 100 (my-filter (lambda (x) (/ x 500))))])
  (check-= (rs-ith/right test-sound 0) 0 1e-7)
  (check-= (rs-ith/right test-sound 1) (mush 1/500) 1e-7)
  (check-= (rs-ith/right test-sound 4) (mush 4/500) 1e-7)
  (check-= (rs-ith/right test-sound 5) (mush (+ 5/500 0/5000)) 1e-7)
  (check-= (rs-ith/right test-sound 6) (mush (+ 6/500 1/5000)) 1e-7)
  (check-= (rs-ith/right test-sound 12) (mush (+ 12/500 7/5000)) 1e-7)
  (check-= (rs-ith/left test-sound 78) (mush (+ 78/500 73/5000 65/2500)) 1e-7))

;; IIR-FILTER

(let* ([my-filter (iir-filter '((13 0.2) (5 0.1)))]
       [test-sound (mono-signal->rsound 100 (my-filter (lambda (x) (/ x 500))))])
  (check-= (rs-ith/right test-sound 0) 0 1e-7)
  (check-= (rs-ith/right test-sound 1) (mush 1/500) 1e-7)
  (check-= (rs-ith/right test-sound 4) (mush 4/500) 1e-7)
  (check-= (rs-ith/right test-sound 5) (mush (+ 5/500 0/5000)) 1e-7)
  (check-= (rs-ith/right test-sound 6) (mush (+ 6/500 1/5000)) 1e-7)
  (check-= (rs-ith/right test-sound 10) (mush (+ 10/500 5/5000)) 1e-7)
  ;; now the IIR starts to behave differently:
    (check-= (rs-ith/right test-sound 11) (mush (+ 11/500 (* 1/10 (+ 6/500 1/5000)))) 1e-7)
  (check-= (rs-ith/right test-sound 12) (mush (+ 12/500 (* 1/10 (+ 7/500 2/5000)))) 1e-7))


  (define (complex-within a b tolerance)
  (< (magnitude (- a b)) tolerance))

(define (complex-check a b tolerance)
  (unless (complex-within a b tolerance)
    (error 'complex-check "expected ~a, got ~a, difference ~a is greater than tolerance ~a\n"
           b a (magnitude (- a b)) tolerance)))

(define (1-zero z)
  (+ 1 (expt z -1)))

(complex-check ((response/raw 1-zero) 0) 2.0 1e-4)
(complex-check ((response/raw 1-zero) 22050) 0.0 1e-4)


(check-equal? (all-but-n 3 '(1 2 3 4 5))
              '((4 5) (3 5) (3 4) (2 5) (2 4) 
                (2 3) (1 5) (1 4) (1 3) (1 2)))

(check-true (andmap = (roots->coefficients '(0.5 0.5+i 0.5-i))
                     (list 1.0 -1.5 1.75 -0.625)))
  
(check-= (magnitude ((roots->poly (list 1.0 -1.0)) 1.0)) 0.0 1e-5)
(check-= (magnitude ((roots->poly (list 1.0 -1.0)) -1.0)) 0.0 1e-5)
(check-= (magnitude ((roots->poly (list 1.0 -1.0)) i)) 2.0 1e-5)  

(check-= (product-of '(1 2 3 4)) 24 0.0)
(check-= (sum-of '(1 2 3 4)) 10 0.0)

(check-= ((coefficients->poly '(3 1 9)) 6)
         123
         1e-7)
              


)))


