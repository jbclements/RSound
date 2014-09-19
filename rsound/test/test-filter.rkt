#lang racket


(require "../rsound.rkt"
         "../util.rkt"
         "../filter.rkt"
         "../network.rkt"
         "../filter-typed.rkt"
         "../reverb.rkt"
         rackunit
         racket/flonum)

(provide the-test-suite)

(define i (sqrt -1))

  
(define test-sig (simple-ctr 0 0.002))
;; there could be a *lot* of good tests here...

(define the-test-suite
(test-suite 
 "filter tests"
 (let ()
   ;; fir-filter
   (check-exn (lambda (exn) (regexp-match #px"expects argument of type <exact integer delays greater than zero>"
                                          (exn-message exn)))
              (lambda () (fir-filter '((-1 34.0)))))
   
   (define filter (fir-filter '((0 0.3) (2 0.15))))
   (check-equal?
    (signal-samples (network () 
                      [a <= (simple-ctr 0.1 0.01)]
                      [b <= filter a])
                    3)
    
    (vector 0.03 0.033 (+ 0.036 0.015)))
   )
 
 (let ()
   ;; regression; check dynamic-lti vs fir-filter
   (define (simple-param-signal)
     (values (flvector 0.0 0.5)
             (flvector 0.0 0.0)
             0.3))
   
   
   (check-equal?
    (signal-samples (network ()
                      [a <= (simple-ctr 0.1 0.01)]
                      [(f i g) = (simple-param-signal)]
                      [out <= (dynamic-lti-signal 2) f i g a])
                    3)
    (signal-samples (network () 
                      [a <= (simple-ctr 0.1 0.01)]
                      [b <= (fir-filter '((0 0.3) (2 0.15))) a])
                    3))
   
   ;; test ordering of terms in dynamic-lti-filter
   
   (check-equal?
    (signal-samples 
     (network ()
       [a <= (simple-ctr 10 1)]
       [out <= (dynamic-lti-signal 4)
            (flvector 0.1 0.0 0.0 0.0)
            (flvector 0.0 0.0 0.0 0.5)
            1.0
            a])
     6)
    (vector 10.0 12.0 13.1 14.2 20.3 22.4))
   
   )
 (let ()

  (check-equal? (real-part/ck -0.9283+2.2938792e-17i) -0.9283)
  (check-equal? (real-part/ck -0.9283-2.2938792e-17i) -0.9283)
  (check-equal? (real-part/ck 0.9283+2.2938792e-17i) 0.9283)
  
  (define fir-terms (flvector 0.8 0.2))
  (define iir-terms (flvector 0.3 0.0))
  (define (param-maker)
    (values fir-terms
            iir-terms
            0.7))
  (define testnet
    (network ()
      [(f i g) = (param-maker)]
      [test-sigout <= test-sig]
      [out <= (dynamic-lti-signal 2)
           f i g test-sigout]))
  (let ()
    (define snd (signal->rsound 10 
                                testnet))
    (check-equal? (rs-ith/left snd 0) 0.0)
    (check-= (rs-ith/left snd 1) 0.0014 1e-5)
    
    (check-= (rs-ith/left snd 2) (+ (* 0.7 (/ 2 500))
                                    (* 0.7 (* 0.8 0.002)) 
                                    (* 0.3 0.0014))
             1e-5)
    3)
  
   (check-exn exn:fail?
              (lambda () (signal->rsound 
                          10 
                          (network ()
                            [a <= test-sig]
                            [(f i g) = (values (flvector 0.0) (flvector 0.0) 1.0)]
                            [out <= (dynamic-lti-signal 0) f i g a]))))
   (check-not-exn (lambda () (signal->rsound 
                              10 
                              (network ()
                                [a <= test-sig]
                                [(f i g) = (values (flvector 0.0) (flvector 0.0) 1.0)]
                                [out <= (dynamic-lti-signal 1) f i g a]))))
  (check-not-exn
   (lambda () (signal->rsound 
               20 
               (network ()
                 [a <= test-sig]
                 [control <= (dc-signal 0.1)]
                 [out <= lpf/dynamic control a]))))
  
  ;; should be the identity on a dc signal (after a while)
  (let ()
    (define zero-vec (flvector 0.0 0.0 0.0 0.0))
    (define tap-mult-vec (flvector -0.1 -0.2 -0.3 -0.4))
  (check-=
   (signal-nth
    (network ()
      [a <= (dc-signal 0.75)]
      [(f i g) = (values zero-vec tap-mult-vec 2.0)]
      [out <= (dynamic-lti-signal 4) f i g a])
    199)
   0.75
   1e-3))
  
  ;; same test with lpf:
  (check-= (signal-nth
            (network ()
              [a = 0.5]
              [control = 0.34]
              [out <= lpf/dynamic control a])
            199)
           0.5
           1e-3)
  
  
(check-= ((coefficients->poly '(3.0 1.0 9.0)) 6.0+0.0i) ; 3(6^2)+1(6)+9
         123
         1e-7)
              
(let ()
  (define poly (coefficient-sets->poly '(1 2) '(1 -4)))
  (check-= (poly 0) -1/2 1e-7))
   
  ;; OLD TESTS:
  
  ;; FIR-FILTER

(define (mush x) (/ (round (* x s16max)) s16max))

(let* ([my-filter (fir-filter '((0 1.0) (13 0.2) (5 0.1)))]
       [test-sound 
        (signal-samples
         (network ()
           [a <= (simple-ctr 0 1/500)]
           [out <= my-filter a])
         79)])
  (check-= (vector-ref test-sound 0) 0 1e-7)
  (check-= (vector-ref test-sound 1) 1/500 1e-7)
  (check-= (vector-ref test-sound 4) 4/500 1e-7)
  (check-= (vector-ref test-sound 5) (+ 5/500 0/5000) 1e-7)
  (check-= (vector-ref test-sound 6) (+ 6/500 1/5000) 1e-7)
  (check-= (vector-ref test-sound 12) (+ 12/500 7/5000) 1e-7)
  (check-= (vector-ref test-sound 78) (+ 78/500 73/5000 65/2500) 1e-7))

;; IIR-FILTER

(let* ([my-filter (iir-filter '((13 0.2) (5 0.1)))]
       [test-sound
        (signal-samples 
         (network ()
           [a <= (simple-ctr 0 1/500)]
           [out <= my-filter a])
         20)])
  (check-= (vector-ref test-sound 0) 0 1e-7)
  (check-= (vector-ref test-sound 1) 1/500 1e-7)
  (check-= (vector-ref test-sound 4) 4/500 1e-7)
  (check-= (vector-ref test-sound 5) (+ 5/500 0/5000) 1e-7)
  (check-= (vector-ref test-sound 6) (+ 6/500 1/5000) 1e-7)
  (check-= (vector-ref test-sound 10) (+ 10/500 5/5000) 1e-7)
  ;; now the IIR starts to behave differently:
  (check-= (vector-ref test-sound 11) (+ 11/500 (* 1/10 (+ 6/500 1/5000))) 1e-7)
  (check-= (vector-ref test-sound 12) (+ 12/500 (* 1/10 (+ 7/500 2/5000))) 1e-7))


  (define (complex-within a b tolerance)
  (< (magnitude (- a b)) tolerance))

(define (complex-check a b tolerance)
  (unless (complex-within a b tolerance)
    (error 'complex-check "expected ~a, got ~a, difference ~a is greater than tolerance ~a\n"
           b a (magnitude (- a b)) tolerance)))

(define (1-zero z)
  (+ 1 (expt z -1)))

(complex-check ((response/raw 1-zero) 0.0) 2.0 1e-4)
(complex-check ((response/raw 1-zero) 22050.0) 0.0 1e-4)


(check-equal? (all-but-n 3 '(1 2 3 4 5))
              '((4 5) (3 5) (3 4) (2 5) (2 4) 
                (2 3) (1 5) (1 4) (1 3) (1 2)))

(check-true (andmap = (roots->coefficients '(0.5+0.0i 0.5+i 0.5-i))
                     (list 1.0 -1.5 1.75 -0.625)))
  
(check-= (magnitude ((roots->poly (list 1.0+0.0i -1.0+0.0i)) 1.0+0.0i)) 0.0 1e-5)
(check-= (magnitude ((roots->poly (list 1.0+0.0i -1.0+0.0i)) -1.0+0.0i)) 0.0 1e-5)
(check-= (magnitude ((roots->poly (list 1.0+0.0i -1.0+0.0i)) 0.0+0.0i)) 1.0 1e-5)

(check-= (product-of '(1.0+0.0i 2.0+0.0i 3.0+0.0i 4.0+0.0i)) 24 0.0)
(check-= (sum-of '(1.0+0.0i 2.0+0.0i 3.0+0.0i 4.0+0.0i)) 10 0.0)

(check-= ((coefficients->poly '(3.0 1.0 9.0)) 6.0+0.0i) ; 3(6^2)+1(6)+9
         123
         1e-7)
              
(let ()
  (define poly (coefficient-sets->poly '(1 2) '(1 -4)))
  (check-= (poly 0) -1/2 1e-7))
   

  ;; filter?
  (let ()
    (check-true (filter? reverb))
    (check-true (filter? sine-wave))
    (check-true (filter? (lambda (x) x)))
    (check-false (filter? (lambda () 3))))
   
)))


(module+ test
  (require rackunit/text-ui)
  (run-tests the-test-suite))