#lang racket


(require "../rsound.rkt"
         "../filter.rkt"
         rackunit
         rackunit/text-ui
         racket/flonum)

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
  
  
)))

(require "../draw.rkt")



#;(let ()
  (define coefficients (lpf-coefficients 0.34))
  (define gain (apply + (cons 1.0 coefficients)))
  (define poles-vec (apply flvector coefficients))
  (rsound-draw
   (msr 20
        (dynamic-lti-signal
         (lambda (t)
           (values (flvector)
                   poles-vec
                   gain))
         0 4
         (lambda (t) 0.5)))))
