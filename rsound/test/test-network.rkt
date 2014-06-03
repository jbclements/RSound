#lang racket

(require "../network.rkt"
         rackunit)

(provide the-test-suite)

;; REWRITING TO GET RID OF SIGNAL STRUCTURE ... ooh, no, init still not working.

;; angles are expressed in 44100ths of a circle....

(define (wraparound angle)
  (cond [(< angle 0) (+ angle 44100)]
        [(< 44099 angle) (- angle 44100)]
        [else angle]))

(define (cyclic-angle incr angle)
  (wraparound (+ angle incr)))

(define srinv (/ 1.0 44100.0))
(define (lookup angle)
  ;; could just be a lookup....
  (sin (* 2.0 pi srinv angle)))

(define variable-pitch-oscillator
  (network (incr)
           [angle = (cyclic-angle incr (prev angle 0.0))]
           [wave = (lookup angle)]))

(define mynet
  (network ()
           [wave <= variable-pitch-oscillator 440]
           [gain =  (* wave 0.1)]))

(define the-test-suite
  (test-suite
   "network tests"
   (let ()
     (define started (network-init mynet))
     (check-equal? (started) (* 0.1 (sin (* 440 1/44100 2 pi))))
     (check-equal? (started) (* 0.1 (sin (* 440 2/44100 2 pi))))
     
     
     ;; test case that exposes an old bug:
     (define testnet
       (network ()
                [offby1 = (+ 10 (prev offby1 0))]
                [out = ((lambda (i) i) (sub1 offby1))]))
     
     (define a (network-init testnet))
     
     (check-equal? (a) 9)
     (check-equal? (a) 19)
     
     )
   
   (check-equal? 
    (signal-samples (network ()
                             (out <= (simple-ctr 10 -1)))
                    3)
    (vector 10 9 8))
   
   (let ()
     (define testnet 
       (network ()
                (a1 <= (simple-ctr 1 1))
                (b1 <= (simple-ctr 4 3))
                (c1 <= (simple-ctr 10 -1))
                (a2 = (+ (* 2 a1) b1))
                (b2 = (+ a1 c1))
                (a1 = (- a2 b2))))
     (check-equal? (signal-samples testnet 3)
                   (vector -5 0 5)))
   
   (let ()
     (define (dual-counter idx)
       (values idx (+ idx 1)))
     (define testnet 
       (network ()
                [base-ctr <= (simple-ctr 1 3)]
                [(a b) = (dual-counter base-ctr)]
                [c <= (simple-ctr 10 -1)]
                [out = (- (* a c) b)]))
     (check-equal? (signal-samples testnet 3)
                   (vector 8 31 48)))
   
   (let ()
     ;; multi-init
     (define (addall a b c)
       (values (+ 1 a) (+ 2 b) (+ 3 c)))
     (define testnet
       (network ()
                [(a b c) = (addall (prev a 3) (prev b 18) (prev c 9))]
                [d = (+ a b c)]))
     
     (check-equal? (signal-samples testnet 3)
                   (vector (+ 4 20 12)
                           (+ 5 22 15)
                           (+ 6 24 18))))
   
   (let ()
     ;; multi-init zeros
     (define (addall a b c)
       (values (+ 1 a) (+ 2 b) (+ 3 c)))
     (define testnet
       (network ()
                [(a b c) = (addall (prev a 0) (prev b 0) (prev c 0))]
                [d = (+ a b c)]))
     
     (check-equal? (signal-samples testnet 3)
                   (vector (+ 1 2 3)
                           (+ 2 4 6)
                           (+ 3 6 9))))
   
   
   (let ()
     
     ;; try out new "init"
     (define testnet
       (network ()
                [a = (add1 (prev a 147))]
                [b = (+ a a)]))
     
     (define a (network-init testnet))
     (check-equal? (a) (* 148 2))
     (check-equal? (a) (* 149 2)))
   
   (let ()
     (define ctr1 (simple-ctr 34 14))
     (define sigfun (network-init ctr1))
     (check-equal? (sigfun) 34)
     (check-equal? (sigfun) 48))
   (let ()
     (define ctr1 (loop-ctr 34 14))
     (define sigfun (network-init ctr1))
     (check-equal? (sigfun) 0)
     (check-equal? (sigfun) 14)
     (check-equal? (sigfun) 28)
     (check-equal? (sigfun) 8))
   
   (let ()
     (define ctr1 (network ()
                           [incrs <= (simple-ctr 0 1)]
                           [out <= (loop-ctr/variable 34) incrs]))
     (define sigfun (network-init ctr1))
     (check-equal? (sigfun) 0)
     (check-equal? (sigfun) 0)
     (check-equal? (sigfun) 1)
     (check-equal? (sigfun) 3)
     (check-equal? (sigfun) 6)
     (check-equal? (sigfun) 10)
     (check-equal? (sigfun) 15)
     (check-equal? (sigfun) 21)
     (check-equal? (sigfun) 28)
     (check-equal? (sigfun) 2)
     (check-equal? (sigfun) 11))
   
   (check-equal? (signal-nth (simple-ctr 34 14) 1) 48)
   
   (check-equal? (signal-samples (simple-ctr 4 3) 5)
                 (vector 4 7 10 13 16))
   
   ;; SIGNAL?
   (check-equal? (signal? (lambda () 14)) #t)
   (check-equal? (signal? (lambda (x y) 14)) #f)
   (check-equal? (signal? (lambda args 14)) #t)
   (check-equal? (signal? (network () [out = 3])) #t)
   
   ;; signal-+s
   
   (check-equal? (signal-samples (signal-+s (list (simple-ctr 2 10)
                                                  (simple-ctr 4 9)
                                                  (simple-ctr 10 1)))
                                 4)
                 (vector 16 36 56 76))
   (check-equal? (signal-samples (signal-*s (list (simple-ctr 1.0 -0.25)
                                                  (loop-ctr 20 10)))
                                 4)
                 (vector 0 7.5 0 2.5))
   
   ;; fixed-inputs
   (let ()
     
     (define net (network (a b c) [out = (+ a b)]))
     (define sig (fixed-inputs net 3 14 "hello"))
     (check-equal? (signal-samples sig 4)
                   (vector 17 17 17 17)))
   
   ;; tap
   (let ()
     
     (define net (network () 
                          [ctr <= (simple-ctr 4 0.5)]
                          [tapped <= (tap 3 #f) ctr]))
     (check-equal? (signal-samples net 5)
                   (vector #f #f #f 4 4.5)))
   
))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-test-suite))

