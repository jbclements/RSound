#lang racket

(require "../network.rkt"
         rackunit)

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
           [angle (cyclic-angle incr (prev angle))]
           [wave (lookup angle)]))

(define mynet
  (network ()
           [wave (variable-pitch-oscillator 440)]
           [gain (* wave 0.1)]))

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
                (offby1 (+ 10 (prev offby1)))
                (out ((lambda (i) i) (sub1 offby1)))))
     
     (define a (network-init testnet))
     
     (check-equal? (a) 9)
     (check-equal? (a) 19)
     
     )
   (let ()
     
     ;; try out new "init"
     (define testnet
       (network ()
                (a (add1 (prev a)) #:init 147)
                (b (+ a a))))
     
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
     (check-equal? (sigfun) 0))))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-test-suite))

#;(define generator ((network/s-maker mynet)))
#;(plot 
 (points (for/list ([i (in-range 100)])
         (vector i (generator)))))