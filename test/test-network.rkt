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
           [wave (lookup angle)]
           wave))

(define mynet
  (network ()
           [wave (variable-pitch-oscillator 440)]
           [gain (* wave 0.1)]
           gain))

(define the-test-suite
  (test-suite
   "network tests"
   (let ()
     (define started (network-init mynet))
     (check-equal? (started) (* 0.1 (sin (* 440 1/44100 2 pi))))
     (check-equal? (started) (* 0.1 (sin (* 440 2/44100 2 pi))))
     )))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-test-suite))

#;(define generator ((network/s-maker mynet)))
#;(plot 
 (points (for/list ([i (in-range 100)])
         (vector i (generator)))))