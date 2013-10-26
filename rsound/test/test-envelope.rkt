#lang racket

(require "../rsound.rkt"
         "../envelope.rkt"
         rackunit)

   ;; ENVELOPE TESTS

(define (s16-round-trip r)
  (s16->real (real->s16 r)))

(provide the-test-suite)
(define the-test-suite
(test-suite
 "envelope"

 (let ()
   (define env-sound ((adsr 10 0.8 20 0.3 30) 500))
   ;; attack:
   (check-equal? (rs-ith/left env-sound 0) 0.0)
   (check-equal? (rs-ith/left env-sound 1) (s16-round-trip 0.08))
   (check-equal? (rs-ith/left env-sound 3) (s16-round-trip 0.24))
   (check-equal? (rs-ith/right env-sound 3) (s16-round-trip 0.24))
   ;; decay:
   (check-equal? (rs-ith/left env-sound 10) (s16-round-trip 0.8))
   (check-equal? (rs-ith/left env-sound 11) (s16-round-trip 0.775))
   (check-equal? (rs-ith/left env-sound 13) (s16-round-trip 0.725))
   (check-equal? (rs-ith/right env-sound 13) (s16-round-trip 0.725))
   ;; hold:
   (check-equal? (rs-ith/left env-sound 75) (s16-round-trip 0.3))
   (check-equal? (rs-ith/left env-sound 76) (s16-round-trip 0.3))
   (check-equal? (rs-ith/left env-sound 77) (s16-round-trip 0.3))
   (check-equal? (rs-ith/right env-sound 77) (s16-round-trip 0.3))
   ;; release
   (check-equal? (rs-ith/left env-sound 470) (s16-round-trip 0.3))
   (check-equal? (rs-ith/left env-sound 471) (s16-round-trip 0.29))
   (check-equal? (rs-ith/left env-sound 473) (s16-round-trip 0.27))
   (check-equal? (rs-ith/right env-sound 473) (s16-round-trip 0.27))
   )
 (let ()
   


(define envelope-sound
  (signal->rsound
   10000
   (envelope-signal '((0 0.2) (50 0.8) (4050 1.0) (8050 -1.0)))))
(check-= (rs-ith/left envelope-sound 0) (s16-round-trip 0.2) 1e-4)
(check-= (rs-ith/left envelope-sound 1) (s16-round-trip 0.212) 1e-4)
(check-= (rs-ith/right envelope-sound 1) (s16-round-trip 0.212) 1e-4)
(check-= (rs-ith/left envelope-sound 49) (s16-round-trip 0.788) 1e-4)
(check-= (rs-ith/left envelope-sound 50) (s16-round-trip 0.8) 1e-4)
(check-= (rs-ith/left envelope-sound 54) (s16-round-trip 0.8002) 1e-4)
(check-= (rs-ith/left envelope-sound 8048) (s16-round-trip -0.999) 1e-4)
(check-= (rs-ith/left envelope-sound 8050) (s16-round-trip 0.0) 1e-4))))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-test-suite))