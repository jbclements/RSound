#lang racket

(require "../frequency-response.rkt"
         rackunit)



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

(check-= (product-of '(1 2 3 4)) 24 0.0)
(check-= (sum-of '(1 2 3 4)) 10 0.0)

(check-= ((coefficients->poly '(3 1 9)) 6)
         123
         1e-7)
              

;; EXAMPLES

;; 100-pole comb
(define (poly1 z)
  (/ 1 (- 1 (* 0.95 (expt z -100)))))

;; show the whole range:
(response-plot poly1
               30
               0      ; min-freq
               22050) ; max-freq

;; show the range from 10K to 11K:
(response-plot poly1
               30
               10000  ; min-freq
               11000) ; max-freq




;; a single zero at 1:
(response-plot (lambda (z) (- 1 (expt z -1))) 6 0 22050)

;; the same thing, using poles&zeros:
(response-plot (poles&zeros->fun '() '(1)) 6 0 22050)

;; modeling a single pole and two zeros.
(response-plot (lambda (z)
                 (let ([a -0.28]
                       [b 0.57])
                 (/ (- 1 (expt z -2))
                    1
                    (+ 1 
                       (* -2 a (expt z -1))
                       (* (+ (* a a) (* b b)) (expt z -2))))))
               11
               0 
               22050)

;; the same thing, using poles&zeros:
(response-plot (poles&zeros->fun '(-0.28+0.57i -0.28-0.57i) '(1 -1))
               11
               0
               22050)

(response-plot (poles&zeros->fun '(0.5 0.5+0.5i 0.5-0.5i) '(0+i 0-i))
               40 
               0
               22050)

(response-plot (coefficient-sets->fun 
                '(1.0 -3.826392 5.516636 -3.5511127 0.861025)
                '(8.697132e-06 3.478853e-05 5.2182793e-05 3.478853e-05
                  8.697132e-06))
               0 
               0
               22050)

              
          
              
              