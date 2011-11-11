#lang racket

(require "../frequency-response.rkt"
         "../filter.rkt"
         rackunit)


;; 100-pole comb
(define (poly1 z)
  (/ 1 (- 1 (* 0.95 (expt z -100)))))

;; show the whole range:
(response-plot poly1
               0      ; min-freq
               22050) ; max-freq

;; show the range from 10K to 11K:
(response-plot poly1
               10000  ; min-freq
               11000) ; max-freq




;; a single zero at 1:
(response-plot (lambda (z) (- 1 (expt z -1))) 0 22050)



;; the same thing, using poles&zeros:
(response-plot (poles&zeros->fun '() '(1)) 0 22050)

;; modeling a single pole and two zeros.
(response-plot (lambda (z)
                 (let ([a -0.28]
                       [b 0.57])
                 (/ (- 1 (expt z -2))
                    1
                    (+ 1 
                       (* -2 a (expt z -1))
                       (* (+ (* a a) (* b b)) (expt z -2))))))
               0 
               22050)

;; the same thing, using poles&zeros:
(response-plot (poles&zeros->fun '(-0.28+0.57i -0.28-0.57i) '(1 -1))
               0
               22050)



(response-plot (poles&zeros->fun '(0.5 0.5+0.5i 0.5-0.5i) '(0+i 0-i)) 
               0
               22050)

(response-plot (coefficient-sets->fun 
                '(1.0 -3.826392 5.516636 -3.5511127 0.861025)
                '(8.697132e-06 3.478853e-05 5.2182793e-05 3.478853e-05
                  8.697132e-06)) 
               0
               22050)


              
          
              
              