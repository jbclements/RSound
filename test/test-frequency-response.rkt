#lang racket

(require "../frequency-response.rkt"
         "../filter.rkt"
         "../filter-typed.rkt"
         racket/flonum
         rackunit)


;; 100-pole comb
(define (poly1 z)
  (/ 1 (- 1 (* 0.95 (expt z -100)))))

(printf "100-pole comb filter, 0 up to Nyquist:\n")
;; show the whole range:
(response-plot poly1
               0.0      ; min-freq
               22050.0) ; max-freq

(printf "100-pole comb filter, 10K up to 11K:\n")
;; show the range from 10K to 11K:
(response-plot poly1
               10000  ; min-freq
               11000) ; max-freq




(printf "a single zero at 1:\n")
;; a single zero at 1:
(response-plot (lambda (z) (- 1 (expt z -1))) 0 22050)


(printf "a single zero at 1:\n")
;; the same thing, using poles&zeros:
(response-plot (poles&zeros->poly '() '(1)) 0 22050)

(printf "one pole, two zeros:\n")
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

(printf "one pole, two zeros:\n")
;; the same thing, using poles&zeros:
(response-plot (poles&zeros->poly '(-0.28+0.57i -0.28-0.57i) '(1 -1))
               0
               22050)

(printf "poles on a vertical line, zeros at i and -i:\n")
(response-plot (poles&zeros->poly '(0.5 0.5+0.5i 0.5-0.5i) '(0+i 0-i)) 
               0
               22050)

(printf "I think this is a set of chebyshev coefficients...:\n")
(response-plot (coefficient-sets->poly
                '(1.0 0.0 0.0 0.0 0.0 0.0)
                '(1.0 -3.826392 5.516636 -3.5511127 0.861025)) 
               0
               22050)

(define (flvector->list flv)
  (for/list ([v (in-flvector flv)]) v))

(lpf-coefficients 0.1)
(lpf-coefficients 0.01)

(response-plot
 (coefficient-sets->poly
  '(1 4 6 4 1)
  '(1 -3.932065224332497 5.808146644839259 -3.8196712238297166 0.9436069610061315))
 0 22050)

(lpf-response 0.1 0 22050)
(lpf-response 0.01 0 22050)

;; this one shows that lpf-sig is doing something sane:
;; this graph should have steep rolloff at about 3000 Hz,
;; and should have 0 db gain at 0 Hz.
(let ()
  (define-values (f i g) (lpf-sig 0.5))
  (printf "fir terms: ~s\n" (flvector->list f))
  (printf "iir terms: ~s\n" (flvector->list i))
  (printf "gain: ~s\n" g)
  (response-plot
   (coefficient-sets->poly
    (map (lambda (x) (* x g)) '(1 4 6 4 1))
    (cons 1.0 (map (lambda (x) (- x ))(flvector->list i))))
   0
   22050))

(define iir-terms
  (reverse
   (list 0.8730817302
         -3.5926648631
         5.5641839804
         -3.8444945434
         1)))


(response-plot
(coefficient-sets->poly 
    (list 1 4 6 4 1)
    iir-terms)
0 22050)
