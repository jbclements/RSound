#lang racket

(require "../frequency-response.rkt"
         "../filter.rkt"
         "../filter-typed.rkt"
         racket/flonum
         rackunit
         plot)


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

(lpf-response-plot 0.1 0 22050)
(lpf-response-plot 0.01 0 22050)

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

;; okay, let's see if these filters are actually doing what they're supposed to.


(define reference-s-poles-500
  (list 
   -0.0099444612+0.0700835358i
   -0.0240080532+0.0290295510i
   -0.0240080532-0.0290295510i
   -0.0099444612-0.0700835358i)
)

(define reference-s-poles-1000
  (list
   -0.0199142092+0.1403452799i
   -0.0480771539+0.0581329184i
   -0.0480771539-0.0581329184i
   -0.0199142092-0.1403452799i
   
   )
  )

(define reference-s-poles-11025
  (list
   -0.2790719918+1.9667583290i
   -0.6737393875+0.8146579738i
   -0.6737393875-0.8146579738i
   -0.2790719918-1.9667583290i

   ))

(define reference-s-poles-1500
  (list
   -0.0299347885+0.2109652578i
   -0.0722689725+0.0873846710i
   -0.0722689725-0.0873846710i
   -0.0299347885-0.2109652578i))

(define reference-s-poles-1000-bigripple
 (list
  -0.0087239785+0.1335251798i
  -0.0210615472+0.0553079404i
  -0.0210615472-0.0553079404i
  -0.0087239785-0.1335251798i
  ))


(define (cplx->xy p)
  (vector (real-part p) (imag-part p)))

(plot
 (mix 
  (points 
   (map (lambda (cplx)
          (vector (real-part cplx) (imag-part cplx)))
        reference-s-poles-500))
  (points 
   (map (lambda (cplx)
          (vector (real-part cplx) (imag-part cplx)))
        reference-s-poles-1000))
  (points 
   (map (lambda (cplx)
          (vector (real-part cplx) (imag-part cplx)))
        reference-s-poles-1500))
  (points 
   (map (lambda (cplx)
          (vector (real-part cplx) (imag-part cplx)))
        reference-s-poles-11025)
   #:color "blue")
  (points
   (map (lambda (cplx)
          (define multiplied (* 11.025 1.3 0.1425 cplx))
          (vector (real-part multiplied) (imag-part multiplied)))
        chebyshev-s-poles)
   #:color "red")
  )
 #:x-min -3
 #:x-max 3
 #:y-min -3
 #:y-max 3)

(define reference-z-poles-1000
  (map s-space->z-space reference-s-poles-1000))

(define reference-z-poles-11025
  (map s-space->z-space reference-s-poles-11025))

(define reference-z-poles-direct-1000
  (list
   0.9707680768+0.1369305667i
   0.9514791950+0.0553910679i
   0.9514791950-0.0553910679i
   0.9707680768-0.1369305667i
   ))

(define reference-z-poles-direct-11025
  (list
   0.0059565954+0.8681048776i
   0.3689458180+0.4171022170i
   0.3689458180-0.4171022170i
   0.0059565954-0.8681048776i
   ))



(plot 
 (mix
  (points
   (map cplx->xy reference-z-poles-direct-1000))
  (points
   (map cplx->xy reference-z-poles-1000)
   #:color "red")
  (parametric
   (lambda (t)
     (vector (cos t) (sin t)))
   0 
   (* 2 pi)))
 #:x-min -1
 #:x-max 1
 #:y-min -1
 #:y-max 1
 )


;(lpf-response-plot 0.142 0 22050 #:db #f)
;(lpf-response-plot 1.0 #;(* 2 pi 0.25) 0 22050 #:db #f)
(lpf-response-plot 0.8 0 22050 #:db #f)

(define coeff (roots->coefficients reference-z-poles-11025))

(define fir-terms (map (lambda (x) (/ x 16.0))(list 1.0 4.0 6.0 4.0 1.0)))
(define iir-terms coeff)
(define fun 
  (coefficient-sets->poly
   fir-terms
   iir-terms))
(response-plot fun 0 22050)

(plot (function
       (lambda (omega)
         (* 2 (tan (/ omega 2)))))
      #:x-min (- pi)
      #:x-max pi
      #:y-max 50
      #:y-min -50)
