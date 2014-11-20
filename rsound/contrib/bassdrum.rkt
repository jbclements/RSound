#lang racket

(require plot
         (except-in "../main.rkt" bassdrum)
         racket/runtime-path
         "../draw.rkt")

(define-runtime-path here ".")

;; YAY! finally got the right frequency equation.
#|
(define peaks
  '(53 250 488 785 1165 1631 2210 2889 3638
       4435 5242 6063 6865 7669 8488 9294 10119 10940)
  #;'(0 200 426 704 1061 1501 2057 2706 3453))

(define data
 (for/list ([z2 (in-list (rest peaks))]
            [z1 (in-list peaks)])
   (vector (* 1/44100 (/ (+ z1 z2) 2)) (* 44100 (/ (- z2 z1))) 1.0)))

(plot (mix (points data)
           (line (lambda (t) (+ 53 (exp (+ (* t -48.034146022934905) 
                                             5.262685711446809))))))
      #:x-min 0
      #:x-max (/ 11000 44100)
      #:y-max 200
      #:y-min 0)

(fit (lambda (x m b) (+ (* m x) b))
     '((m -1)
       (b 6))
     #;'(#(0 200 1.0)
       #(1 100 1.0)
       #(2 50 1.0))
     (take data 9))
|#

(define envelope
  (map
   (lambda (x)
     (vector (* 1/44100 (first x))  (/ (second x) 32767) 1.0))
   '((53 13565)
     (250 14172)
     (488 14606)
     (785 14778) 
     (1165 14934)
     (1631 15078)
     (2210 15104)
     (2889 13665)
     (3638 11605)
     (4435 9653) 
     (5242 7926) 
     (6063 6459) 
     (6865 5254) 
     (7669 4242) 
     (8488 3445)
     (9294 2770)
     (10119 2224)
     (10940 1788)
     (11759 1411)
     (12574 1117))))

(plot (mix (points envelope)
           (line
            ((lambda (z a b c d)
               (lambda (x) (exp (+ (* z x x x x)
                                   (* a x x x)
                                   (* b x x)
                                   (* c x)
                                   d))))
             -939.9970010671246
             685.4085357019798
             -182.02721078280973
             9.22106772821044
             -0.8933256427308123)))
      #:x-min 0
      #:x-max 0.5
      #:y-max 1.0
      #:y-min 0
      #:width 600)

;; I guess... plot doesn't contain curve fitting any more?
#;(fit (lambda (x z a b c d) (+ (* z x x x x)
                              (* a x x x)
                              (* b x x)
                              (* c x)
                              d))
     '((z -1)
       (a 1)
       (b -1)
       (c -1)
       (d -1))
     envelope)


#;(plot (line ((lambda (k1 k2 k3 k4) (lambda (x) (+ k1 (* k2 (exp (* k3 (+ x k4)))))))
             111.97512944218796
             199.98814726453253
             -0.8431357195542737
             -99.27499367623983
             ))
      #:x-max 3500
      #:y-max 200)

;; okay, here's the synth kick:

;; frequency terms:
(define m -48.034146022934905)
(define b 5.262685711446809)

;; envelope terms:
(define (env t)
  (exp (+ (* -939.9970010671246 t t t t)
          (* 685.4085357019798 t t t)
          (* -182.02721078280973 t t)
          (* 9.22106772821044 t)
          -0.8933256427308123)))



(define (bassdrum t)
  (let ([t2 (* t 1/44100)])
  (* (env t2)
     (sin (* twopi
             (+ (* 53 t2)
                (* (/ 1 m)
                   (exp (+ (* t2 m) b)))))))))


(define bd (signal->rsound 15727 (indexed-signal bassdrum)))
(rs-draw bd)
(define bd2 (rs-read 
             (build-path here "drum-samples" "bassdrum.wav")))
(rs-write bd "/tmp/bassdrum-synth.wav")
(define bd3 
  (rs-append* (list bd (silence 22050) bd2 (silence 22050))))


