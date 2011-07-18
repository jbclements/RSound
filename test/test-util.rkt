#lang racket 

(require "../rsound.rkt"
         "../util.rkt"
         rackunit)


(define r (make-tone 882 0.2 44100 44100))

(check-equal? (rsound-ith/left/s16 r 0) 0)
(check-equal? (rsound-ith/right/s16 r 50) 0)
(check-= (rsound-ith/left/s16 r 27) 
         (round (* s16max (* 0.2 (sin (* twopi 882 27/44100))))) 0.0)

;; table-based-sine-wave

(check-= ((raw-sine-wave 4 44100) 13) (sin (* 2 pi 13/44100 4)) 1e-4)

(check-= ((raw-square-wave 2 500) 0) 1.0 1e-4)
(check-= ((raw-square-wave 2 500) 10) 1.0 1e-4)
(check-= ((raw-square-wave 2 500) 124) 1.0 1e-4)
(check-= ((raw-square-wave 2 500) 125) -1.0 1e-4)
(check-= ((raw-square-wave 2 500) 249) -1.0 1e-4)
(check-= ((raw-square-wave 2 500) 250)  1.0 1e-4)
(check-= ((raw-square-wave 3 500) 166) -1.0 1e-4)
(check-= ((raw-square-wave 3 500) 167)  1.0 1e-4)



;; vectors->rsound

(let ([r (vectors->rsound (vector 3 4 5) (vector 2 -15 0) 200)]
      [s (/ s16max 15)])
  (check-equal? (rsound-ith/left/s16 r 0)  (round (* s 3)))
  (check-equal? (rsound-ith/right/s16 r 1)  (round (* s -15))))

(check-not-exn (lambda () (make-harm3tone 430 0.1 400 44100)))

;; is fader too slow?
;; answer: not yet a problem.
#;(time 
 (for ([i (in-range 100)])
   (signal->rsound 44100 44100 (fader 44100))))

#;(time 
 (for ([i (in-range 100)])
   (signal->rsound 44100 44100 (sine-wave 440 44100))))

#;(time
 (for ([i (in-range 100)])
   (signal->rsound 44100 44100 (sawtooth-wave 440 44100))))

(let ([tr (sawtooth-wave 100 1000)])
  (check-= (tr 0) 0.0 1e-5)
  (check-= (tr 1) 0.2 1e-5)
  (check-= (tr 5) -1.0 1e-5))

;; memoizing

(let ([s1 (signal->rsound 200 44100 (signal-*s (list (dc-signal 0.5) (sine-wave 100 44100))))]
      [s2 (time (make-tone 100 0.5 441000 44100))]
      [s3 (time (make-tone 100 0.5 441000 44100))])
  (check-= (rsound-ith/right/s16 s1 73)
           (rsound-ith/right/s16 s2 73)
           1e-2)
  (check-= (rsound-ith/right/s16 s2 73)
           (rsound-ith/right/s16 s3 73)
           1e-2))

;; bug in memoization:
(check-not-exn
 (lambda ()
   (make-tone 100 0.5 200 44100)
   (make-tone 100 0.5 400 44100)))

;; FFT

(let* ([tone (make-tone 147 1.0 4800 44100)]
       [fft (rsound-fft/left tone)])
  (check-= (magnitude (vector-ref fft 15)) 0.0 1e-2)
  (check-= (magnitude (vector-ref fft 16)) 2400.0 1e-2)
  (check-= (magnitude (vector-ref fft 17)) 0.0 1e-2))

(check-= ((frisellinator 100) 0) 0.0 1e-4)
(check-= ((frisellinator 100) 100) 1.0 1e-4)
(check-= ((frisellinator 100) 50) 0.5 1e-4)



(check-equal? (binary-logn 4096) 12)
(check-equal? (binary-logn 4095) #f)

;; SIGNAL
(check-equal? ((signal (lambda (t b c) (+ t b c)) 3 4) 1) 8)

;; SIGNAL?
(check-equal? (signal? (lambda (t) 14)) #t)
(check-equal? (signal? (lambda (x y) 14)) #f)
(check-equal? (signal? (lambda args 14)) #t)

;; MIDI-NOTE-NUM->PITCH

(check-= (midi-note-num->pitch 69) 440.0 1e-4)
(check-= (midi-note-num->pitch 57) 220.0 1e-4)
(check-= (midi-note-num->pitch 56) (/ 220 (expt 2 1/12)) 1e-4)
(check-exn (lambda (exn)
             (regexp-match #px"^midi-note-num->pitch: " (exn-message exn)))
           (lambda ()
             (midi-note-num->pitch (list 34 24))))

;; RSOUND->SIGNAL
(check-= ((rsound->signal/left 
           (signal->rsound 100 44100 (lambda (x) (/ x 1000)))) 23)
         0.023
         1e-4)

;; off the edge:
(check-= ((rsound->signal/left 
           (signal->rsound 100 44100 (lambda (x) (/ x 1000)))) 150)
         0.0
         1e-4)

(check-exn exn:fail? (lambda () (rsound->signal/left 14)))

(check-= ((rsound->signal/right
           (signal->rsound 100 44100 (lambda (x) (/ x 1000)))) 23)
         0.023
         1e-4)

(check-exn exn:fail? (lambda () (rsound->signal/right 14)))

;; FIR-FILTER

(define (mush x) (/ (round (* x s16max)) s16max))

(let* ([my-filter (fir-filter '((13 0.2) (5 0.1)))]
       [test-sound (signal->rsound 100 44100 (my-filter (lambda (x) (/ x 500))))])
  (check-= (rsound-ith/right test-sound 0) 0 1e-7)
  (check-= (rsound-ith/right test-sound 1) (mush 1/500) 1e-7)
  (check-= (rsound-ith/right test-sound 4) (mush 4/500) 1e-7)
  (check-= (rsound-ith/right test-sound 5) (mush (+ 5/500 0/5000)) 1e-7)
  (check-= (rsound-ith/right test-sound 6) (mush (+ 6/500 1/5000)) 1e-7)
  (check-= (rsound-ith/right test-sound 12) (mush (+ 12/500 7/5000)) 1e-7)
  (check-= (rsound-ith/left test-sound 78) (mush (+ 78/500 73/5000 65/2500)) 1e-7))

;; IIR-FILTER

(let* ([my-filter (iir-filter '((13 0.2) (5 0.1)))]
       [test-sound (signal->rsound 100 44100 (my-filter (lambda (x) (/ x 500))))])
  (check-= (rsound-ith/right test-sound 0) 0 1e-7)
  (check-= (rsound-ith/right test-sound 1) (mush 1/500) 1e-7)
  (check-= (rsound-ith/right test-sound 4) (mush 4/500) 1e-7)
  (check-= (rsound-ith/right test-sound 5) (mush (+ 5/500 0/5000)) 1e-7)
  (check-= (rsound-ith/right test-sound 6) (mush (+ 6/500 1/5000)) 1e-7)
  (check-= (rsound-ith/right test-sound 10) (mush (+ 10/500 5/5000)) 1e-7)
  ;; now the IIR starts to behave differently:
    (check-= (rsound-ith/right test-sound 11) (mush (+ 11/500 (* 1/10 (+ 6/500 1/5000)))) 1e-7)
  (check-= (rsound-ith/right test-sound 12) (mush (+ 12/500 (* 1/10 (+ 7/500 2/5000)))) 1e-7))

;; CLIP&SCALE

;; reduce-volume 

(check-= (thresh 1.0 7.0) 1.0 1e-4)
(check-= (thresh 2.0 -1.2) -1.2 1e-4)
(check-= (thresh 2.0 -6.0) -2.0 1e-4)
(check-= (thresh -2.5 3.4) 2.5 1e-4)

(let* ([f (lambda (t) (* 2 (sin (* t twopi 1 1/12))))]
       [vf (clip&volume 0.5 f)]
       [sf (scale 4.0 f)])
  (check-= (f 0) 0 1e-4)
  (check-= (f 6) 0 1e-4)
  (check-= (f 3) 2.0 1e-4)
  (check-= (vf 0) 0 1e-4)
  (check-= (vf 3) 0.5 1e-4)
  (check-= (sf 3) 8.0 1e-4))


;; how much slower is signal?
;; answer: negligible; only about 2% slower
(define (n-times-throwaway n x) 
  (time
   (for ([i (in-range n)])
     (x))))

(n-times-throwaway 
 40
 (lambda ()
   (signal->rsound 44100 44100 (lambda (t) (sin (* twopi t 340 1/44100))))))

(define (testfun t f)
  (sin (* twopi t f 1/44100)))

(n-times-throwaway 
 40
 (lambda ()
   (signal->rsound 44100 44100 (lambda (t) (testfun t 340)))))

(n-times-throwaway 
 40
 (lambda ()
   (signal->rsound 44100 44100 (signal testfun 340))))
