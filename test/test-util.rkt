#lang racket 

(require "../rsound.rkt"
         "../util.rkt"
         rackunit
         rackunit/text-ui
         (for-syntax syntax/parse))

(run-tests
(test-suite "utils tests"
(let ()
(define r (make-tone 882 0.2 (default-sample-rate)))

(check-equal? (rs-ith/left/s16 r 0) 0)
(check-equal? (rs-ith/right/s16 r 50) 0)
(check-= (rs-ith/left/s16 r 27) 
         (round (* s16max (* 0.2 (sin (* twopi 882 27/44100))))) 0.0)
  
  ;; non-default sample-rate:
  (let ([r (parameterize ([default-sample-rate 3420])
             (make-tone 882 0.2 1000))])
    (check-equal? (rsound-sample-rate r) 3420)
    ;; serious inexactness results from rounding in wavetables:
    (check-= (rs-ith/left/s16 r 27) 
             (round (* s16max (* 0.2 (sin (* twopi 882 27/3420))))) 4.0))

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
  
  (define pulse-12.5 (make-pulse-tone 0.125))
  (define short-pulse  (pulse-12.5 441 0.2 50))
  (check-= (rs-ith/left short-pulse 0) 0.2 1e-4)
  (check-= (rs-ith/left short-pulse 11) 0.2 1e-4)
  (check-= (rs-ith/left short-pulse 13) -0.2 1e-4)


;; overlay*
  
  (define shorty (mono 20 t (* 0.01 t)))
  (check-= (rs-ith/left shorty 2) 0.02 1e-4)
  (check-= (rs-ith/right shorty 9) 0.09 1e-4)
  
  (define shorty2 (rs-overlay shorty shorty))
  (check-= (rs-ith/right shorty2 13) 0.26 1e-4)
  
  (define shorty3 (rs-overlay* (list shorty shorty shorty)))
  (check-= (rs-ith/left shorty3 6) 0.18 1e-4)
  
  (let ([s (rs-scale 0.75 shorty)])
    (check-= (rs-ith/left s 8) 0.06 1e-4))
  

;; vectors->rsound

(let ([r (vectors->rsound (vector 3 4 5) (vector 2 -15 0))]
      [s (/ s16max 15)])
  (check-equal? (rs-ith/left/s16 r 0)  (round (* s 3)))
  (check-equal? (rs-ith/right/s16 r 1)  (round (* s -15))))

(check-not-exn (lambda () (make-harm3tone 430 0.1 400)))

;; is fader too slow?
;; answer: not yet a problem.
#;(time 
 (for ([i (in-range 100)])
   (mono-signal->rsound 44100 (fader 44100))))

#;(time 
 (for ([i (in-range 100)])
   (mono-signal->rsound 44100 (sine-wave 440))))

#;(time
 (for ([i (in-range 100)])
   (mono-signal->rsound 44100 (sawtooth-wave 440))))

  (parameterize ([default-sample-rate 1000])
(let ([tr (sawtooth-wave 100)])
  (check-= (tr 0) 0.0 1e-5)
  (check-= (tr 1) 0.2 1e-5)
  (check-= (tr 5) -1.0 1e-5)))

;; memoizing

(let ([s1 (mono-signal->rsound 200 (signal-*s (list (dc-signal 0.5) 
                                                    (sine-wave 100))))]
      [s2 (time (make-tone 100 0.5 441000))]
      [s3 (time (make-tone 100 0.5 441000))])
  (check-= (rs-ith/right/s16 s1 73)
           (rs-ith/right/s16 s2 73)
           1e-2)
  (check-= (rs-ith/right/s16 s2 73)
           (rs-ith/right/s16 s3 73)
           1e-2))

;; bug in memoization:
(check-not-exn
 (lambda ()
   (make-tone 100 0.5 200)
   (make-tone 100 0.5 400)))

;; FFT

(let* ([tone (make-tone 147 1.0 4800)]
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
           (mono-signal->rsound 100 (lambda (x) (/ x 1000)))) 23)
         0.023
         1e-4)

;; off the edge:
(check-= ((rsound->signal/left 
           (mono-signal->rsound 100 (lambda (x) (/ x 1000)))) 150)
         0.0
         1e-4)

(check-exn exn:fail? (lambda () (rsound->signal/left 14)))

(check-= ((rsound->signal/right
           (mono-signal->rsound 100 (lambda (x) (/ x 1000)))) 23)
         0.023
         1e-4)

(check-exn exn:fail? (lambda () (rsound->signal/right 14)))

;; CLIP&SCALE

;; reduce-volume 

(check-= (thresh 1.0 7.0) 1.0 1e-4)
(check-= (thresh 2.0 -1.2) -1.2 1e-4)
(check-= (thresh 2.0 -6.0) -2.0 1e-4)
(check-= (thresh -2.5 3.4) 2.5 1e-4)

(let* ([f (lambda (t) (* 2 (sin (* t twopi 1 1/12))))]
       [vf (clip&volume 0.5 f)]
       [sf (signal-scale 4.0 f)])
  (check-= (f 0) 0 1e-4)
  (check-= (f 6) 0 1e-4)
  (check-= (f 3) 2.0 1e-4)
  (check-= (vf 0) 0 1e-4)
  (check-= (vf 3) 0.5 1e-4)
  (check-= (sf 3) 8.0 1e-4))
  
  
(let ()
  (define s (noise 100))
  (define t (rearrange 100 (lambda (x) (- 99 x)) s))
  (for/and ([i (in-range 100)])
    (and (equal? (rs-ith/left/s16 s i) (rs-ith/left/s16 t (- 99 i)))
         (equal? (rs-ith/right/s16 s i) (rs-ith/right/s16 t (- 99 i))))))
  
  ;; RS-MAP
  (define (within a b tol)
    (< (abs (- a b)) tol))

  (let ()
    (define s (noise 50))
    (define t (rs-map/idx (lambda (s i)
                            (* s (/ i 50)))
                          s))
    (check-true
     (for/and ([i (in-range 50)])
       (and (within (* (/ i 50) (rs-ith/left s i)) 
                    (rs-ith/left t i)
                    1e-2)
            (within (* (/ i 50) (rs-ith/right s i))
                    (rs-ith/right t i)
                    1e-2)))))
  
  (let ()
    (define s (noise 50))
    (define t (noise 50))
    (define u (rs-mult s t))
    (for/and ([i (in-range 1)])
      (check-= (rs-ith/left u i) (* (rs-ith/left s i) (rs-ith/left t i)) 1e-2)
      (check-= (rs-ith/right u i) 
               (* (rs-ith/right s i) 
                  (rs-ith/right t i)) 1e-2)))
  
  ;; TILE-TO-LEN
  
  (let ()
    (define s (noise 12))
    (define t (tile-to-len s 41))
    (for/and ([i (in-range 41)])
      (check-equal? (rs-ith/left/s16 t i)
                    (rs-ith/left/s16 s (modulo i 12)))
      (check-equal? (rs-ith/right/s16 t i)
                    (rs-ith/right/s16 s (modulo i 12)))))
  
  
(let ()
  (define s1 (fader-snd 11025 150))
  (check-= (rs-ith/left s1 149)
           (expt (expt 0.001 (/ 1 11025)) 149)
           1e-3))

  ;; RESAMPLE
  (let ()
    (define s1 (noise 50))
    (define s2 (resample 0.25 s1))
    (check-equal? (rs-frames s2) 200)
    (check-equal? (rs-ith/right s2 156)
                  (rs-ith/right s1 39))
    (check-equal? (rs-ith/left s2 157)
                  (rs-ith/left s1 39))
    (check-equal? (rs-ith/right s2 158)
                  (rs-ith/right s1 39))
    (check-equal? (rs-ith/left s2 159)
                  (rs-ith/left s1 39)))
  
  
  
;; tests of rsound-largest-sample
(let ([sample-sound (make-tone 2000 0.15 10)])
  (check-= (/ (rs-largest-sample sample-sound) s16max) 0.15 1))
  
  
;; clip
(define test-rsound (noise 200))
  
  (check-equal? (rsound-start test-rsound) 0)
  (check-equal? (rsound-stop test-rsound) 200)
  (let ([shorter-test (clip test-rsound 30 60)])
    (check-equal? (rsound-start shorter-test) 30)
    (check-equal? (rsound-stop shorter-test) 60)
    (check-equal? (rs-frames shorter-test) 30)
    (check-equal? (rsound-sample-rate shorter-test) 44100)
    (check-equal? (rs-ith/left/s16 shorter-test 6)
                  (rs-ith/left/s16 test-rsound 36)))
  
  
;; how much slower is signal?
;; answer: negligible; only about 2% slower
#|(define (n-times-throwaway n x) 
  (time
   (for ([i (in-range n)])
     (x))))

(n-times-throwaway 
 40
 (lambda ()
   (mono-signal->rsound (default-sample-rate) (lambda (t) (sin (* twopi t 340 1/44100))))))

(define (testfun t f)
  (sin (* twopi t f 1/44100)))

(n-times-throwaway 
 40
 (lambda ()
   (mono-signal->rsound (default-sample-rate) (lambda (t) (testfun t 340)))))

(n-times-throwaway 
 40
 (lambda ()
   (mono-signal->rsound (default-sample-rate) (signal testfun 340))))
|#
)))

