#lang racket 

(require "../rsound.rkt"
         "../util.rkt"
         "../network.rkt"
         "plot-signal.rkt"
         math/array
         rackunit
         rackunit/text-ui
         (for-syntax syntax/parse))

   (define (round-trip real)
     (s16->real (real->s16 real)))
   (define (int-round-trip int)
     (real->s16 (s16->real int)))

(provide the-test-suite)

(define the-test-suite
(test-suite 
 "utils tests"
 (random-seed 23497)
 (let ()
   ;; non-table-based-sine-wave and square wave
   
   (check-= (signal-nth (fixed-inputs sine-wave 4) 0)
            0.0 #;(sin (* 2 pi 1/44100 4))
            1e-4)
   (check-= (signal-nth (fixed-inputs sine-wave 4) 13)
            (sin (* 2 pi 13/44100 4)) 1e-4)
   
   (define sw (fixed-inputs square-wave 2))
   (check-= (signal-nth sw 0) 1.0 1e-4)
   (check-= (signal-nth sw 10) 1.0 1e-4)
   (check-= (signal-nth sw 11024) 1.0 1e-4)
   (check-= (signal-nth sw 11025) 0.0 1e-4)
   (check-= (signal-nth sw 22049) 0.0 1e-4)
   (check-= (signal-nth sw 22050) 1.0 1e-4)
   
   
   ;; these don't work, because our signals now assume 
   ;; a fixed sample rate of 44.1KHz.
   #;(parameterize ([default-sample-rate 500])
     (define sw (fixed-inputs square-wave 2))
     (check-= (signal-nth sw 0) 1.0 1e-4)
     (check-= (signal-nth sw 10) 1.0 1e-4)
     (check-= (signal-nth sw 124) 1.0 1e-4)
     (check-= (signal-nth sw 125) -1.0 1e-4)
     (check-= (signal-nth sw 249) -1.0 1e-4)
     (check-= (signal-nth sw 250)  1.0 1e-4)
     (check-= (signal-nth sw 166) -1.0 1e-4)
     (check-= (signal-nth sw 167)  1.0 1e-4))
   
   ;; make-tone
   (define r (make-tone 882 0.2 44100))
   
   (check-= (rs-ith/left/s16 r 0) 
            (round (* s16max (* 0.2 (sin (* twopi 882 0/44100))))) 0.0)
   (check-equal? (rs-ith/right/s16 r 50) 0)
   (check-= (rs-ith/left/s16 r 27) 
            (round (* s16max (* 0.2 (sin (* twopi 882 27/44100))))) 0.0)
   ;; errors late:
   (check-= (rs-ith/left/s16 r 44098)
            (round (* s16max (* 0.2 (sin (* twopi 882 44098/44100)))))
            1e-7)
   (let ()
   ;; errors crop up only on certain frequencies:
   (define r (make-tone 440 0.2 44100))
   
     (check-= (rs-ith/left/s16 r 0)
              (round (* s16max (* 0.2 (sin (* twopi 440 0/44100))))) 0.0)
   (check-= (rs-ith/left/s16 r 27) 
            (round (* s16max (* 0.2 (sin (* twopi 440 27/44100))))) 0.0)
   ;; errors late:
     (define (small x) (< x 1e-7))
     (for ([i 44100])
       (unless (small 
                (abs
                 (- (rs-ith/left/s16 r i)
                    (round (* s16max (* 0.2 (sin (* twopi 440 (/ i 44100)))))))))
         (error 'uhoh "failure on sample # ~s" i))))

   ;; can't do this any more...
   ;; non-default sample-rate:
   #;(let ([r (parameterize ([default-sample-rate 3420])
              (make-tone 882 0.2 1000))])
     (check-equal? (rsound-sample-rate r) 3420)
     ;; serious inexactness results from rounding in wavetables:
     (check-= (rs-ith/left/s16 r 27) 
              (round (* s16max (* 0.2 (sin (* twopi 882 28/44100))))) 4.0))
   
   
   (let ()
     (define r (make-tone 400 1.0 1000))
     (define s (clip r 30 60))
     (define t (resample-to-rate 22050 s))
     (check-= (rs-ith/left t 0) (rs-ith/left s 0) 0)
     (check-= (rs-ith/left t 1) (rs-ith/left s 2) 0)
     (check-= (rs-ith/left t 2) (rs-ith/left s 4) 0)
     (check-= (rsound-sample-rate t) 22050 0)
     )
   
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
   
   ;; RS-SCALE USES A TRUNCATE (IN C CODE)
   ;; this is not ideal, but it's the current behavior.
   (let ()
     (define n (noise 500))
     (define o (rs-scale 0.75 n))
     (for ([i 500])
       (check-= (s16->real (truncate (* 0.75 (rs-ith/left/s16 n i))))
                (rs-ith/left o i)
                1e-9)))
   
   (let ()
     (define n (noise 500))
     (define o (rs-scale -0.75 n))
     (for ([i 500])
       (check-= (- (s16->real (truncate (* 0.75 (rs-ith/left/s16 n i)))))
                (rs-ith/left o i)
                1e-9)))
   
   
   ;; vectors->rsound
   
   (let ([r (vectors->rsound (vector 3 4 5) (vector 2 -15 0))]
         [s (/ s16max 15)])
     (check-equal? (rs-ith/left/s16 r 0)  (round (* s 3)))
     (check-equal? (rs-ith/right/s16 r 1)  (round (* s -15))))
   
   #;(check-not-exn (lambda () (make-harm3tone 430 0.1 400)))
   
   ;; is fader too slow?
   ;; answer: not yet a problem.
   #;(time 
      (for ([i (in-range 100)])
        (signal->rsound 44100 (fader 44100))))
   
   #;(time 
      (for ([i (in-range 100)])
        (signal->rsound 44100 (sine-wave 440))))
   
   #;(time
      (for ([i (in-range 100)])
        (signal->rsound 44100 (sawtooth-wave 440))))
   
   (let ([tr (fixed-inputs sawtooth-wave 100)])
     (check-= (signal-nth tr 0) 0.0 1e-5)
     (check-= (signal-nth tr 1) 2/441 1e-5)
     (check-= (signal-nth tr 221) (+ -1.0 1/441) 1e-5))
   
   ;; signal-*
   
   (check-equal? (signal-samples (signal-* (simple-ctr 1 0.5)
                                           (simple-ctr 0 2))
                                 4)
                 (vector 0 3.0 8.0 15.0))
   
   ;; signal-+
   
   (check-equal? (signal-samples (signal-+ (simple-ctr 1.0 0.5)
                                           (simple-ctr 0 2))
                                 4)
                 (vector 1.0 3.5 6.0 8.5))
   
   
   
   ;; memoizing
   
   (let ([s1 (signal->rsound 200 (signal-*s (list (dc-signal 0.5) 
                                                  (fixed-inputs sine-wave 100))))]
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
   
   (let* ([tone (make-tone 172.265625 1.0 4096)]
          [fft (rsound-fft/left tone)])
     (check-= (magnitude (array-ref fft #(15))) 0.0 1e-2)
     (check-= (magnitude (array-ref fft #(16))) 2048.0 1e-2)
     (check-= (magnitude (array-ref fft #(17))) 0.0 1e-2))
   
   (check-= (signal-nth (frisellinator 100) 0) 0.0 1e-4)
   (check-= (signal-nth (frisellinator 100) 100) 1.0 1e-4)
   (check-= (signal-nth (frisellinator 100) 50) 0.5 1e-4)
   
   ;; MIDI-NOTE-NUM->PITCH
   
   (check-= (midi-note-num->pitch 69) 440.0 1e-4)
   (check-= (midi-note-num->pitch 57) 220.0 1e-4)
   (check-= (midi-note-num->pitch 56) (/ 220 (expt 2 1/12)) 1e-4)
   (check-exn (lambda (exn)
                (regexp-match #px"^midi-note-num->pitch: " (exn-message exn)))
              (lambda ()
                (midi-note-num->pitch (list 34 24))))
   
   ;; PITCH->MIDI-NOTE-NUM
   
   (check-= (pitch->midi-note-num 440.0) 69.0 1e-4)
   (check-= (pitch->midi-note-num 220.0) 57.0 1e-4)
   (check-= (pitch->midi-note-num (midi-note-num->pitch 22.0)) 22.0 1e-4)
   
   ;; indexed-signal
   (check-= (signal-nth (indexed-signal (lambda (x) (* x 3))) 4)
            12
            1e-4)
   
   ;; RSOUND->SIGNAL
   (check-= (signal-nth (rsound->signal/left 
                         (mono 100 x (/ x 1000))) 23)
            0.023
            1e-4)
   
   ;; off the edge:
   (check-= (signal-nth 
             (rsound->signal/left 
              (mono 100 x (/ x 1000))) 150)
            0.0
            1e-4)
   
   (check-exn exn:fail? (lambda () (rsound->signal/left 14)))
   
   (check-= (signal-nth
             (rsound->signal/right
              (mono 100 x (/ x 1000))) 23)
            0.023
            1e-4)
   
   (check-exn exn:fail? (lambda () (rsound->signal/right 14)))
   
   ;; CLIP&SCALE
   
   ;; reduce-volume 
   
   (check-= (thresh 1.0 7.0) 1.0 1e-4)
   (check-= (thresh 2.0 -1.2) -1.2 1e-4)
   (check-= (thresh 2.0 -6.0) -2.0 1e-4)
   (check-= (thresh -2.5 3.4) 2.5 1e-4)
   
   (let* ([f (indexed-signal (lambda (t) (* 2 (sin (* t twopi 1 1/12)))))]
          [vf (clip&volume 0.5 f)]
          [sf (signal-scale 4.0 f)])
     (check-= (signal-nth f 0) 0 1e-4)
     (check-= (signal-nth f 6) 0 1e-4)
     (check-= (signal-nth f 3) 2.0 1e-4)
     (check-= (signal-nth vf 0) 0 1e-4)
     (check-= (signal-nth vf 3) 0.5 1e-4)
     (check-= (signal-nth sf 3) 8.0 1e-4))
   
   
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
     (for/and ([i (in-range 50)])
       (check-= (rs-ith/left u i) 
                (round-trip (* (rs-ith/left s i) (rs-ith/left t i))) 1e-9)
       (check-= (rs-ith/right u i) 
                (round-trip (* (rs-ith/right s i) (rs-ith/right t i))) 1e-9)))
   
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
   
   ;; resample/interp
   ;; tolerances low (1e-4) because of rounding
   ;; due to s16 coercion
   (let ()
     (define s1 (noise 50))
     (define s2 (resample/interp 0.25 s1))
     (check-equal? (rs-frames s2) 200)
     (check-equal? (rs-ith/right s2 156)
                   (rs-ith/right s1 39))
     (check-= (rs-ith/left s2 157)
              (+ (* 0.75 (rs-ith/left s1 39))
                 (* 0.25 (rs-ith/left s1 40)))
              1e-4)
     (check-= (rs-ith/right s2 158)
              (+ (* 0.5 (rs-ith/right s1 39))
                 (* 0.5 (rs-ith/right s1 40)))
              1e-4)
     (check-= (rs-ith/left s2 159)
              (+ (* 0.25 (rs-ith/left s1 39))
                 (* 0.75 (rs-ith/left s1 40)))
              1e-4))
   
   ;; wavetable-osc
   
   (define sine-wt
     (make-tone 1 1.0 44100))
   (define sound
     (signal->rsound
      10
      (fixed-inputs (wavetable-osc/l sine-wt)
                    0.3
                    440)))
   

   (check-equal? (rs-ith/left sound 0) 0.0)
   (check-equal? (rs-ith/left sound 1) 
                 (round-trip (* 0.3 (rs-ith/left sine-wt 440))))
   (check-equal? (rs-ith/left sound 2)
                 (round-trip (* 0.3 (rs-ith/left sine-wt 880))))
   
   
   
   
   
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
   (signal->rsound (default-sample-rate) (lambda (t) (sin (* twopi t 340 1/44100))))))

(define (testfun t f)
  (sin (* twopi t f 1/44100)))

(n-times-throwaway 
 40
 (lambda ()
   (signal->rsound (default-sample-rate) (lambda (t) (testfun t 340)))))

(n-times-throwaway 
 40
 (lambda ()
   (signal->rsound (default-sample-rate) (signal testfun 340))))
|#
   
   
   (check-exn
    (lambda (exn) (regexp-match #px"sine-wave: contract violation" (exn-message exn)))
    (lambda ()
      (signal-nth
       (network ()
         [out <= sine-wave #f])
       1))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-test-suite))

