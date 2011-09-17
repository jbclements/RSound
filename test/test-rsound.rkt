#lang racket

(require "../rsound.rkt"
         rackunit
         rackunit/text-ui
         racket/runtime-path
         ffi/vector)

;;; HELPERS FOR TEST CASES


(define twopi (* 2 pi))
(define s16max #x7fff)

(define (sine-wave pitch sample-rate volume)
  (let ([scalar (* twopi pitch)])
    (lambda (i)
      (let ([t (/ i sample-rate)])
        (* volume (sin (* scalar t)))))))

;; make a monaural pitch with the given number of frames
(define (make-tone pitch volume frames)
  (mono-signal->rsound frames (sine-wave pitch (default-sample-rate) volume)))


(define-runtime-path short-test-wav "./short-test.wav")

(define-runtime-path kick-wav "./kick_01.wav")


(define-runtime-path short-with-pad-wav "./short-with-pad.wav")


;;; TEST CASES
(run-tests
(test-suite "rsound"
(let ()
(let ([t (mono-signal->rsound 100 (lambda (i) (sin (* twopi 13/44100 i))))])
  (check-equal? (rsound-ith/left/s16 t 0) 0)
  (check-equal? (rsound-ith/right/s16 t 1) (inexact->exact
                                            (round (* s16max (sin (* twopi 13/44100)))))))


;; signal->rsound/stereo
(let ([s (signals->rsound 100 (lambda (i) (/ i 100)) (lambda (i) (- 1 (/ i 100))))])
  (check-= (rsound-ith/left s 13) 13/100 1e-4)
  (check-= (rsound-ith/right s 89) 11/100 1e-4))

;; test rsound-ith/left & right
(let ([t (mono-signal->rsound 100 (lambda (i) (sin (* twopi 13/44100 i))))])
  (check-= (rsound-ith/left t 0) 0 1e-4)
  (check-= (rsound-ith/right t 1) (sin (* twopi 13/44100)) 1e-4))

;; test of rsound-equal?
(let ([v1 (mono-signal->rsound 100 (lambda (i) (/ i 100)))]
      [v2 (mono-signal->rsound 100 (lambda (i) (/ i 100)))])
  (check rsound-equal? v1 v2)
  (s16vector-set! (rsound-data v2) 50 -30)
  (check-equal? (rsound-equal? v1 v2) false))
;; tests of make-silence


;; tests of rsound-largest-sample
(let ([sample-sound (make-tone 2000 0.15 10)])
  (check-= (/ (rsound-largest-sample sample-sound) s16max) 0.15 1))

;; tests of check-below-threshold:
#;(let ([sample-sound (rsound-append* (list (make-silence 10) 
                                          (make-tone 2000 0.15 10)))])
  (check-not-exn (lambda () (check-below-threshold (rsound-data sample-sound) (rsound-frames sample-sound) 0.15)))
  (check-exn exn:fail? (lambda () (check-below-threshold (rsound-data sample-sound) (rsound-frames sample-sound) 0.1))))

(check-exn (lambda (exn)
             (regexp-match #px"^rsound-append\\*: " (exn-message exn)))
           (lambda () (rsound-append* 34)))

;; how long does it take to scan a minute of sound for loud things?
#;(let ([sample-sound (make-tone 400 0.15 (* 60 (default-sample-rate)))])
  (time (check-below-threshold sample-sound 0.2)))

;; make-silence:
(let ([s (make-silence 100)])
  (for ([i (in-range 100)])
    (check-equal? (rsound-ith/left/s16 s i) 0)
    (check-equal? (rsound-ith/right/s16 s i) 0)))
(check-equal? (rsound-frames (make-silence 22050)) 22050)
(check-equal? (rsound-sample-rate (make-silence 22050)) (default-sample-rate))

;; sound-list-total-frames:
(check-equal? (sound-list-total-frames (list (list (make-silence 22050) 0))) 22050)
(check-equal? (sound-list-total-frames (list (list (make-silence 22050) 0)
                                             (list (make-silence 44100) 22050)
                                             (list (make-silence 20000) 44100)))
              66150)

;; different sample rates:
(check-exn exn:fail? (lambda () (same-sample-rate-check (list (parameterize ([default-sample-rate 22050])
                                                                (make-silence 10)) 
                                                              (make-silence 20)))))
(check-exn exn:fail? (lambda () (same-sample-rate-check (list ))))
(check-not-exn       (lambda () (same-sample-rate-check (list (make-silence 10) 
                                                              (make-silence 20)))))


;; rsound-overlay*
(let* ([sample-sound (make-tone 400 0.15 6)]
       [overlaid (rsound-overlay* (list (list sample-sound 0) (list sample-sound 0)))]
       [doublevol (make-tone 400 0.3 6)])
  (for ([i (in-range 6)])
    (check-= (rsound-ith/left/s16 overlaid i) (rsound-ith/left/s16 doublevol i) 1.0)
    (check-= (rsound-ith/right/s16 overlaid i) (rsound-ith/right/s16 doublevol i) 1.0)))
  
  (let* ([sample-sound (mono-signal->rsound 100 (lambda (x) (* 0.2 (random))))]
         [overlaid (rsound-overlay* (list (list sample-sound 25) 
                                          (list sample-sound 0)
                                          (list sample-sound 75)))])
    (for ([i (in-range 25)])
      (check-equal? (rsound-ith/left/s16 overlaid i)
                    (rsound-ith/left/s16 sample-sound i)))
    (for ([i (in-range 25 75)])
      (check-equal? (rsound-ith/right/s16 overlaid i)
                    (+ (rsound-ith/right/s16 sample-sound i)
                       (rsound-ith/right/s16 sample-sound (- i 25)))))
    (for ([i (in-range 75 100)])
      (check-equal? (rsound-ith/right/s16 overlaid i)
                    (+ (rsound-ith/right/s16 sample-sound i)
                       (rsound-ith/right/s16 sample-sound (- i 25))
                       (rsound-ith/right/s16 sample-sound (- i 75)))))
    (for ([i (in-range 100 125)])
      (check-equal? (rsound-ith/left/s16 overlaid i)
                    (+ (rsound-ith/left/s16 sample-sound (- i 25))
                       (rsound-ith/left/s16 sample-sound (- i 75))))))

(check-exn exn:fail? 
           (lambda ()
             (rsound-overlay* (list (list (make-tone 400 0.15 3) 0)
                                    (list (make-tone 400 0.15 3) 34/5)))))

;; rsound-read

;; tests copied from read-wav; these call the rsound-read funs directly:

(define test-rsound (rsound-read short-test-wav))

(check-equal? (rsound-frames test-rsound) 100)
(check-equal? (rsound-read-sample-rate short-test-wav) 44100)
(check-equal? (rsound-read-frames short-test-wav) 100)

(define (desired-nth-sample n)
  (round (* #x8000 (sin (* 2 pi (/ n 44100) 700)))))
;; truncation due to 16-bit PCM rounding:
(define first-sample (desired-nth-sample 1))
(define second-sample (desired-nth-sample 2))
(define thirtieth-sample (desired-nth-sample 30))
(define fiftieth-sample (desired-nth-sample 50))

(check-= (rsound-ith/left/s16 test-rsound 0) 0.0 1e-4)
(check-= (rsound-ith/left/s16 test-rsound 1) first-sample 1e-4)
(check-= (rsound-ith/right/s16 test-rsound 2) second-sample 1e-4)
;; why is this one not exact? Something to do with negative numbers... still can't quite figure it out.
(check-= (rsound-ith/right/s16 test-rsound 50) fiftieth-sample 1)


(define test-sub-rsound (rsound-read/clip short-test-wav 30 40))

(check-not-exn (lambda () (rsound-read/clip short-test-wav 30 40.0)))

(check-equal? (rsound-frames test-sub-rsound) 10)
(check-= (rsound-ith/left/s16 test-sub-rsound 0) (desired-nth-sample 30) 1e-4)
(check-= (rsound-ith/right/s16 test-sub-rsound 1) (desired-nth-sample 31) 1e-4)

;; round-trip using rsound-write

(let ([temp (make-temporary-file)])
  (rsound-write test-rsound temp)
  (check rsound-equal? (rsound-read temp) test-rsound))

;;rsound-append (*)
(let ([short-test2 (rsound-append test-rsound test-rsound)])
  (check-equal? (rsound-ith/left/s16 short-test2 150) 
                (rsound-ith/left/s16 short-test2 50))
  (check-equal? (rsound-ith/right/s16 short-test2 153) 
                (rsound-ith/right/s16 short-test2 53)))

(let ([short-test2 (rsound-append* (list (make-silence 50)
                                         test-rsound
                                         test-rsound))])
  (check-equal? (rsound-ith/left/s16 short-test2 200)
                (rsound-ith/left/s16 test-rsound 50))
  (check-equal? (rsound-ith/right/s16 short-test2 203) 
                (rsound-ith/right/s16 test-rsound 53)))

;; rsound-clip

(let ([shorter-test (rsound-clip test-rsound 30 60)])
  (check-equal? (rsound-frames shorter-test) 30)
  (check-equal? (rsound-sample-rate shorter-test) 44100)
  (check-equal? (rsound-ith/left/s16 shorter-test 6)
                (rsound-ith/left/s16 test-rsound 36)))



(define kick-rsound (rsound-read kick-wav))

;; purely regression testing:
(check-equal? (rsound-frames kick-rsound) 4410)
(check-equal? (rsound-ith/left/s16 kick-rsound 1803) 27532)
(check-equal? (rsound-ith/right/s16 kick-rsound 1803) 27532)

;; test with PAD

(define short-with-pad (rsound-read short-with-pad-wav))
(check-equal? (rsound-frames short-with-pad) #x21)
(check-equal? (rsound-ith/left/s16 short-with-pad 5) #x892)
(check-equal? (rsound-ith/right/s16 short-with-pad 6) #x478)


;; check that you can't loop with an rsound of length 0
(check-exn exn:fail?
           (lambda () (rsound-loop (make-silence 0))))

;; clipping isn't happening right.

(check-= (/ (rsound-ith/left/s16 (mono-signal->rsound 300
                                                 (lambda (i) (* 1.5 (sin (* twopi 147/44100 i)))))
                                 73)
            #x7fff)
         1.0
         1e-4)


;; set-rsound-ith/left! and right!
(let ([s (make-silence 100)])
  (set-rsound-ith/left! s 34 0.7)
  (set-rsound-ith/right! s 87 0.3)
  (check-= (rsound-ith/left s 34) 0.7 1e-5)
  (check-= (rsound-ith/right s 34) 0.0 1e-5)
  (check-= (rsound-ith/left s 87) 0.0 1e-5)
  (check-= (rsound-ith/right s 87) 0.3 1e-5))

#|(time (rsound-draw (make-tone 400 0.15 10) 400 100))
(time (rsound-draw (make-tone 400 0.15 100) 400 100))
(time (rsound-draw (make-tone 400 0.15 1000) 400 100))
(time (rsound-draw (rsound-overlay* (list (list (make-tone 400 0.15 1000) 0)
                                          (list (make-tone 404 0.15 1000) 0))) 400 100))

|#)))