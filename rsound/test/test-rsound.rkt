#lang racket

(require "../rsound.rkt"
         "../common.rkt"
         "../network.rkt"
         "../util.rkt"
         rackunit
         racket/runtime-path
         ffi/vector)

(provide the-test-suite)

;;; HELPERS FOR TEST CASES

(define (round-trip real)
  (s16->real (real->s16 real)))
(define (int-round-trip int)
  (real->s16 (s16->real int)))

(define-runtime-path short-test-wav "./short-test.wav")

(define-runtime-path kick-wav "./kick_01.wav")


(define-runtime-path short-with-pad-wav "./short-with-pad.wav")

(define (add-ticker n)
  (network ()
    [frame = (prev adder 0)]
    [adder = (add1 frame)]
    (out = (n frame))))

(define the-test-suite
  (test-suite "rsound"
(let ()
(let ([t (signal->rsound 100 (add-ticker (lambda (i) (sin (* twopi 13/44100 i)))))])
  (check-equal? (rs-ith/left/s16 t 0) 0)
  (check-equal? (rs-ith/right/s16 t 1) (inexact->exact
                                            (round (* s16max (sin (* twopi 13/44100)))))))

  (define ramp-signal (add-ticker (lambda (i) (/ i 100))))
;; signal->rsound/stereo
(let ([s (signals->rsound 100 ramp-signal 
                          (add-ticker (lambda (i) (- 1 (/ i 100)))))])
  (check-= (rs-ith/left s 13) 13/100 1e-4)
  (check-= (rs-ith/right s 89) 11/100 1e-4))

;; test rs-ith/left & right
(let ([t (signal->rsound 100 (add-ticker
                              (lambda (i) (sin (* twopi 13/44100 i)))))])
  (check-= (rs-ith/left t 0) 0 1e-4)
  (check-= (rs-ith/right t 1) (sin (* twopi 13/44100)) 1e-4))

;; test of rs-equal?
(let ([v1 (signal->rsound 100 ramp-signal)]
      [v2 (signal->rsound 100 ramp-signal)])
  (check-equal? v1 v2)
  (check rs-equal? v1 v2)
  (s16vector-set! (rsound-data v2) 50 -30)
  (check-false (equal? v1 v2))
  (check-equal? (rs-equal? v1 v2) false)
  (check-not-exn (lambda () (equal-hash-code v1))))

;; tests of check-below-threshold:
#;(let ([sample-sound (rsound-append* (list (silence 10) 
                                          (make-tone 2000 0.15 10)))])
  (check-not-exn (lambda () (check-below-threshold (rsound-data sample-sound) (rs-frames sample-sound) 0.15)))
  (check-exn exn:fail? (lambda () (check-below-threshold (rsound-data sample-sound) (rs-frames sample-sound) 0.1))))

(check-exn (lambda (exn)
             (regexp-match #px"^rs-append\\*: " (exn-message exn)))
           (lambda () (rs-append* 34)))

;; how long does it take to scan a minute of sound for loud things?
#;(let ([sample-sound (make-tone 400 0.15 (* 60 (default-sample-rate)))])
  (time (check-below-threshold sample-sound 0.2)))

;; silence:
(let ([s (silence 100)])
  (for ([i (in-range 100)])
    (check-equal? (rs-ith/left/s16 s i) 0)
    (check-equal? (rs-ith/right/s16 s i) 0)))
(check-equal? (rs-frames (silence 22050)) 22050)
(check-equal? (rsound-sample-rate (silence 22050)) (default-sample-rate))
(check-exn (lambda (exn) 
             (regexp-match #px"expected: positive integer" (exn-message exn)))
           (lambda () (silence 0)))

;; sound-list-total-frames:
(check-equal? (sound-list-total-frames (list (list (silence 22050) 0))) 22050)
(check-equal? (sound-list-total-frames (list (list (silence 22050) 0)
                                             (list (silence 44100) 22050)
                                             (list (silence 20000) 44100)))
              66150)

;; different sample rates:
(check-exn exn:fail? (lambda () (same-sample-rate-check (list (parameterize ([default-sample-rate 22050])
                                                                (silence 10)) 
                                                              (silence 20)))))
(check-exn exn:fail? (lambda () (same-sample-rate-check (list ))))
(check-not-exn       (lambda () (same-sample-rate-check (list (silence 10) 
                                                              (silence 20)))))


;; assemble
(let* ([sample-sound (make-tone 400 0.15 6)]
       [overlaid (assemble (list (list sample-sound 0) (list sample-sound 0)))]
       [doublevol (make-tone 400 0.3 6)])
  (for ([i (in-range 6)])
    (check-= (rs-ith/left/s16 overlaid i) (rs-ith/left/s16 doublevol i) 1.0)
    (check-= (rs-ith/right/s16 overlaid i) (rs-ith/right/s16 doublevol i) 1.0))
  
  (define s2 (assemble (list (list sample-sound 0) (list sample-sound 3.0))))
  (check-equal? (rs-ith/left/s16 s2 5)
                (+ (rs-ith/left/s16 sample-sound 5)
                   (rs-ith/left/s16 sample-sound 2))))
  
  (let* ([sample-sound (signal->rsound 100 (lambda () (* 0.2 (random))))]
         [overlaid (assemble (list (list sample-sound 25) 
                                          (list sample-sound 0)
                                          (list sample-sound 75)))])
    (for ([i (in-range 25)])
      (check-equal? (rs-ith/left/s16 overlaid i)
                    (rs-ith/left/s16 sample-sound i)))
    (for ([i (in-range 25 75)])
      (check-equal? (rs-ith/right/s16 overlaid i)
                    (+ (rs-ith/right/s16 sample-sound i)
                       (rs-ith/right/s16 sample-sound (- i 25)))))
    (for ([i (in-range 75 100)])
      (check-equal? (rs-ith/right/s16 overlaid i)
                    (+ (rs-ith/right/s16 sample-sound i)
                       (rs-ith/right/s16 sample-sound (- i 25))
                       (rs-ith/right/s16 sample-sound (- i 75)))))
    (for ([i (in-range 100 125)])
      (check-equal? (rs-ith/left/s16 overlaid i)
                    (+ (rs-ith/left/s16 sample-sound (- i 25))
                       (rs-ith/left/s16 sample-sound (- i 75))))))


;; rsound-read

  
;; better error message on empty .wav file:
(check-exn (lambda (exn) (regexp-match #px"expected: file of length >= 0" (exn-message exn)))
           (lambda ()
             (define f (make-temporary-file))
             (rs-read f)))
  
;; tests copied from read-wav; these call the rsound-read funs directly:

(define test-rsound (rs-read short-test-wav))

(check-equal? (rs-frames test-rsound) 100)
(check-equal? (rs-read-sample-rate short-test-wav) 44100)
(check-equal? (rs-read-frames short-test-wav) 100)

(define (desired-nth-sample n)
  (round (* #x8000 (sin (* 2 pi (/ n 44100) 700)))))
;; truncation due to 16-bit PCM rounding:
(define first-sample (desired-nth-sample 1))
(define second-sample (desired-nth-sample 2))
(define thirtieth-sample (desired-nth-sample 30))
(define fiftieth-sample (desired-nth-sample 50))

(check-= (rs-ith/left/s16 test-rsound 0) 0.0 1e-4)
(check-= (rs-ith/left/s16 test-rsound 1) first-sample 1e-4)
(check-= (rs-ith/right/s16 test-rsound 2) second-sample 1e-4)
;; why is this one not exact? Something to do with negative numbers... still can't quite figure it out.
(check-= (rs-ith/right/s16 test-rsound 50) fiftieth-sample 1)


(define test-sub-rsound (rs-read/clip short-test-wav 30 40))

(check-not-exn (lambda () (rs-read/clip short-test-wav 30 40.0)))

(check-equal? (rs-frames test-sub-rsound) 10)
(check-= (rs-ith/left/s16 test-sub-rsound 0) (desired-nth-sample 30) 1e-4)
(check-= (rs-ith/right/s16 test-sub-rsound 1) (desired-nth-sample 31) 1e-4)

;; round-trip using rsound-write

(let ([temp (make-temporary-file)])
  (rs-write test-rsound temp)
  (check rs-equal? (rs-read temp) test-rsound))

;; round-trip with inexact but integer frame rate
(let ([temp (make-temporary-file)]
      [inexact-fr-sound (rsound (rsound-data test-rsound)
                                (rsound-start test-rsound)
                                (rsound-stop test-rsound)
                                (exact->inexact (rsound-sample-rate test-rsound)))])
  (check-not-exn (lambda () (rs-write inexact-fr-sound temp)))
  (check rs-equal? (rs-read temp) inexact-fr-sound))
  
;; rs-write with non-integer frame rate
  (let ([temp (make-temporary-file)]
      [inexact-fr-sound (rsound (rsound-data test-rsound)
                                (rsound-start test-rsound)
                                (rsound-stop test-rsound)
                                234.241)])
  (check-exn (lambda (exn)
               (regexp-match (regexp-quote "expected: rsound with integer sample rate") 
                             (exn-message exn)))
             (lambda () (rs-write inexact-fr-sound temp))))

;;rsound-append (*)
(let ([short-test2 (rs-append test-rsound test-rsound)])
  (check-equal? (rs-ith/left/s16 short-test2 150) 
                (rs-ith/left/s16 short-test2 50))
  (check-equal? (rs-ith/right/s16 short-test2 153) 
                (rs-ith/right/s16 short-test2 53)))

(let ([short-test2 (rs-append* (list (silence 50)
                                         test-rsound
                                         test-rsound))])
  (check-equal? (rs-ith/left/s16 short-test2 200)
                (rs-ith/left/s16 test-rsound 50))
  (check-equal? (rs-ith/right/s16 short-test2 203) 
                (rs-ith/right/s16 test-rsound 53)))




(define kick-rsound (rs-read kick-wav))

;; purely regression testing:
(check-equal? (rs-frames kick-rsound) 4410)
(check-equal? (rs-ith/left/s16 kick-rsound 1803) 27532)
(check-equal? (rs-ith/right/s16 kick-rsound 1803) 27532)

;; test with PAD

(define short-with-pad (rs-read short-with-pad-wav))
(check-equal? (rs-frames short-with-pad) #x21)
(check-equal? (rs-ith/left/s16 short-with-pad 5) #x892)
(check-equal? (rs-ith/right/s16 short-with-pad 6) #x478)


;; check that you can't loop with an rsound of length 0
;; actually, you can't loop at all...
#;(check-exn exn:fail?
           (lambda () (rs-loop (silence 0))))

;; clipping isn't happening right.

(check-= (/ (rs-ith/left/s16 
             (signal->rsound 
              300
              (add-ticker
               (lambda (i) (* 1.5 (sin (* twopi 147/44100 i))))))
             73)
            #x7fff)
         1.0
         1e-4)


;; set-rs-ith/left! and right!t
(let ([s (silence 100)])
  (set-rs-ith/left! s 34 0.7)
  (set-rs-ith/right! s 87 0.3)
  (check-= (rs-ith/left s 34) 0.7 1e-5)
  (check-= (rs-ith/right s 34) 0.0 1e-5)
  (check-= (rs-ith/left s 87) 0.0 1e-5)
  (check-= (rs-ith/right s 87) 0.3 1e-5))

#|(time (rsound-draw (make-tone 400 0.15 10) 400 100))
(time (rsound-draw (make-tone 400 0.15 100) 400 100))
(time (rsound-draw (make-tone 400 0.15 1000) 400 100))
(time (rsound-draw (rsound-assemble (list (list (make-tone 400 0.15 1000) 0)
                                          (list (make-tone 404 0.15 1000) 0))) 400 100))

|#
  
  ;; rs-filter
  (let ()
    ;; a stateful filter, so we can make sure left and right are independent:
    (define my-filter 
      (network (in)
        [ctr <= (simple-ctr 0 (/ 1.0 200.0))]
        [out = (+ in ctr)]))
    (define n (noise 100))
    (define p (rs-filter n my-filter))
    (for ([i 100])
      (check-= (rs-ith/left p i) 
               (round-trip (+ (rs-ith/left n i) (/ i 200.0)))
               1e-9)
      (check-= (rs-ith/right p i) 
               (round-trip (+ (rs-ith/right n i) (/ i 200.0)))
               1e-9)))
  
  
  )))

;;; TEST CASES
(module+ test
  (require rackunit/text-ui)
  (run-tests the-test-suite))
