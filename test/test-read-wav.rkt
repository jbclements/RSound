#lang racket

(require "../read-wav.rkt"
         "../write-wav.rkt"
         "../rsound.rkt"
         racket/runtime-path
         racket/math
         rackunit
         rackunit/text-ui
         ffi/vector)


(define-runtime-path short-test-wav "./short-test.wav")

(define-runtime-path kick-wav "./kick_01.wav")

(define-runtime-path short-with-pad-wav "./short-with-pad.wav")
(define-runtime-path non-integral-frames-wav "./test-non-integer-frames.wav")
(define-runtime-path short-with-LIST-wav "./test-with-list.wav")


(define test-rsound (apply rsound (read-sound/s16vector short-test-wav 0 #f)))

(define (desired-nth-sample n)
  (round (* #x8000 (sin (* 2 pi (/ n 44100) 700)))))
;; truncation due to 16-bit PCM rounding:
(define first-sample (desired-nth-sample 1))
(define second-sample (desired-nth-sample 2))
(define thirtieth-sample (desired-nth-sample 30))
(define fiftieth-sample (desired-nth-sample 50))


(run-tests
 (test-suite 
  "read tests"
  (let ()
    
    
    (check-= (rsound-ith/left/s16 test-rsound 0) 0.0 1e-4)
    (check-= (rsound-ith/left/s16 test-rsound 1) first-sample 1e-4)
    (check-= (rsound-ith/right/s16 test-rsound 2) second-sample 1e-4)
    ;; why is this one not exact? Something to do with negative numbers... still can't quite figure it out.
    (check-= (rsound-ith/right/s16 test-rsound 50) fiftieth-sample 1)
    
    
    (define test-sub-rsound (apply rsound (read-sound/s16vector short-test-wav 30 40)))
    
    (check-equal? (rsound-frames test-sub-rsound) 10)
    (check-= (rsound-ith/left/s16 test-sub-rsound 0) (desired-nth-sample 30) 1e-4)
    (check-= (rsound-ith/right/s16 test-sub-rsound 1) (desired-nth-sample 31) 1e-4)
    
    
    (define kick-rsound (apply rsound (read-sound/s16vector kick-wav 0 #f)))
    
    ;; purely regression testing:
    (check-equal? (rsound-frames kick-rsound) 4410)
    (check-equal? (rsound-ith/left/s16 kick-rsound 4310) -4195)
    (check-equal? (rsound-ith/right/s16 kick-rsound 4310) -4195)
    
    ;; I want a short example with a PAD chunk...
    
    (define short-with-pad (apply rsound (read-sound/s16vector short-with-pad-wav 0 #f)))
    (check-equal? (rsound-frames short-with-pad) #x21)
    (check-equal? (rsound-ith/left/s16 short-with-pad 5) #x892)
    (check-equal? (rsound-ith/right/s16 short-with-pad 6) #x478)
    
    ;; a test with a non-integer number of frames:
    (check-exn exn:fail? (lambda () (read-sound/s16vector non-integral-frames-wav 0 #f)))
    
    ;; a short example with a #"LIST" chunk:
    
    (define short-with-LIST (apply rsound (read-sound/s16vector short-with-LIST-wav 0 #f)))
    (check-equal? (rsound-frames short-with-LIST) (/ #x78 4))
    (check-equal? (rsound-ith/right/s16 short-with-LIST 2) -4416)
    
    
    
    )))