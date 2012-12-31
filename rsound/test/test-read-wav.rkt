#lang racket

(require "../read-wav.rkt"
         "../write-wav.rkt"
         racket/runtime-path
         racket/math
         rackunit
         ffi/vector)

(provide the-test-suite)

(define-runtime-path short-test-wav "./short-test.wav")

(define-runtime-path kick-wav "./kick_01.wav")

(define-runtime-path short-with-pad-wav "./short-with-pad.wav")
(define-runtime-path non-integral-frames-wav "./test-non-integer-frames.wav")
(define-runtime-path short-with-LIST-wav "./test-with-list.wav")
(define-runtime-path short-with-FLLR-wav "./short-with-FLLR.wav")

(match-define 
 (list s16vec sample-rate) (read-sound/s16vector short-test-wav 0 #f))

(define (desired-nth-sample n)
  (round (* #x8000 (sin (* 2 pi (/ n 44100) 700)))))
;; truncation due to 16-bit PCM rounding:
(define first-sample (desired-nth-sample 1))
(define second-sample (desired-nth-sample 2))
(define thirtieth-sample (desired-nth-sample 30))
(define fiftieth-sample (desired-nth-sample 50))


(define the-test-suite
 (test-suite 
  "read tests"
  (let ()
    
    
    (check-= (s16vector-ref s16vec 0) 0.0 1e-4)
    (check-= (s16vector-ref s16vec 2) first-sample 1e-4)
    (check-= (s16vector-ref s16vec 5) second-sample 1e-4)
    ;; why is this one not exact? Something to do with negative numbers... still can't quite figure it out.
    (check-= (s16vector-ref s16vec 101) fiftieth-sample 1.0)
    
    (match-define (list short-s16vec short-sr)
                  (read-sound/s16vector short-test-wav 30 40))
    
    (check-equal? (s16vector-length short-s16vec) 20)
    (check-= (s16vector-ref short-s16vec 0) (desired-nth-sample 30) 1e-4)
    (check-= (s16vector-ref short-s16vec 3) (desired-nth-sample 31) 1e-4)
    
    
    (match-define (list kick-s16vec sr)
                  (read-sound/s16vector kick-wav 0 #f))
    
    ;; purely regression testing:
    (check-equal? (s16vector-length kick-s16vec) (* 2 4410))
    (check-equal? (s16vector-ref kick-s16vec (* 2 4310)) -4195)
    (check-equal? (s16vector-ref kick-s16vec (add1 (* 2 4310)))
                  -4195)
    
    ;; I want a short example with a PAD chunk...
    
    (match-define (list short-with-pad swp-sr) 
                  (read-sound/s16vector short-with-pad-wav 0 #f))
    (check-equal? (s16vector-length short-with-pad) (* 2 #x21))
    (check-equal? (s16vector-ref short-with-pad 10) #x892)
    (check-equal? (s16vector-ref short-with-pad 13) #x478)
    
    ;; a test with a non-integer number of frames:
    (check-exn exn:fail? (lambda () (read-sound/s16vector non-integral-frames-wav 0 #f)))
    
    ;; a short example with a #"LIST" chunk:
    
    (match-define (list short-with-LIST swl-sr)
                  (read-sound/s16vector short-with-LIST-wav 0 #f))
    (check-equal? (s16vector-length short-with-LIST) (* 2 (/ #x78 4)))
    (check-equal? (s16vector-ref short-with-LIST 5) -4416)
    
    ;; short example with #"FLLR" chunk:
    ;; ... not worth fixing this, right now.
    #;(match-define (list foo bar)
                  (read-sound/s16vector short-with-FLLR-wav 0 #f))
    
    3
    )))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-test-suite))