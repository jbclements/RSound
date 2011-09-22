#lang racket

(require "../rsound.rkt"
         "../util.rkt"
         "../write-wav.rkt"
         "../read-wav.rkt"
         rackunit
         rackunit/text-ui)

(run-tests
(test-suite 
 "write-wav"
(let ()
(define sound-len 32114)
(define test-samplerate 30022)
(define r (parameterize ([default-sample-rate test-samplerate])
            (make-tone 882 0.20 sound-len)))

(check-equal? (rs-ith/left/s16 r 0) 0)
(check-= (rs-ith/left/s16 r 27) (round (* 0.2 s16max (sin (* twopi 882 (/ 27 test-samplerate))))) 5.0)
(check-= (rs-ith/right/s16 r 27) (round (* 0.2 s16max (sin (* twopi 882 (/ 27 test-samplerate))))) 5.0)

(define temp-filename (make-temporary-file))
(write-sound/s16vector (rsound-data r) (rsound-sample-rate r) (path->string temp-filename))

(check-equal? (rs-read-frames temp-filename) sound-len)
(check-equal? (rs-read-sample-rate temp-filename) test-samplerate)

(define s (apply rsound (read-sound/s16vector (path->string temp-filename) 0 #f)))

(check-true
 (for/and ([i (in-range sound-len)])
   (and (= (rs-ith/left/s16 r i) (rs-ith/left/s16 s i))
        (= (rs-ith/right/s16 r i) (rs-ith/right/s16 s i))))))))


