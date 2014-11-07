#lang racket

(require "../rsound.rkt"
         "../common.rkt"
         "../util.rkt"
         "../write-wav.rkt"
         "../read-wav.rkt"
         rackunit
         rackunit/text-ui)

(provide the-test-suite)

(define the-test-suite
  (test-suite 
   "write-wav"
   (let ()
     (define sound-len 32114)
     (define test-samplerate 30022)
     (define r (parameterize ([default-sample-rate test-samplerate])
                 (noise sound-len)))
     
     (let ()
       (define temp-filename (make-temporary-file))
       (write-sound/s16vector (rsound-data r) (rsound-start r) 
                              (rsound-stop r) (rsound-sample-rate r) (path->string temp-filename))
       
       (check-equal? (rs-read-frames temp-filename) sound-len)
       (check-equal? (rs-read-sample-rate temp-filename) test-samplerate)
       
       (define s 
         (apply vec->rsound 
                (read-sound/s16vector (path->string temp-filename) 0 #f)))
       
       (check-true
        (for/and ([i (in-range sound-len)])
          (and (= (rs-ith/left/s16 r i) (rs-ith/left/s16 s i))
               (= (rs-ith/right/s16 r i) (rs-ith/right/s16 s i))))))
     
     (let ()
       (define start 10001)
       (define stop  19934)
       (define temp-filename (make-temporary-file))
       (write-sound/s16vector (rsound-data r)  start stop
                              (rsound-sample-rate r)
                              (path->string temp-filename))
       
       (check-equal? (rs-read-frames temp-filename) (- stop start))
       (check-equal? (rs-read-sample-rate temp-filename) test-samplerate)
       
       (define s 
         (apply vec->rsound 
                (read-sound/s16vector (path->string temp-filename) 0 #f)))
       
       (check-true
        (for/and ([i (in-range (- stop start))])
          (and (= (rs-ith/left/s16 r (+ start i)) (rs-ith/left/s16 s i))
               (= (rs-ith/right/s16 r (+ start i)) (rs-ith/right/s16 s i))))))
     
     ;; test case for partial write:
     
     
     
     
     
     )))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-test-suite))
