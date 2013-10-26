#lang racket

(require "../rsound-commander.rkt"
         rackunit
         ffi/vector)

(provide the-test-suite)
(define the-test-suite
  (test-suite
   "rsound-commander"
   
   ;; tests for signal->signal/block/unsafe
   (let ()
     (define ctr 0)
     (define (simple-signal) 
       (set! ctr (add1 ctr))
       (/ (modulo ctr 32767) 32767))
     (define sbu (signal->signal/block/unsafe simple-signal))
     (define s16vec (make-s16vector 100))
     (sbu (s16vector->cpointer s16vec) 50)
     (check-equal? (s16vector-ref s16vec 0) 1)
     (check-equal? (s16vector-ref s16vec 1) 1)
     (check-equal? (s16vector-ref s16vec 30) 16)
     (check-equal? (s16vector-ref s16vec 31) 16)
     )
   
   (let ()
     (define ctr 0)
     (define (simple-signal) 
       (set! ctr (add1 ctr))
       (modulo ctr 32767))
     (define sbu (signal/16->signal/block/unsafe simple-signal))
     (define s16vec (make-s16vector 100))
     (sbu (s16vector->cpointer s16vec) 50)
     (check-equal? (s16vector-ref s16vec 0) 1)
     (check-equal? (s16vector-ref s16vec 1) 1)
     (check-equal? (s16vector-ref s16vec 30) 16)
     (check-equal? (s16vector-ref s16vec 31) 16)
     )))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-test-suite))