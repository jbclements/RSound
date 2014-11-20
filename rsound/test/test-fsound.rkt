#lang racket

(require "../rsound.rkt"
         "../fsound.rkt"
         #;"../common.rkt"
         #;"../network.rkt"
         #;"../util.rkt"
         rackunit
         racket/runtime-path
         ffi/vector)

(provide the-test-suite)

(define the-test-suite
  (test-suite
   "fsound tests"
   (let ()
     (define v (list->s16vector '(24 978 -322 16000)))
     
     (define r (vec->rsound v 44100))
     
     (define fs (rsound->fsound r))
     
     (check-equal? (fs-ith/left fs 0)
                   (exact->inexact 24/32767))
     
     (check-equal? (fs-ith/right fs 0)
                   (exact->inexact 978/32767))
     
     (check-equal? (fs-ith/left fs 1)
                   (exact->inexact  -322/32767))
     
     (check-equal? (fs-ith/right fs 1)
                   (exact->inexact 16000/32767))
     
     (define fs2 
       (vec->fsound (list->f64vector 
                     (map exact->inexact 
                          (map (lambda (x) (/ x 32767))
                               '(24 978 -322 16000))))
                    44100))
     
     
     (check-true (fs-equal? fs fs2))
     (check-true (equal? fs fs2))
     (check-true (rs-equal? r (fsound->rsound fs)))
     (check-true (equal? r (fsound->rsound fs)))
     
     (set-fs-ith/left! fs2 1 0.879)
     (check-false (equal? fs fs2)))))


(module+ test
  (require rackunit/text-ui)
  (run-tests the-test-suite))
