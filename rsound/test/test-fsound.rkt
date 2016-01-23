#lang racket

(module+ test
(require "../rsound.rkt"
         "../fsound.rkt"
         rackunit
         rackunit/text-ui
         racket/block
         ffi/vector)


(run-tests
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
     (check-false (equal? fs fs2))

     (block
      (define v (vector 9.0 728.0 -2.3 4.0))
      (define fs (vector->fsound v 44100))
      (check-equal? (for/vector ([idx 4]) (fs-ith/left fs idx))
                    v)
      (check-equal? (for/vector ([idx 4]) (fs-ith/right fs idx))
                    v)))))

)