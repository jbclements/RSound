#lang racket

(require "../rsound.rkt"
         "../util.rkt"
         "../paste-util.rkt"
         ffi/vector
         rackunit)

(provide the-test-suite)
(define the-test-suite
 (test-suite 
  "paste-util"
  
  (let ()
    (define s (noise 50))
    
    (zero-buffer! (s16vector->cpointer (rsound-data s))
                  50)
    (for/and ([i (in-range 50)])
      (check-equal? (rs-ith/left/s16 s i) 0)
      (check-equal? (rs-ith/right/s16 s i) 0)))
  
  (let ()
    
    (define s (mono 50 t (* 0.1 (random))))
    (define t (mono 50 t 0.01))
    
    (rs-copy-add! (s16vector->cpointer (rsound-data t)) 15
                  s 9
                  11
                  50)
    (for/and ([i (in-range 9 20)]
              [j (in-range 15 26)])
      (check-= (rs-ith/left t j)
               (+ 0.01 (rs-ith/left s i))
               1e-3)
      (check-= (rs-ith/right t j)
               (+ 0.01 (rs-ith/right s i))
               1e-3)))
  
  
  (let ()
    
    (define s (noise 50))
    (define t (mono 50 t 0.01))
    
    (rs-copy-mult-add! (s16vector->cpointer (rsound-data t)) 15
                  s 9
                  11 50
                  -0.5)
    (for/and ([i (in-range 9 20)]
              [j (in-range 15 26)])
      (check-= (rs-ith/left t j)
               (+ 0.01 (* -0.5 (rs-ith/left s i)))
               1e-3)
      (check-= (rs-ith/right t j)
               (+ 0.01 (* -0.5 (rs-ith/right s i)))
               1e-3)))
  
  (let ()
    
    (define s (noise 50))
    (define t (mono 50 t 0.01))
    
    (check-exn exn:fail?
               (lambda ()
                 (rs-copy-add! (s16vector->cpointer (rsound-data t))
                               -3
                               s 9
                               11
                               50))))
  
  (let ()
    
    (define s (noise 50))
    (define t (mono 50 t 0.01))
    
    (check-exn exn:fail?
               (lambda ()
                 (rs-copy-add! (s16vector->cpointer (rsound-data t))
                               45
                               s 9
                               11
                               50))))
  
  (let ()
    
    (define s (noise 50))
    (define t (mono 50 t 0.01))
    
    (check-exn exn:fail?
               (lambda ()
                 (rs-copy-add! (s16vector->cpointer (rsound-data t))
                               15
                               s 45
                               11
                               50))))
  
  (let ()
    
    (define s (noise 50))
    (define t (mono 50 t 0.01))
    
    (check-exn exn:fail?
               (lambda ()
                 (rs-copy-add! (s16vector->cpointer (rsound-data t))
                               15
                               s 9
                               -4
                               50))))
  
  ;; test for an rsound that's clipped
  (let ()
    
    (define s (clip (noise 50) 5 50))
    (define t (mono 50 t 0.01))
    
    (rs-copy-add! (s16vector->cpointer (rsound-data t)) 15
                  s 9
                  11
                  50)
    (for/and ([i (in-range 9 11)]
              [j (in-range 15 26)])
      (check-= (rs-ith/left t j)
               (+ 0.01 (rs-ith/left s i))
               1e-3)
      (check-= (rs-ith/right t j)
               (+ 0.01 (rs-ith/right s i))
               1e-3)))
  
  
  
  ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-test-suite))