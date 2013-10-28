#lang racket

(require "../private/s16vector-add.rkt"
         ffi/vector
         ffi/unsafe
         rackunit)

(provide the-test-suite)

(define the-test-suite
(test-suite
 "s16-vector-add"
(let () 
  (random-seed 2234)
  
;; s16vector of length 20 with random numbers from -50 to 49
(define src-buf (make-s16vector 20 0))
(define src-cpointer (s16vector->cpointer src-buf))
(for ([i (in-range (s16vector-length src-buf))])
  (s16vector-set! src-buf i (- (random 100) 50)))
  
(define tgt-buf (make-s16vector 100 0))
(define tgt-cpointer (s16vector->cpointer tgt-buf))

(s16buffer-add!/c (ptr-add tgt-cpointer (* 2 36)) src-cpointer 20)
(s16buffer-add!/c (ptr-add tgt-cpointer (* 2 46)) src-cpointer 15)

(check-equal?
 (for/and ([i (in-range 26)])
   (equal? (s16vector-ref tgt-buf i) 0))
 #t)

(check-equal?
 (for/and ([i (in-range 10)])
   (equal? (s16vector-ref tgt-buf (+ 46 i)) 
           (+ (s16vector-ref src-buf i)
              (s16vector-ref src-buf (+ i 10)))))
 #t)
  
  ;; now try mult-add
  
  (s16buffer-mult-add!/c (ptr-add tgt-cpointer (* 2 48)) src-cpointer 20 0.34)
  
  (for/and ([i (in-range 8)])
    (printf "i: ~s\n" i)
    (check-equal? (s16vector-ref tgt-buf (+ 48 i))
                  (inexact->exact
                   (+ (s16vector-ref src-buf (+ i 2))
                      (s16vector-ref src-buf (+ i 12))
                      (truncate (* 0.34 (s16vector-ref src-buf i)))))))
  )))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-test-suite))
