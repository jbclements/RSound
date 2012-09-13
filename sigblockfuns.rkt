#lang racket

;; functions like rsound, but that produce and combine signal/blocks



(module+ test
  (require rackunit)
  
  (define (test-signal i)
    (/ (modulo i 100) 100))
  
  (define test-sbu
    (signal->sig/block/unsafe test-signal 44100))
  
  ()
  )