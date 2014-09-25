#lang racket/base

(provide define/memo)

;; Memoization for function with arbitrary # args
(define-syntax-rule (define/memo (f x ...) e ...)
  (define f
    (let ([m (make-hash)])
      (λ (x ...) (hash-ref! m (list x ...) (λ () e ...))))))
