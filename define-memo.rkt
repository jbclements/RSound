#lang racket/base

(require (for-syntax syntax/parse)
         (for-syntax racket/base))

(provide define/memo)

;; 1-4-ARG MEMOIZATION
;; (I would use Dave Herman's planet package, but the dependencies
;; are deadly)
;; no time right now to abstract this over # of args
(define-syntax (define/memo stx)
  (syntax-parse stx
    [(_ (name:id arg1:id arg2:id arg3:id arg4:id)
        body:expr ...)
     #`(define name
         (let ()
           (define the-hash (make-hash))
           (lambda (arg1 arg2 arg3 arg4)
             (define hash-key (list arg1 arg2 arg3 arg4))
             (define hash-lookup (hash-ref the-hash hash-key #f))
             (cond [(not hash-lookup)
                    (define result (let () body ...))
                    (hash-set! the-hash hash-key result)
                    result]
                   [else hash-lookup]))))]
    [(_ (name:id arg1:id arg2:id arg3:id)
        body:expr ...)
     #`(define name
         (let ()
           (define the-hash (make-hash))
           (lambda (arg1 arg2 arg3)
             (define hash-key (list arg1 arg2 arg3))
             (define hash-lookup (hash-ref the-hash hash-key #f))
             (cond [(not hash-lookup)
                    (define result (let () body ...))
                    (hash-set! the-hash hash-key result)
                    result]
                   [else hash-lookup]))))]
    [(_ (name:id arg1:id arg2:id)
        body:expr ...)
     #`(define name
         (let ()
           (define the-hash (make-hash))
           (lambda (arg1 arg2)
             (define hash-key (list arg1 arg2))
             (define hash-lookup (hash-ref the-hash hash-key #f))
             (cond [(not hash-lookup)
                    (define result (let () body ...))
                    (hash-set! the-hash hash-key result)
                    result]
                   [else hash-lookup]))))]
    [(_ (name:id arg1:id)
        body:expr ...)
     #`(define name
         (let ()
           (define the-hash (make-hash))
           (lambda (arg1)
             (define hash-key arg1)
             (define hash-lookup (hash-ref the-hash hash-key #f))
             (cond [(not hash-lookup)
                    (define result (let () body ...))
                    (hash-set! the-hash hash-key result)
                    result]
                   [else hash-lookup]))))]))