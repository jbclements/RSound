#lang racket/base

(require (for-syntax syntax/parse
                     racket/base))

(provide define/argcheck)

;; a macro that inserts calls to raise-argument-error.
;; I keep getting bitten when I change the argument list
;; and not the error message.

;; contracts would be a better choice here... but you can't
;; use contracts with functions that will be used in teaching
;; languages.
(define-syntax (define/argcheck stx)
  (define-syntax-class argspec
    #:description "argument specification"
    (pattern (var:id test:expr description:expr)))
  (syntax-parse stx
    [(_ (fun-name:id arg:argspec ...) body:expr ...)
     (with-syntax ([quoted-fun-name
                    #`(quote fun-name)]
                   [(index ...)
                    (for/list ([i (in-naturals)] 
                               [j (syntax->list #'(arg ...))])
                      i)])
     #'(define (fun-name arg.var ...)
         (begin 
           (unless (arg.test arg.var)
             (raise-argument-error quoted-fun-name arg.description
                                   index arg.var ...))
           ...)
         body
         ...))]))

(module+ test
  (require rackunit)
(define/argcheck (f [x number? "number"] [y boolean? "boolean"])
  14
  (if y x 9))

(check-equal? (f 3 #t) 3)
(check-exn (lambda (exn)
             (regexp-match #px"expected: number" (exn-message exn)))
           (lambda () 
             (f #f #t)))
(check-exn (lambda (exn)
             (regexp-match #px"expected: boolean" (exn-message exn)))
           (lambda () 
             (f 7 8))))