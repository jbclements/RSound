#lang racket

(require (for-syntax syntax/parse)
         plot)

;; this representation can in principle handle multiple-out networks,
;; but the surface syntax will get yucky. For now, let's just do one
;; output....

;; a network is either a function or (make-network number number procedure)

;; ins and outs not currently used, could be used for static checking...
;; or for better error messages, at any rat.

(struct network/s (ins outs maker))

;; maker: (-> (arg ... -> result ...))

;; ah, fergeddit; I'm going with zero inits for prevs.


  (define-syntax prev (syntax-rules ()))

(define-syntax (network stx)
  (define-syntax-class network-clause
    #:description "network clause"
    (pattern (out:id (node:expr input:node-in ...))))
  (define-syntax-class node-in
    #:description "network node input"
    #:literals (prev)
    (pattern (prev in-ref:id))
    (pattern in:expr))
  #;(define (rewrite-prevs inss))
  (syntax-parse stx
    [(_ (in:id ...)
        clause:network-clause ...
        out:id)
     (define num-ins (length (syntax->list #'(in ...))))
     (define num-clauses (length (syntax->list #'(clause ...))))
     (define lhses (syntax->list #'(clause.out ...)))
     (define (rewrite/l input-list)
       (map rewrite (syntax->list input-list)))
     (define (rewrite in)
       (syntax-parse in
         [(prev in:id) #`(vector-ref saves-vec #,(find-idx #'in))]
         [other:expr #'other]))
     (define (find-idx id)
       (let loop ([lhses lhses]
                  [i 0])
         (cond [(null? lhses) 
                (error 'network
                       "expected prev to name an output node, got: ~s"
                       id)]
               [(free-identifier=? id (car lhses))
                #`(quote #,i)]
               [else (loop (cdr lhses) (add1 i))])))
     (with-syntax ([(signal-thunk ...)
                    (generate-temporaries #'(clause ...))]
                   [((arg ...) ...)
                    (map rewrite/l (syntax->list
                                    #'((clause.input ...) ...)))]
                   [(idx ...)
                    (for/list ([i (in-range num-clauses)])
                      #`(quote #,i))])
       (with-syntax 
           ([maker
             #`(lambda ()
                 (define saves-vec
                   (make-vector (quote #,num-clauses) 0.0))
                 (define signal-thunk
                   (let ([node-val clause.node])
                     (cond [(network/s? node-val)
                            ((network/s-maker node-val))]
                           [else node-val])))
                 ...
                 (lambda (in ...)
                   (let* ([clause.out (signal-thunk arg ...)]
                         ...)
                     (begin
                       (vector-set! saves-vec idx clause.out)
                       ...)
                     out)))])
         #`(network/s (quote #,num-ins) 1 maker)))]))


;; angles are expressed in 44100ths of a circle....

(define (wraparound angle)
  (cond [(< angle 0) (+ angle 44100)]
        [(< 44099 angle) (- angle 44100)]
        [else angle]))

(define (cyclic-angle incr angle)
  (wraparound (+ angle incr)))

(define srinv (/ 1.0 44100.0))
(define (lookup angle)
  ;; could just be a lookup....
  (sin (* 2.0 pi srinv angle)))

(define variable-pitch-oscillator
  (network (incr)
           [angle (cyclic-angle incr (prev angle))]
           [wave (lookup angle)]
           wave))

(define mynet
(network ()
         [wave (variable-pitch-oscillator 440)]
         [gain (* wave 0.1)]
         gain))

(define generator ((network/s-maker mynet)))
(plot 
 (points (for/list ([i (in-range 100)])
         (vector i (generator)))))
