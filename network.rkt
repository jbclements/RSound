#lang racket

(require (for-syntax syntax/parse))


(provide network
         prev
         signal?
         signal-*s
         signal-+s
         (struct-out network/s)
         (contract-out [network-init (-> network/c procedure?)])
         loop-ctr
         simple-ctr
         signal-samples
         signal-nth)

;; this representation can in principle handle multiple-out networks,
;; but the surface syntax will get yucky. For now, let's just do one
;; output....

;; a network is either a function or (make-network number number procedure)

;; ins and outs not currently used, could be used for static checking...
;; or for better error messages, at any rate.

(struct network/s (ins outs maker))

(define network/c (or/c network/s? procedure?))

;; given a network/c, initialize it and return a thunk that 
;; produces samples
(define (network-init signal)
  (cond [(network/s? signal) ((network/s-maker signal))]
        [(procedure? signal) signal]
        [else (raise-argument-error
               'signal-init
               "network or procedure" signal)]))


;; maker: (-> (arg ... -> result ...))

;; ah, fergeddit; I'm going with zero inits for prevs.


(define-syntax prev (syntax-rules ()))

;; this is the cleaned-up version. It insists on having a sequence of ids for the out-names
(define-syntax (network/inr stx)
  (define-syntax-class network-clause
    #:description "network/inr clause"
    (pattern ((out:id ...) (node:expr input:node-in ...) (init:expr ...))))
  (define-syntax-class node-in
    #:description "network/inr node input"
    #:literals (prev)
    (pattern (prev in-ref:id))
    (pattern in:expr))
  (syntax-parse stx
    [(_ (in:id ...)
        clause:network-clause ...+)
     (define num-ins (length (syntax->list #'(in ...))))
     (define num-clauses (length (syntax->list #'(clause ...))))
     (define lhses (syntax->list #'((clause.out ...) ...)))
     (define last-out 
       (syntax-parse (car (reverse lhses))
         [(out:id) #'out]
         [(out:id out2:id ...+) #'(values out out2 ...)]))
     (define lhses/flattened (syntax->list #'(clause.out ... ...)))
     (define (rewrite/l input-list)
       (map rewrite (syntax->list input-list)))
     ;; rewrite occurrences of "prev" into vector references
     (define (rewrite in)
       (syntax-parse in
         #:literals (prev)
         [(prev in:id) #`(vector-ref saves-vec #,(find-idx #'in))]
         [other:expr #'other]))
     ;; extract inits; in the case of multi-inits, this may involve
     ;; extra work.
     (define (extract-inits clause-info)
       (syntax-parse clause-info
         [((id:id ...) (init:expr ...))
          (define out-len (length (syntax->list #'(id ...))))
          (define init-len (length (syntax->list #'(init ...))))
          (cond [(= out-len init-len) #'(init ...)]
                [(and (< 1 out-len) (= init-len 0))
                 ;; auto-default:
                 (for/list ([i out-len]) #'0.0)]
                [else (error 'network
                             "expected init list of length ~a, got ~e"
                             out-len 
                             (map syntax->datum
                                  (syntax->list #'(init ...))))])]))
     (define (find-idx id)
       (let loop ([lhses lhses/flattened]
                  [i 0])
         (cond [(null? lhses) 
                (error 'network
                       "expected prev to name an output node, got: ~s"
                       id)]
               [(free-identifier=? id (car lhses))
                #`(quote #,i)]
               [else (loop (cdr lhses) (add1 i))])))
     (with-syntax ([(signal-proc ...)
                    (generate-temporaries #'(clause ...))]
                   [((arg ...) ...)
                    (map rewrite/l (syntax->list
                                    #'((clause.input ...) ...)))]
                   [(idx ...)
                    (for/list ([i (in-range (length lhses/flattened))])
                      #`(quote #,i))]
                   [(lhs ...) lhses/flattened]
                   [((init ...) ...) (map extract-inits
                                    (syntax->list
                                     #'(((clause.out ...)
                                         (clause.init ...))
                                        ...)))])
       (with-syntax 
           ([maker
             #`(lambda ()
                 (define saves-vec
                   (vector init ... ...))
                 (define signal-proc (network-init clause.node))
                 ...
                 (lambda (in ...)
                   (let*-values ([(clause.out ...) (signal-proc arg ...)]
                         ...)
                     (begin
                       (vector-set! saves-vec idx lhs)
                       ...)
                     #,last-out)))])
         #`(network/s (quote #,num-ins) 1 maker)))])
  )

(define-syntax (network stx)
  (define-syntax-class network-clause
    #:description "network clause"
    (pattern (out1:id (node:expr input:node-in ...) (~optional (~seq #:init init:expr) #:defaults ([init #'0]))))
    (pattern ((out*:id ...) (node:expr input:node-in ...)
                            (~optional (~seq #:init init*:expr ...)
                                       #:defaults ([(init* 1) null])))))
  (define-syntax-class node-in
    #:description "network node input"
    #:literals (prev)
    (pattern (prev in-ref:id))
    (pattern in:expr))
  (define (rewrite clause)
    (syntax-parse clause
      [(out1:id  (node:expr input:node-in ...)
                 (~optional (~seq #:init init:expr) #:defaults ([init #'0])))
       #'((out1) (node input ...) (init))]
      [((out*:id ...) (node:expr input:node-in ...) (~optional (~seq #:init (init:expr ...)) #:defaults ([(init 1) null])))
       #'((out* ...) (node input ...) (init ...))]))
  (syntax-parse stx
    [(_ (in:id ...)
        clause:network-clause ...+)
     (with-syntax ([(new-clause ...)
                    (map rewrite (syntax->list #'(clause ...)))])
       #'(network/inr (in ...)
                      new-clause ...))])
  )


;; a signal is a network with no inputs.
;; can this procedure be used as a signal? 
(define (signal? f)
  (or (and (network/s? f) (= (network/s-ins f) 0))
      (and (procedure? f) (procedure-arity-includes? f 0))))


;; multiply the signals together:
(define (signal-*s los)
  (unless (andmap signal? los)
    (raise-argument-error 'signal-*s "list of signals" 0 los))
  (network/s 0 1 (lambda ()
                   (define sigfuns (map network-init los))
                   (lambda ()
                     (for/product ([fun sigfuns]) (fun))))))

;; add the signals together:
(define (signal-+s los)
  (unless (andmap signal? los)
    (raise-argument-error 'signal-*s "list of signals" 0 los))
  (network/s 0 1 (lambda ()
                   (define sigfuns (map network-init los))
                   (lambda ()
                     (for/sum ([fun sigfuns]) (fun))))))

;; a simple signal that starts at 0 and increments by "skip"
;; until it passes "len", then jumps back to 0
(define (loop-ctr len skip)
  (define limit-val (- len skip))
  (define (increment p)
    (define next-p (+ p skip))
    (cond [(< next-p len) next-p]
          [else (- next-p len)]))
  (network ()
           (out (increment (prev out)) #:init (- skip))))

;; a signal that simply starts at "init"  and adds "skip"
;; each time
(define (simple-ctr init skip)
  (network ()
           (out (+ skip (prev out)) #:init (- init skip))))


(struct unforgeable/s ())
(define unforgeable (unforgeable/s))

;; a vector containing the first 'n' samples of a signal
(define (signal-samples signal n)
  (define sigfun (network-init signal))
  (for/vector ([i n]) (sigfun)))

;; determine the nth sample, by discarding the first n-1:
(define (signal-nth signal n)
  (define sigfun (network-init signal))
  (for ([i n]) (sigfun))
  (sigfun))

