#lang racket

(require rackunit
         plot
         "../network.rkt"
         "../util.rkt"
         "../filter.rkt")

;; this is just a pair of helper functions for testing.

(provide plot-signal plot-signals)
;; for interactive testing, right now.

(define (plot-signal signal #:max [max-idx 30])
  (plot-signals (list signal) #:max max-idx))

;; plot the first few samples of a set of signals
(define (plot-signals signals #:max [max-idx 30])
  (define sigfuns (map network-init signals))
  (define datas
    (for/list ([sigfun sigfuns])
      (points
       (for/list ([i max-idx])
         (vector i (sigfun))))))
  (plot (apply mix datas)))
