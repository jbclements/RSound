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

#|
(plot-signals (list (raw-sine-wave 882 44100)
                    (sine-wave 882)))


(plot-signal (raw-sine-wave 882 44100))
(plot-signal (sine-wave 882))

(plot-signal 
   (network ()
            [a ((simple-ctr 1/20 0))]
            [control ((dc-signal 0.1))]
            [out (lpf/dynamic control a)])
   #:max 400)
|#