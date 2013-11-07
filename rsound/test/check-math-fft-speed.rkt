#lang racket

(require math/array math/statistics math/utils)

(max-math-threads 1)

(define timings
  (for/list ([i 20])
    ;(for/list: : (Listof Real) ([i 20])
    ;(: a (Array Number))
    (define a (build-array #[32768]
                           (lambda (indexes)
                             (random))))
    
    (define-values (_ cpu-msec real-msec gc-msec)
      (time-apply (lambda () (array-fft a)) null))
    cpu-msec))

(exact->inexact (mean timings))
(stddev timings)

;; for non-typed racket:
;200.15
;10.446410866895864




#;(array-inverse-fft (array-fft (array #[0 1 0 -1 0 1 0 -1])))