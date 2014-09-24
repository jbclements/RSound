#lang racket/base

(require memoize)
;; this module figures out what number of cycles comes "close"
;; to being a whole number of frames.

(provide cycles-to-use)

;; experiments suggest that for all numbers between 1.0 and 22050.0,
;; it's pretty easy to find a number of cycles whose cumulative period
;; when rounded to an integer number of frames is off by less than
;; this fraction of a cycle.
(define thresh 4e-5)

(define/memo (cycles-to-use freq sample-rate)
  (when (not (and (real? freq) (< 0 freq)))
    (raise-type-error 'cycles-to-use "positive real number" 0
                      freq sample-rate))
  (define period (/ sample-rate freq))
  (define fail (add1 (ceiling freq)))
  (let loop ([i 1])
    (cond [(< fail i)
           (error 'cycle-search
                  "couldn't find good number of cycles for frequency ~s"
                  freq)]
          [(< (/ (abs (- (round (* i period)) (* i period)))
                 period)
              thresh)
           i]
          [else (loop (add1 i))])))