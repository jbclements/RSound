#lang racket/base

(require racket/contract
         "prefs.rkt"
         (only-in "rsound-commander.rkt" channels))

;; no contract check, keep it fast:
(provide frame->sample
         positive-integer?
         nonnegative-integer?)

(provide (contract-out [default-sample-rate (parameter/c positive?)]))

(define (positive-integer? n)
  (and (integer? n) (< 0 n)))

(define (nonnegative-integer? n)
  (and (integer? n) (<= 0 n)))


;; used for creating sounds; specifying the 
;; sample rate every time is too much of a pain
;; for students.
(define default-sample-rate (make-parameter (get-fr-pref)))

;; translate a frame number and a channel into a sample number
(define (frame->sample f left?)
  (+ (* f channels) (if left? 0 1)))

