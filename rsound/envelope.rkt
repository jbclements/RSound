#lang racket/base

(require "rsound.rkt"
         "util.rkt")

(provide adsr
         adsr/exp)

(define (straight-line frames a b)
  (define (sig f) (+ a (* f (/ (- b a) frames))))
  (signal->rsound frames (indexed-signal sig)))

(define (exp-line frames a b)
  (define a-log (log (max a 1e-2)))
  (define b-log (log (max b 1e-2)))
  (define (sig f) (exp (+ a-log (* f (/ (- b-log a-log) frames)))))
  (signal->rsound frames (indexed-signal sig)))



;; if total is less than 
(define (((adsr/helper line-maker) a ah d dh r) total)
  (clip (rs-append* 
         (list
          (exp-line a 0 ah)
          (exp-line d ah dh)
          (exp-line (max 0 (- total a d r)) dh dh)
          (exp-line r dh 0)))
        0 total))

(define adsr (adsr/helper straight-line))
(define adsr/exp (adsr/helper exp-line))


(module+ test
  (require rackunit)
  (check-= (rs-ith/left (straight-line 30 0.2 0.3) 15) 0.25 1e-3))