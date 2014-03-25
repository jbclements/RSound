#lang racket

(require "../draw.rkt"
         "../rsound.rkt"
         "../util.rkt")

;; is the scrolling adequate?

;; make a long sound of noise
(define n (noise (* 10 44100)))

(rs-draw n)

