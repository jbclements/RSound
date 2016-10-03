#lang racket

(require "../main.rkt"
         "../filter.rkt"
         rackunit
         racket/runtime-path
         2htdp/universe
         2htdp/image)

(define-runtime-path here ".")


;; terrible example. use some quiet white noise.
#;(define speaking
  (rs-read (build-path here "speaking.wav")))

(define noise
 (network ()
          [noise = (- (random) 0.5)]
          [out = (* noise 0.05)]))

(define cutoff (box 0.5))


(define the-signal
  (network ()
           [c = (unbox cutoff)]
           [audio <= noise]
           [out <= lpf/dynamic c audio]))

(define (draw-world dc)
  (empty-scene 500 200))

(define (update-mouse world x y event)
  (when (< 0 x 500)
    (define new-cutoff (exact->inexact (/ x 500)))
    (set-box! cutoff new-cutoff)
    new-cutoff))


(module+ main

(signal-play the-signal)

(big-bang 0
          [to-draw draw-world]
          [on-mouse update-mouse]))


