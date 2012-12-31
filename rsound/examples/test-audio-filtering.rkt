#lang racket

(require "../main.rkt"
         "../filter.rkt"
         rackunit
         racket/runtime-path
         2htdp/universe
         2htdp/image)

(define-runtime-path here ".")

(define speaking
  (rs-read (build-path here "speaking.wav")))

(define cutoff (box 0.5))


(define the-signal
  (lpf/dynamic (lambda (x) (unbox cutoff))
               (rsound->signal/left speaking)))

(define (draw-world dc)
  (empty-scene 500 200))

(define (update-mouse world x y event)
  (when (< 0 x 500)
    (define new-cutoff (exact->inexact (/ x 500)))
    (set-box! cutoff new-cutoff)
    new-cutoff))

(signal-play the-signal 44100.0)

(big-bang 0
          [to-draw draw-world]
          [on-mouse update-mouse])


#;signal
#;(lpf/dynamic ...)
#;(play speaking)
#;(sleep 1.0)
#;(play speaking)