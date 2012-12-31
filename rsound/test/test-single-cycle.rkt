#lang racket

(require "../rsound.rkt"
         "../single-cycle.rkt"
         rackunit)

(check-not-exn (lambda () (synth-note "vgame" 132 0 44100)))
(check-not-exn (lambda () (synth-note "main" 37 10 44100)))

