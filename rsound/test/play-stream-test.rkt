#lang racket

(require "../main.rkt")

(stop)
(printf "known bug: I expect this to be silent. :(\n")
(printf "tone should be uninterrupted, 2 seconds long.\n")
(define t (make-tone 441 0.2 22000))
(play/s/f t 0)
(play/s/f t 22000)
(play/s/f t 44000)
(play/s/f t 66000)

(sleep 3.0)

