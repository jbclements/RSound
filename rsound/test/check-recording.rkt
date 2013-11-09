#lang racket

(require "../rsound.rkt")

(printf "starting to record ...\n")
(define s (record-sound (* 44100 4)))
(printf "... done\n")

(sleep 4)

(play s)