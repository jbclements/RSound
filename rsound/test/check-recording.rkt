#lang racket


(module+ main
(require rsound)

(printf "starting to record ...\n")
(define s (record-sound (* (default-sample-rate) 4)))
(printf "... done\n")

(sleep 4)

(play s))