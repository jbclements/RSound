#lang racket

(require "../rsound.rkt"
         "../util.rkt")

(printf "\nshort ding\n")
(rsound-play ding)
(sleep 2)

(printf "\n2-second tone @ 440\n")
(rsound-play (make-tone 440 0.5 88200 44100))
(sleep 3)

(printf "\n2-second-tone interrupted by stop-playing.\n")
(rsound-play (make-tone 550 0.5 88200 44100))
(sleep 1)
(stop-playing)
(sleep 1)

(printf "\n2-second-tone interrupted by 1-second-tone.\n")
(rsound-play (make-tone 550 0.5 88200 44100))
(sleep 1)
(rsound-play (make-tone 660 0.5 44100 44100))
(sleep 2)

(printf "\nfast 5x interrupt\n")
(for ([i (in-range 5)])
  (rsound-play (make-tone (* 440 (expt 1.25 i)) 0.5 44100 44100))
  (sleep 0.01))
(sleep 1)

(printf "\nplaying a signal\n")
(signal-play (sine-wave 440 44100) 44100)
(sleep 1)
(stop-playing)
(sleep 1)

;; test errors during signal execution





