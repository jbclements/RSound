#lang racket

(require "../rsound.rkt"
         "../network.rkt"
         "../util.rkt")

(printf "\nshort ding\n")
(play ding)
(sleep 2)

(printf "\n2-second tone @ 440\n")
(play (make-tone 440 0.1 88200))
(sleep 3)

(printf "\n2-second-tone interrupted by stop-playing.\n")
(play (make-tone 550 0.1 88200))
(sleep 1)
(stop)
(sleep 1)

(printf "\n2-second-tone interrupted by 1-second-tone.\n")
(play (make-tone 550 0.1 88200))
(sleep 1)
(play (make-tone 660 0.1 44100))
(sleep 2)

(printf "\nfast 5x interrupt\n")
(for ([i (in-range 5)])
  (play (make-tone (* 440 (expt 1.25 i)) 0.05 44100))
  (sleep 0.01))
(sleep 1)

(printf "\nmany short-lived streams\n")
(define tiny-tone (make-tone 550 0.05 10))
(for ([i (in-range 80)])
  (play tiny-tone)
  (sleep 0.005))
(play ding)
(sleep 1)

;; UNIX can't handle more than 30.
;; on Mac OS X, 300 is definitely starting to affect performance:
(printf "\nmany simultaneous streams\n")
(define quiet-tone (make-tone 480 0.001 44100))
(for ([i (in-range 30)])
  (play quiet-tone))
(sleep 1)
(printf "...stop\n")
(sleep 1)

(printf "\nplaying a signal\n")
(signal-play (fixed-inputs sine-wave 440))
(sleep 1)
(stop)
(sleep 1)





;; test errors during signal execution





