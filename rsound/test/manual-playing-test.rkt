#lang racket

(require "../rsound.rkt"
         "../network.rkt"
         "../util.rkt"
         (only-in "../common.rkt" default-sample-rate))


(define (print-and-flush str)
  (printf "~a" str)
  (flush-output))

(module+ main
(print-and-flush "\nshort ding\n")
(play ding)
(sleep 2)

(print-and-flush "\n2-second tone @ 440\n")
(play (make-tone 440 0.1 88200))
(sleep 3)

  (print-and-flush "\n2-second tone @ 440, 48K frame rate")
  (parameterize ([default-sample-rate 48000])
    (play (make-tone 440 0.1 96000)))
  (sleep 3)

(print-and-flush "\n2-second-tone interrupted by stop-playing.\n")
(play (make-tone 550 0.1 88200))
(sleep 1)
(stop)
(sleep 1)

(print-and-flush "\n2-second-tone interrupted by 1-second-tone.\n")
(play (make-tone 550 0.1 88200))
(sleep 1)
(play (make-tone 660 0.1 44100))
(sleep 2)

(print-and-flush "\nfast 5x interrupt\n")
(for ([i (in-range 5)])
  (play (make-tone (* 440 (expt 1.25 i)) 0.05 44100))
  (sleep 0.01))
(sleep 1)

(print-and-flush "\nmany short-lived streams\n")
(define tiny-tone (make-tone 550 0.05 10))
(for ([i (in-range 80)])
  (play tiny-tone)
  (sleep 0.005))
(play ding)
(sleep 1)

;; UNIX can't handle more than 30.
;; on Mac OS X, 300 is definitely starting to affect performance:
(print-and-flush "\nmany simultaneous streams\n")
(define quiet-tone (make-tone 480 0.001 44100))
(for ([i (in-range 30)])
  (play quiet-tone))
(sleep 1)
(print-and-flush "...stop\n")
(sleep 1)

(print-and-flush "\nplaying a signal for 10 second\n")
(signal-play (const-network sine-wave 440))
(sleep 10)
(stop)
(sleep 1))





;; test errors during signal execution





