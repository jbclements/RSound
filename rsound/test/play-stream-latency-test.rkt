#lang racket

(require "../main.rkt")

;; this is a manual test. Check the time gap between the 
;; printing of the text and the playing of the sound.
(stop)
(printf "check for gap between printout and sound (7 dings).\n")
(define ps (make-pstream))
(for ([i (in-range 7)])
  (printf "ding\n")
  (pstream-play ps ding)
  (sleep 1.5))

