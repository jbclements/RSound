#lang racket

(require "../rsound.rkt"
         "../util.rkt"
         "../stream-play.rkt"
         rackunit
         rackunit/text-ui)

(run-tests
(test-suite "stream-play"
(check-equal? (current-time/s) 0)
(play/s (silence 100))
(sleep 0.1)
(check-false (= (current-time/s) 0))

(printf "play a ding.\n")
(play/s ding)
(sleep 1)))