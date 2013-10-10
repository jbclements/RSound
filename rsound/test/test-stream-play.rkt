#lang racket

(require "../main.rkt"
         rackunit
         rackunit/text-ui)

(run-tests
(test-suite 
 "stream-play"
 
(let ()
 (define ps (make-pstream))
 (define t (make-tone 441 0.2 22000))
 (sleep 0.5)
 (define now_0 (pstream-current-frame ps))
 (check-true (< 0 now_0))

 (printf "one ding.\n")
 (pstream-play ps ding)
 (sleep 1)

 (define now_1 (pstream-current-frame ps))
 (check-true (< (+ 22050 now_0) now_1))
 
 (printf "tone should be uninterrupted, 2 seconds long.\n")
 (pstream-queue ps t now_1)
 (pstream-queue ps t (+ now_1 22000))
 (pstream-queue ps t (+ now_1 44000))
 (pstream-queue ps t (+ now_1 66000))

 (sleep 4.0))
))