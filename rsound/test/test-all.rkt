#lang racket

(printf "did you run with raco test?\n")

(module+ test
  (require "test-all-non-gui.rkt"
           "test-draw.rkt"
           "test-frequency-response.rkt"
           "test-playing.rkt"
           "test-playing-signal-block.rkt"
           "test-stream-play.rkt"
           "play-stream-test.rkt"
           "play-latency-test.rkt"
           "play-stream-latency-test.rkt"
           
           (prefix-in fft: "test-fft.rkt")
           ;; just load it:
           "../drum-samples.rkt"
           rackunit/text-ui)
  (run-tests
   fft:the-test-suite))
;; try manually: test-sequencer-playing.rkt