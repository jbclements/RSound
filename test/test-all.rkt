#lang racket

(require "test-all-non-gui.rkt"
         "test-draw.rkt"
         "test-frequency-response.rkt"
         "test-playing.rkt"
         "test-playing-signal-block.rkt"
         "test-stream-play.rkt"
         "play-stream-test.rkt"
         "play-latency-test.rkt"
         "play-stream-latency-test.rkt"
         ;; just load it:
         "../drum-samples.rkt")

;; try manually: test-sequencer-playing.rkt