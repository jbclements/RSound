#lang racket

;; run the test suite defined inside of sequencer.rkt
(require rackunit/text-ui
         (submod "../sequencer.rkt" the-test-suite))

(provide the-test-suite)

(module+ test
  (require rackunit/text-ui)
  (run-tests the-test-suite))


