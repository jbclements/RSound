#lang racket 

;; this list is partially converted to the "test suite" style... I should finish the conversion.
(require (prefix-in rw: "test-read-wav.rkt")
         (prefix-in ww: "test-write-wav.rkt")
         (prefix-in r: "test-rsound.rkt")
         (prefix-in u: "test-util.rkt")
         (prefix-in f: "test-filter.rkt")
         (prefix-in n: "test-network.rkt")
         "test-s16vector-add.rkt"
         "test-sequencer.rkt"
         "test-make-tone.rkt"
         "test-paste-util.rkt"
         "test-single-cycle.rkt"
         rackunit/text-ui
         rackunit)

(run-tests 
 (test-suite "non-gui-tests"
             rw:the-test-suite
             ww:the-test-suite
             r:the-test-suite
             u:the-test-suite
             f:the-test-suite
             n:the-test-suite
             ))