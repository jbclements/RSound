#lang racket 

;; these tests don't produce any sound or open any boxes; consequently, they can
;; be run non-interactively.

(require (prefix-in rw: "test-read-wav.rkt")
         (prefix-in ww: "test-write-wav.rkt")
         (prefix-in r: "test-rsound.rkt")
         (prefix-in u: "test-util.rkt")
         (prefix-in f: "test-filter.rkt")
         (prefix-in n: "test-network.rkt")
         (prefix-in s16: "test-s16vector-add.rkt")
         (prefix-in sq: "test-sequencer.rkt")
         (prefix-in mt: "test-make-tone.rkt")
         (prefix-in pu: "test-paste-util.rkt")
         (prefix-in sc: "test-single-cycle.rkt")
         (prefix-in rc: "test-rsound-commander.rkt")
         (prefix-in ev: "test-envelope.rkt")
         (prefix-in rvb: "test-reverb.rkt")
         rackunit/text-ui
         rackunit)

(run-tests 
 (test-suite 
  "non-gui-tests"
  rw:the-test-suite
  ww:the-test-suite
  r:the-test-suite
  u:the-test-suite
  f:the-test-suite
  n:the-test-suite
  s16:the-test-suite
  sq:the-test-suite
  mt:the-test-suite
  pu:the-test-suite
  sc:the-test-suite
  rc:the-test-suite
  ev:the-test-suite
  rvb:the-test-suite
  ))