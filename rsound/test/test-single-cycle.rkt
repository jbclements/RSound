#lang racket

(require "../rsound.rkt"
         "../single-cycle.rkt"
         rackunit)

(provide the-test-suite)
(define the-test-suite
(test-suite
 "single-cycle"
 (let ()
(check-not-exn (lambda () (synth-note "vgame" 132 0 44100)))
(check-not-exn (lambda () (synth-note "main" 37 10 44100)))
(check-exn 
 (lambda (exn) (regexp-match #px"string that is 'main'" 
                             (exn-message exn)))
 (lambda () (synth-note 1 1 1 1)))
(check-exn 
 (lambda (exn) (regexp-match #px"expected: exact nonnegative integer" 
                             (exn-message exn)))
 (lambda () (synth-note "main" #f 1 1)))
(check-exn
 (lambda (exn) (regexp-match (regexp-quote
                              "expected: name of existing file")
                             (exn-message exn)))
 (lambda () (synth-note "main" 27384720 1 1))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-test-suite))

