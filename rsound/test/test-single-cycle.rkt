#lang racket

(require "../rsound.rkt"
         "../single-cycle.rkt"
         rackunit)

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
                              "AKWF_273848/AKWF_27384720.wav
  system error: No such file or directory; errno=2")
                             (exn-message exn)))
 (lambda () (synth-note "main" 27384720 1 1)))

