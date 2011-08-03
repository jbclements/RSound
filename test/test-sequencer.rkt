#lang racket

(require rackunit
         rackunit/text-ui)

;; 1 second of static at -20 dB
(define test-static
  (mono-signal->rsound 
   44100
   44100
   (lambda (t) (* 0.1 (random)))))

(run-tests
(test-suite "sequencer"
            
(let ()

  (define entry-heap (make-unplayed-heap))
  
  (heap-add-all! (list (entry )))
  
  )))


