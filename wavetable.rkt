#lang racket

(require racket/flonum)

(provide 
 (contract-out [build-wavetable (-> procedure? 
                                    flvector?)]
               [wavetable-build-sample-rate number?]))

;; given a length and a function, build the corresponding flvector
(define (build-flvector len fun)
  (let ([newvec (make-flvector len)])
    (for ([i (in-range len)])
      (flvector-set! newvec i (exact->inexact (fun i))))
    newvec))

;; build a wavetable for a periodic function with the 
;; contract (pitch sample-rate -> sample -> value)
(define (build-wavetable fun)
  (build-flvector wavetable-build-sample-rate
                  (fun 1 wavetable-build-sample-rate)))

;; this is independent, but it should be nice and high to get 
;; good wavetables
(define wavetable-build-sample-rate 44100)
