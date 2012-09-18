#lang racket

(require racket/flonum
         "network.rkt")

(provide 
 (contract-out [build-wavetable (-> procedure? 
                                    flvector?)]
               [wavetable-build-sample-rate number?]))

;; build a wavetable for a periodic function with the 
;; contract (pitch sample-rate -> signal)
(define (build-wavetable fun)
  (define sigfun (network-init (fun 1 wavetable-build-sample-rate)))
  (let ([newvec (make-flvector wavetable-build-sample-rate)])
    (for ([i (in-range wavetable-build-sample-rate)])
      (flvector-set! newvec i (exact->inexact (sigfun))))
    newvec))

;; this is independent, but it should be nice and high to get 
;; good wavetables
(define wavetable-build-sample-rate 44100)
