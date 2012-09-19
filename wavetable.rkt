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

;; given a wavetable, make a wavetable lookup function
;; how much slower would it be with interpolation?
;; flvector -> number number -> signal
(define ((make-table-based-wavefun vec) pitch sample-rate)
  (define relative-pitch (* pitch (/ wavetable-build-sample-rate sample-rate)))
  (define skip-rate (inexact->exact (round relative-pitch)))
  (define index-net (loop-ctr wavetable-build-sample-rate skip-rate))
  (network ()
           (idx (index-net))
           (out (flvector-ref vec idx))))

;; this is independent, but it should be nice and high to get 
;; good wavetables
(define wavetable-build-sample-rate 44100)

(module+ test
  (require rackunit)
  ())
