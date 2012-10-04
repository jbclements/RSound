#lang racket

(require racket/flonum
         "network.rkt")

(provide 
 (contract-out [build-wavetable (-> procedure? 
                                    flvector?)]
               [make-table-based-wavefun
                (-> flvector? (-> number? number? signal?))]))

(define SR (inexact->exact (default-sample-rate)))
(unless (integer? SR)
  (raise-argument-error 'build-wavetable "integer" 1 fun SR))

;; build a wavetable for a periodic function with the 
;; contract (pitch sample-rate -> signal)
(define (build-wavetable fun SR)

  (define srint (inexact->exact SR))
  (define sigfun (network-init (fun 1)))
  (let ([newvec (make-flvector srint)])
    (for ([i (in-range srint)])
      (flvector-set! newvec i (exact->inexact (sigfun))))
    newvec))

;; given a wavetable, make a wavetable lookup function
;; how much slower would it be with interpolation?
;; flvector -> number number -> signal
(define ((make-table-based-wavefun vec) pitch)
  (define skip-rate (inexact->exact (round pitch)))
  (define index-net (loop-ctr wavetable-build-sample-rate skip-rate))
  (network ()
           (idx (index-net))
           (out (flvector-ref vec idx))))

(module+ test
  (require rackunit)
  (define wtvec (build-wavetable (lambda (pitch sample-rate) random)))
  (define wavefun ((make-table-based-wavefun wtvec) 33 22100))
  (check-equal? (signal-nth wavefun 0) (flvector-ref wtvec 0))
  (check-equal? (signal-nth wavefun 5) (flvector-ref wtvec (* 33 2 5)))
  
  )
