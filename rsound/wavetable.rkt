#lang racket/base

;; could be ... typed?

(require racket/flonum
         racket/contract
         "common.rkt"
         "network.rkt")

(provide 
 (contract-out [build-wavetable (-> procedure? 
                                    flvector?)]
               [wavetable-build-sample-rate number?]
               [make-table-based-wavefun
                (-> flvector? (-> number? number? signal?))]))

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
  (unless (<= (flvector-length vec) wavetable-build-sample-rate)
    (raise-argument-error 'make-table-based-wavefun
                          (format "wavetable vector of length ~s"
                                  wavetable-build-sample-rate)
                          0 vec))
  (define relative-pitch (* pitch (/ wavetable-build-sample-rate sample-rate)))
  (define skip-rate (inexact->exact (round relative-pitch)))
  (define index-net (loop-ctr wavetable-build-sample-rate skip-rate))
  (network ()
           (idx <= index-net)
           (out = (flvector-ref vec idx))))

;; if this is different from the generated samples, resampling
;; will occur. You can set it high, to get good resolution,
;; or you can just guess at the right resolution and it'll sound
;; bad if you miss.
(define wavetable-build-sample-rate (default-sample-rate))

(module+ test
  (require rackunit)
  (define wtvec (build-wavetable (lambda (pitch sample-rate) random)))
  (define wavefun ((make-table-based-wavefun wtvec) 33 22100))
  (check-equal? (signal-nth wavefun 0) (flvector-ref wtvec 0))
  (check-equal? (signal-nth wavefun 5) (flvector-ref wtvec (* 33 2 5)))
  
  )
