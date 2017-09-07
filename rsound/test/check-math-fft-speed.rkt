#lang racket

(require math/array math/statistics math/utils)

(define num-timings 20)

(module+ main
  (max-math-threads 1)
  
  (define timings
    (for/list ([i num-timings])
      ;(for/list: : (Listof Real) ([i 20])
      ;(: a (Array Number))
      (define a (build-array #[32768]
                             (lambda (indexes)
                               (random))))
      
      (define-values (_ cpu-msec real-msec gc-msec)
        (time-apply (lambda () (array-fft a)) null))
      cpu-msec))
  
  (printf "mean time (n=~v) to perform array-fft on 32768 random samples: ~v msec\n"
          num-timings
          (exact->inexact (mean timings)))
  (printf "stddev (n=~v) of array-fft times: ~v msec\n"
          num-timings
          (stddev timings))
  
  ;; for non-typed racket:
  ;200.15
  ;10.446410866895864

  ;; on 2017-08-13, this looks faster:
  ;; 77 ms, stddev 8 ms.
  ;; either machine is faster (though this is home laptop), or
  ;; racket is quite a bit faster.
  
  
  
  
  #;(array-inverse-fft (array-fft (array #[0 1 0 -1 0 1 0 -1]))))