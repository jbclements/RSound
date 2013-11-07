#lang racket

(require plot
         rackunit
         math/array
         )

(provide the-test-suite)


(define 2*pi (* 2 pi))


(define data (build-array #[4096] (lambda (t) (+ (sin (* (vector-ref t 0) (/ 2*pi 512))) 0.0))))
(define data-points (for/list ((j (in-array data))
                               (i (in-naturals)))
                      (vector i j)))


(define the-test-suite
(test-suite "ffi"

;;; Data

(let ()
  (printf "sine wave with 8+ cycles\n")
  (display 
   (plot (points data-points #:sym 'dot #:color 'blue)
        #:title "Data"
        #:x-min -100
        #:x-max 5000
        #:y-min -1.2
        #:y-max 1.2)))

(printf "FFT of sine wave ~n")
(let ()
  (define data1 (time (array-fft data)))

(define data1-max (for/fold ((max-magnitude 0.0))
                            ((x (in-array data1)))
                    (max (magnitude x) max-magnitude)))
(define data1-points-forward (for/list ((j (in-array data1))
                                        (i (in-naturals)))
                               (vector i (magnitude j))))
(printf "8th dot should be higher\n")
(display
 (plot (points data1-points-forward #:sym 'dot #:color 'blue)
      #:title "Radix 2 Complex FFT (Forward)"
      #:x-min -10
      #:x-max 20
      #:y-min -200
      #:y-max (* 1.2 data1-max)))

  

  (define data2 (array-inverse-fft data1))
  #;(fft-complex-radix2-inverse data1)
(define data1-points-inverse (for/list ((j (in-array data2))
                                        (i (in-naturals)))
                               (vector i (real-part j))))
  (printf "should look like original:\n")
(display
 (plot (points data1-points-inverse #:sym 'dot #:color 'blue)
      #:title "Radix 2 Complex FFT (Inverse)"
      #:x-min -100
      #:x-max 5000
      #:y-min -1.2
      #:y-max 1.2)))


(let ()
  (define arr 
    (build-array #[4096] (lambda (i) (cos (* (vector-ref i 0) 2*pi 16/4096)))))
  (define fft-arr
    (time (array-fft arr)))
  (check-= (magnitude (array-ref fft-arr #(15))) 0.0 1e-9)
  (check-= (magnitude (array-ref fft-arr #(16))) 2048.0 1e-9)
  (check-= (magnitude (array-ref fft-arr #(17))) 0.0 1e-9))

;; check that FFT + FFT-inverse is (pretty close to) the identity
(let* ([arr (build-array #[4096]
                         (lambda (i) (random)))])
  (define round-trip (array-inverse-fft (array-fft arr)))
  (for ([i 4096])
    (check-= (real-part (array-ref arr #[234]))
             (real-part (array-ref round-trip #(234))) 1e-4)
    (check-= (imag-part (array-ref arr #[234]))
             (imag-part (array-ref round-trip #(234))) 1e-4)))

))


(module+ test
  (require rackunit/text-ui)
  (run-tests the-test-suite))
