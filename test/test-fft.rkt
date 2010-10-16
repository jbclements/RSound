#lang racket

(require #;plot
         rackunit
         "../fft.rkt")

;;; Data

(define 2*pi (* 2 pi))
#|
(printf "Data~n")

(define data (build-vector 4096 (lambda (t) (+ (sin (* t (/ 2*pi 500))) 0.0))))
(define data-points (for/list ((j (in-vector data))
                               (i (in-naturals)))
                      (vector i j)))
(plot (points data-points #:sym 'dot #:color 'blue)
      #:title "Data"
      #:x-min -100
      #:x-max 5000
      #:y-min -1.2
      #:y-max 1.2)

;;; Radix 2 Complex FFT (Decimation in Time)

(printf "Radix 2 Complex FFT - Decimation in Time~n")

(define data1 (vector-copy data))
(time (fft-complex-radix2-forward data1))
(define data1-max (for/fold ((max-magnitude 0.0))
                            ((x (in-vector data1)))
                    (max (magnitude x) max-magnitude)))
(define data1-points-forward (for/list ((j (in-vector data1))
                                        (i (in-naturals)))
                               (vector i (magnitude j))))
(plot (points data1-points-forward #:sym 'dot #:color 'blue)
      #:title "Radix 2 Complex FFT (Forward)"
      #:x-min -100
      #:x-max 5000
      #:y-max (* 1.2 data1-max))

(fft-complex-radix2-inverse data1)
(define data1-points-inverse (for/list ((j (in-vector data1))
                                        (i (in-naturals)))
                               (vector i (real-part j))))
(plot (points data1-points-inverse #:sym 'dot #:color 'blue)
      #:title "Radix 2 Complex FFT (Inverse)"
      #:x-min -100
      #:x-max 5000
      #:y-min -1.2
      #:y-max 1.2)

;;; Complex FFT

(printf "Complex FFT~n")

(define data0 (vector-copy data))
(time (fft-complex-forward data0))
(printf "Running it again...\n")
(define data4 (vector-copy data))
(time (fft-complex-forward data4))
(define data0-max (for/fold ((max-magnitude 0.0))
                            ((x (in-vector data0)))
                    (max (magnitude x) max-magnitude)))
(define data0-points-forward (for/list ((j (in-vector data0))
                                        (i (in-naturals)))
                               (vector i (magnitude j))))
(plot (points data0-points-forward #:sym 'dot #:color 'blue)
      #:title "Complex FFT (Forward)"
      #:x-min -100
      #:x-max 5000
      #:y-max (* 1.2 data0-max))

(fft-complex-inverse data0)
(define data0-points-inverse (for/list ((j (in-vector data0))
                                        (i (in-naturals)))
                               (vector i (real-part j))))
(plot (points data0-points-inverse #:sym 'dot #:color 'blue)
      #:title "Complex FFT (Inverse)"
      #:x-min -100
      #:x-max 5000
      #:y-min -1.2
      #:y-max 1.2)



;;; Radix 2 Complex FFT (Decimation in Frequency)

(printf "Radix 2 Complex FFT - Decimation in Frequency~n")

(define data2 (vector-copy data))
(time (fft-complex-radix2-dif-forward data2))
(define data2-max (for/fold ((max-magnitude 0.0))
                            ((x (in-vector data2)))
                    (max (magnitude x) max-magnitude)))
(define data2-points-forward (for/list ((j (in-vector data2))
                                        (i (in-naturals)))
                               (vector i (magnitude j))))
(plot (points data2-points-forward #:sym 'dot #:color 'blue)
      #:title "Radix 2 Complex FFT (Forward)"
      #:x-min -100
      #:x-max 5000
      #:y-max (* 1.2 data2-max))

(fft-complex-radix2-dif-inverse data2)
(define data2-points-inverse (for/list ((j (in-vector data2))
                                        (i (in-naturals)))
                               (vector i (real-part j))))
(plot (points data2-points-inverse #:sym 'dot #:color 'blue)
      #:title "Radix 2 Complex DFT (Inverse)"
      #:x-min -100
      #:x-max 5000
      #:y-min -1.2
      #:y-max 1.2)
t
;;; Multi-Radix Complex DFT

(printf "Multi-Radix Complex DFT~n")

(define dft (time (dft-complex-forward data)))
(define dft-max (for/fold ((max-magnitude 0.0))
                          ((x (in-vector dft)))
                  (max (magnitude x) max-magnitude)))
(define dft-points (for/list ((j (in-vector dft))
                              (i (in-naturals)))
                     (vector i (magnitude j))))
(plot (points dft-points #:sym 'dot #:color 'blue)
      #:title "Multi-Radix Complex DFT (Forward)"
      #:x-min -100
      #:x-max 5000
      #:y-max (* 1.2 dft-max))

(define dft-inv (dft-complex-inverse dft))
(define dft-inv-points (for/list ((j (in-vector data))
                                  (i (in-naturals)))
                         (vector i (real-part j))))
(plot (points dft-inv-points #:sym 'dot #:color 'blue)
      #:title "Multi-Radix Complex DFT (Inverse)"
      #:x-min -100
      #:x-max 5000
      #:y-min -1.2
      #:y-max 1.2)
|#


(let* ([vect (build-vector 4800 (lambda (i) (cos (* i 2*pi 147/44100))))])
  (time (fft-complex-forward vect))
  (check-= (magnitude (vector-ref vect 15)) 0.0 1e-3)
  (check-= (magnitude (vector-ref vect 16)) 2400.0 1e-3)
  (check-= (magnitude (vector-ref vect 17)) 0.0 1e-3))

;; check that FFT + FFT-inverse is (pretty close to) the identity
(let* ([vect (build-vector 4096 (lambda (i) (random)))]
       [v2 (vector-copy vect)])
  (fft-complex-radix2-forward v2)
  (fft-complex-radix2-inverse v2)
  (check-= (vector-ref vect 234) (real-part (vector-ref v2 234)) 1e-4))


(check-not-exn (lambda () (fft-complex-radix2-inverse (build-vector 16 (lambda (i) 0)))))
(check-not-exn (lambda () (fft-complex-radix2-forward (build-vector 16 (lambda (i) 0)))))
(check-not-exn (lambda () (fft-complex-forward (build-vector 16 (lambda (i) 0)))))


