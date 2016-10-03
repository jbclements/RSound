#lang typed/racket

(require racket/flonum)
(require/typed "network.rkt"
               [#:struct network/s ([ins : Index] 
                                   [outs : Index]
                                   [maker : (-> (Float -> Float))])])
(require/typed "common.rkt"
               [default-sample-rate (Parameterof Real)])

;; this file provides the "reverb" network
(provide reverb)

;; constants here from Moorer 1979:
;; uses a base delay of 100ms
(define base-delay 100/1000) ;; in seconds
(define base-delay-frames (* base-delay (exact->inexact (default-sample-rate))))
(define d1 (inexact->exact (round base-delay-frames)))
(define d2 (inexact->exact (round (* 1.1 d1))))
(define d3 (inexact->exact (round (* 1.2 d1))))
(define d4 (inexact->exact (round (* 1.3 d1))))
(define d5 (inexact->exact (round (* 1.4 d1))))
(define d6 (inexact->exact (round (* 1.5 d1))))
(define g11 0.46)
(define g12 0.48)
(define g13 0.50)
(define g14 0.52)
(define g15 0.53)
(define g16 0.55)
(define g-konst 0.7) ;; (closer to 1.0 gives more ring)
        
(define g21 (* (- 1.0 g11) g-konst))
(define g22 (* (- 1.0 g12) g-konst))
(define g23 (* (- 1.0 g13) g-konst))
(define g24 (* (- 1.0 g14) g-konst))
(define g25 (* (- 1.0 g15) g-konst))
(define g26 (* (- 1.0 g16) g-konst))
        

;; this function produces a function from floats to floats.
(: starter (-> (Float -> Float)))
(define starter
      (lambda ()
        (define v1 (make-flvector (inexact->exact (round d1)) 0.0))
        (define v2 (make-flvector (inexact->exact (round d2)) 0.0))
        (define v3 (make-flvector (inexact->exact (round d3)) 0.0))
        (define v4 (make-flvector (inexact->exact (round d4)) 0.0))
        (define v5 (make-flvector (inexact->exact (round d5)) 0.0))
        (define v6 (make-flvector (inexact->exact (round d6)) 0.0))
        
        ;; the lpf midpoints
        (define mvec (make-flvector 6 0.0))
        (define m1 0.0)
        ;(define m2 0.0)
        ;(define m3 0.0)
        ;(define m4 0.0)
        ;(define m5 0.0)
        ;(define m6 0.0)
        
        ;; the tap counters
        (define c1 0)
        (define c2 0)
        (define c3 0)
        (define c4 0)
        (define c5 0)
        (define c6 0)

        ;; the main feedback buffers
        (lambda (in)

          ;; the first comb filter
          (define delayed1 (flvector-ref v1 c1))
          (define midnode1 (fl+ delayed1 (fl* g11 m1)))
          (define out1 (fl+ (fl* g21 midnode1) in))
          (flvector-set! v1 c1 out1)
          (define next-c1 (add1 c1))
          (set! c1 (cond [(<= d1 next-c1) 0]
                         [else next-c1]))
          (set! m1 midnode1)
  
          ;; the second comb filter
          (define delayed2 (flvector-ref v2 c2))
          (define midnode2 (fl+ delayed2 (fl* g12 (flvector-ref mvec 1))))
          (define out2 (fl+ (fl* g22 midnode2) in))
          (flvector-set! v2 c2 out2)
          (define next-c2 (add1 c2))
          (set! c2 (cond [(<= d2 next-c2) 0]
                         [else next-c2]))
          (flvector-set! mvec 1 midnode2)

          ;; the third comb filter (and so on)
          (define delayed3 (flvector-ref v3 c3))
          (define midnode3 (fl+ delayed3 (fl* g13 (flvector-ref mvec 2))))
          (define out3 (fl+ (fl* g23 midnode3) in))
          (flvector-set! v3 c3 out3)
          (define next-c3 (add1 c3))
          (set! c3 (cond [(<= d3 next-c3) 0]
                         [else next-c3]))
          (flvector-set! mvec 2 midnode3)

          (define delayed4 (flvector-ref v4 c4))
          (define midnode4 (fl+ delayed4 (fl* g14 (flvector-ref mvec 3))))
          (define out4 (fl+ (fl* g24 midnode4) in))
          (flvector-set! v4 c4 out4)
          (define next-c4 (add1 c4))
          (set! c4 (cond [(<= d4 next-c4) 0]
                         [else next-c4]))
          (flvector-set! mvec 3 midnode4)
          (define delayed5 (flvector-ref v5 c5))
          (define midnode5 (fl+ delayed5 (fl* g15 (flvector-ref mvec 4))))
          (define out5 (fl+ (fl* g25 midnode5) in))
          (flvector-set! v5 c5 out5)
          (define next-c5 (add1 c5))
          (set! c5 (cond [(<= d5 next-c5) 0]
                         [else next-c5]))
          (flvector-set! mvec 4 midnode5)
          (define delayed6 (flvector-ref v6 c6))
          (define midnode6 (fl+ delayed6 (fl* g16 (flvector-ref mvec 5))))
          (define out6 (fl+ (fl* g26 midnode6) in))
          (flvector-set! v6 c6 out6)
          (define next-c6 (add1 c6))
          (set! c6 (cond [(<= d6 next-c6) 0]
                         [else next-c6]))
          (flvector-set! mvec 5 midnode6)

          (/ (+ out1 out2 out3 out4 out5 out6) 6.0))))


(: reverb network/s)
(define reverb
  (network/s 1 1 starter))

