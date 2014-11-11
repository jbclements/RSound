#lang racket

(require "../rsound.rkt"
         "../util.rkt"
         "../draw.rkt"
         ffi/vector
         rackunit
         math/array)




(check-equal? (abs-max-from (lambda (x) (- 5 (* x x))) 4) 5)
(check-equal? (abs-max-from (lambda (x) (- 5 (* x x))) 5) 11)


(check-equal? (interpolate (lambda (x) x) 0.7) 0.7)
(check-equal? (interpolate (lambda (x) (+ 2/3 (- (* x x)))) 10) (- 2/3 100))

(check-equal? (call-with-values (lambda () (rasterize-column (lambda (x) (+ 2/3 (- (* x x)))) -3 10)) list)
              (list (- 2/3 100) 2/3))

(check-equal? (call-with-values (lambda () (rasterize-column (lambda (x) (+ 2/3 (- (* x x)))) -3.5 1)) list)
              (list (- 2/3 12.5) (exact->inexact 2/3)))


;; these don't really lend themselves to testing; I suppose if I separated
;; the rendering from the drawing...

(define rsound-4samp (rsound 
                      (s16vector 0 0 50 50 -50 -50 0 0)
                      0
                      4
                      44100))

(rs-draw rsound-4samp #:title "4 samples")

(define rsound-800samp (rsound
                        (apply s16vector (build-list 1600 (lambda (i) (inexact->exact (round (* s16max (sin (* 2 pi 3/800 i))))))))
                        0
                        800
                        44100))

(rs-draw rsound-800samp #:width 800 #:title "800 samples")

(rs-draw rsound-800samp #:width 400 #:title "800 samples at width 400")

(rs-draw rsound-800samp #:width 20 #:title "800 samples at width 20")

#;(define rsound-longer (read-rsound/clip "/tmp/gmafh.wav" (* 44100 60) (* 44100 70)))

#;(play-rsound rsound-longer)

#;(rs-draw rsound-longer #:width 800)

;; there should be no gap in the waveform:
(rs-draw (signal->rsound 300
                         (indexed-signal
                          (lambda (i) (* 1.5 (sin (* twopi 147/44100 i))))))
               #:title "no gap in waveform")


;; drawing non-sounds

(let ([lvec (vector 3 4123 2 4 3 2 2 2 4 2 3 4 1 2 2 23  4 3 3)]
      [rvec (vector 3 23 298 4 2 23 1 2 3 4 9 8 2 24 2 79 1 23 9)])
  (vector-display-frame
   "non-rsound vectors"
   (lambda (i) (vector-ref lvec i))
   (lambda (i) (vector-ref rvec i))
   19
   800
   200
   0
   19
   #f))

;; phase:

(check-= (phase 1+i) (* 1/4 pi) 1e-4)
(check-= (phase -1+i) (* 3/4 pi) 1e-4)
(check-= (phase -1-i) (* -3/4 pi) 1e-4)
(check-= (phase 1-i) (* -1/4 pi) 1e-4)

(let ([lvec (vector 10 0 5 +5i -5 -5i)]
      [rvec (vector 3+4i 3-4i -3-4i -4+3i 10 0)])
  (vector-pair-draw/magnitude lvec rvec #:title "vector-pair-draw/magnitude"))

;; draw-ffts

#;(let ([v (build-vector 16 (lambda (i) (* s16max i)))])
  (ffts-draw (list v) (list v) 16))

(let ([v1 (build-array (vector 16) (lambda (idxes) (* s16max (vector-ref idxes 0))))]
      [v2 (build-array (vector 16) (lambda (idxes) 0))])
  (ffts-draw (list v1 v2) (list v2 v1) 2 10))

(let ([v1 (build-array (vector 16) (lambda (idxes) (* s16max (vector-ref idxes 0))))]
      [v2 (build-array (vector 16) (lambda (idxes) 0))])
  (ffts-draw (list v1 v2) (list v2 v1) 2 4))


#;(let ([v (build-vector 128 (lambda (i) (* s16max (* 0.5 (+ (sin (* i 4/128 twopi))
                                                           (sin (* i (/ 35.99 128) twopi)))))))])
  (fft-complex-radix2-forward v)
  (ffts-draw (list v) (list v) 128 128))


;; oops, found a bug in rasterize-column.
(check-equal? (call-with-values
               (lambda ()
                 (rasterize-column (lambda (i) (cond [(= i 34) 1.0] 
                                                     [else 0.0]))
                                   33.5
                                   34.5))
               list)
              (list 0.5 1.0))


(let ([s (signal->rsound 4096
                         (indexed-signal
                          (lambda (i)
                            (* 0.5 (+ (sin (* i 4/128 twopi))
                                      (sin (* i (/ 35.99 128) twopi)))))))])
  ;; should show four spikes 
  (vector-draw/real/imag (rsound-fft/left s))
  ;; should show four spikes
  (vector-draw/mag/phase (rsound-fft/left s))
  ;; one of the spikes is so crisp it disappears when the window isn't tall:
  ;; window should be tall:
  (rsound-fft-draw s #:height 800)
  (rsound-fft-draw s #:zoom-freq 22050))



;; try something too short:
(check-exn 
 (lambda (exn) (regexp-match #rx"fewer than" (exn-message exn)))
 (lambda () (rsound-fft-draw (silence 500))))


