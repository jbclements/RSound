#lang racket


(require "../network.rkt"
         "../reverb.rkt"
         "../rsound.rkt"
         rackunit)

(provide the-test-suite)

(define the-test-suite
  (test-suite 
   "reverb"
   (let ()
     (define snd-vec
       (signal-samples (network ()
                         [ctr <= frame-ctr]
                         [impulse = (cond [(= ctr 0) 1.0]
                                          [else 0.0])]
                         [out <= reverb impulse])
                       5000))
     ;; check for the impulse:
     (check-= (vector-ref snd-vec 0) 1.0 1e-9)
     ;; check for nothing before about 1/20 sec
     (check-true
      (for/and ([i (in-range 1 2000)])
        (< (abs (- (vector-ref snd-vec i) 0.0)) 1e-9)))
     ;; make sure there's some reverb:
     (check-true
      (for/or ([i (in-range 2000 5000)])
        (< 0.0 (vector-ref snd-vec i)))))))

(module+ test
  (require rackunit/text-ui)
  
  (run-tests the-test-suite))

