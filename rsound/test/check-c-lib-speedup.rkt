#lang racket

(require "../private/s16vector-add.rkt"
         ffi/vector
         ffi/unsafe
         rackunit
         racket/unsafe/ops
         "../rsound.rkt"
         "../util.rkt")

(define (report test-buf-size cpu-msec)
  (printf "# of samples: ~s\n" test-buf-size)
  (printf "time taken: ~s msec\n" cpu-msec)
  (printf "time per sample: ~s msec\n" (exact->inexact (/ cpu-msec test-buf-size))))

(printf "BUFFER ADD\n")

(let () 
  (printf "test using c code\n")
  (define test-buf-size 10000000)
  
  ;; s16vector of length 20 with random numbers from -50 to 49
  (define src-buf (make-s16vector test-buf-size 0))
  (define src-cpointer (s16vector->cpointer src-buf))
  (for ([i (in-range (s16vector-length src-buf))])
    (s16vector-set! src-buf i (- (random 100) 50)))
  
  (define tgt-buf (make-s16vector test-buf-size 0))
  (define tgt-cpointer (s16vector->cpointer tgt-buf))
  
  
  (define-values (_1 cpu-msec _2 _3)
    (time-apply
     s16buffer-add!/c 
     (list tgt-cpointer src-cpointer test-buf-size)))
  (report test-buf-size cpu-msec)
  )


(let () 
  (printf "test using unsafe-s16vector-set!\n")
  (define test-buf-size 10000000)
  
  ;; s16vector with random numbers from -50 to 49
  (define src-buf (make-s16vector test-buf-size 0))
  (for ([i (in-range (s16vector-length src-buf))])
    (s16vector-set! src-buf i (- (random 100) 50)))
  
  (define tgt-buf (make-s16vector test-buf-size 0))
  
  (define-values (_1 cpu-msec _2 _3)
    (time-apply
     (lambda ()
       (for ([i (in-range test-buf-size)])
         (unsafe-s16vector-set! tgt-buf i (unsafe-s16vector-ref src-buf i))))
     (list)))
  (report test-buf-size cpu-msec)
  )

(printf "ADD AND MULTIPLY\n")

(let () 
  (printf "test using c-library add-and-multiply\n")
  (define test-buf-size 10000000)
  
  ;; s16vector with random numbers from -50 to 49
  (define src-buf (make-s16vector test-buf-size 0))
  (for ([i (in-range (s16vector-length src-buf))])
    (s16vector-set! src-buf i (- (random 100) 50)))
  
  (define tgt-buf (make-s16vector test-buf-size 0))
  
  (define tgt-cpointer (s16vector->cpointer tgt-buf))
  (define src-cpointer (s16vector->cpointer src-buf))
  
  (define-values (_1 cpu-msec _2 _3)
    (time-apply
     (lambda ()
       (s16buffer-mult-add!/c tgt-cpointer src-cpointer test-buf-size 0.34))
     (list)))
  (report test-buf-size cpu-msec)
  )

(let () 
  (printf "test using unsafe-s16vector-set! and a multiply\n")
  (define test-buf-size 10000000)
  
  ;; s16vector with random numbers from -50 to 49
  (define src-buf (make-s16vector test-buf-size 0))
  (for ([i (in-range (s16vector-length src-buf))])
    (s16vector-set! src-buf i (- (random 100) 50)))
  
  (define tgt-buf (make-s16vector test-buf-size 0))
  
  (define-values (_1 cpu-msec _2 _3)
    (time-apply
     (lambda ()
       (for ([i (in-range test-buf-size)])
         (unsafe-s16vector-set! tgt-buf i 
                                (* 0.34 (unsafe-s16vector-ref src-buf i)))))
     (list)))
  (report test-buf-size cpu-msec)
  )

(let () 
  (printf "test using rs-mult (including allocation)\n")
  (define test-buf-size 10000000)

  (define CHANNELS 2)
  ;; s16vector with random numbers from -50 to 49
  (define src (silence (/ test-buf-size CHANNELS)))
  (for ([i (in-range (/ test-buf-size CHANNELS))])
    (set-rs-ith/left! src i (random))
    (set-rs-ith/right! src i (random)))
  
  (define-values (_1 cpu-msec _2 _3)
    (time-apply
     (lambda ()
       (rs-scale 0.34 src))
     (list)))
  (report test-buf-size cpu-msec)
  )









