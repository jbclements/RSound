#lang racket

(require ffi/vector)

(require "../rsound-commander.rkt")

(define buffer-frames 10000000)
(define frame-rate 44100)
(define CHANNELS 2)

(define tgt (make-s16vector (* CHANNELS buffer-frames)))

(define msec-per-frame-available (/ 1000 frame-rate))
(printf "msec available per frame: ~s\n" (exact->inexact msec-per-frame-available))

(define (report msec)
  (define msec-per-frame (/ msec buffer-frames))
  (printf "msec per frame: ~a\n" (exact->inexact msec-per-frame))
  (printf "runs at ~ ~ax\n" 
          (round (/ msec-per-frame-available msec-per-frame))))

(let ()
  (define (simplest-signal) 16000)
  (define sbu (signal/16->signal/block/unsafe simplest-signal))
  
  (printf "trivial signal using signal/16:\n")
  (define-values (dc cpu-msec real-msec gc-msec)
    (time-apply
     sbu 
     (list (s16vector->cpointer tgt) buffer-frames)))
  (report cpu-msec))

(let ()
  (define ctr 0)
  (define (sig)
    (set! ctr (add1 ctr))
    (cond [(< (modulo ctr 143) 70) 23000]
          [else -23000]))
  (printf "simple integer computation signal using signal/16:\n")
  (define sbu (signal/16->signal/block/unsafe sig)) 
  (define-values (dc cpu-msec real-msec gc-msec)
    (time-apply 
     sbu
     (list (s16vector->cpointer tgt) buffer-frames)))
  (report cpu-msec))


(let ()
  (define (simplest-signal) 0.7)
  (define sbu (signal->signal/block/unsafe simplest-signal))
  
  (printf "trivial signal:\n")
  (define-values (dc cpu-msec real-msec gc-msec)
    (time-apply
     sbu 
     (list (s16vector->cpointer tgt) buffer-frames)))
  (report cpu-msec))

(let ()
  (define ctr 0)
  (define (sig)
    (set! ctr (add1 ctr))
    (cond [(< (modulo ctr 143) 70) 0.7]
          [else -0.7]))
  (printf "simple integer computation signal:\n")
  (define sbu (signal->signal/block/unsafe sig)) 
  (define-values (dc cpu-msec real-msec gc-msec)
    (time-apply 
     sbu
     (list (s16vector->cpointer tgt) buffer-frames)))
  (report cpu-msec))


#;(let ()
  (define (simplest-signal) 16000)
  (define sbu (signal/16->signal/block/unsafe/experimental simplest-signal))
  
  (printf "trivial signal using signal/16 and unsafe-s16vector-set!:\n")
  (define-values (dc cpu-msec real-msec gc-msec)
    (time-apply
     sbu 
     (list (s16vector->cpointer tgt) buffer-frames)))
  (report cpu-msec))

#;(let ()
  (define ctr 0)
  (define (sig)
    (set! ctr (add1 ctr))
    (cond [(< (modulo ctr 143) 70) 23000]
          [else -23000]))
  (printf "simple integer computation signal using signal/16 and unsafe-s16vector-set!:\n")
  (define sbu (signal/16->signal/block/unsafe/experimental sig)) 
  (define-values (dc cpu-msec real-msec gc-msec)
    (time-apply 
     sbu
     (list (s16vector->cpointer tgt) buffer-frames)))
  (display (s16vector-ref tgt 5243110))
  (newline)
  (report cpu-msec))