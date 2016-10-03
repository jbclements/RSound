#lang racket

(require ffi/vector)

(require rsound/rsound-commander
         rsound/network
         rsound/reverb
         (only-in rsound/common default-sample-rate))

(define buffer-frames 10000000)
(define FRAME-RATE (default-sample-rate))
(define CHANNELS 2)

(define tgt (make-s16vector (* CHANNELS buffer-frames)))

(define msec-per-frame-available (/ 1000 FRAME-RATE))

(define (report msec)
  (define msec-per-frame (/ msec buffer-frames))
  (printf "msec per frame: ~a\n" (exact->inexact msec-per-frame))
  (printf "uses ~s% of available time\n" 
          (/ (round (* 1e+6 (/ msec-per-frame msec-per-frame-available))) 1e+4)))

(define (check-signal sig)
  (define sbu (signal/16->signal/block/unsafe sig)) 
  (define-values (dc cpu-msec real-msec gc-msec)
    (time-apply 
     sbu
     (list (s16vector->cpointer tgt) buffer-frames)))
  (report real-msec))

(module+ main

  (printf "msec available per frame: ~s\n" (exact->inexact msec-per-frame-available))

  (let ()
  (define (simplest-signal) 16000)
  (printf "trivial signal using signal/16:\n")
  (check-signal simplest-signal))

(let ()
  (define ctr 0)
  (define (sig)
    (set! ctr (add1 ctr))
    (cond [(< (modulo ctr 143) 70) 23000]
          [else -23000]))
  (printf "simple integer computation signal using signal/16:\n")
  (check-signal sig))


(let ()
  (define (simplest-signal) 0.7)
  (printf "trivial signal:\n")
  (check-signal simplest-signal))

(let ()
  (define ctr 0)
  (define (sig)
    (set! ctr (add1 ctr))
    (cond [(< (modulo ctr 143) 70) 0.7]
          [else -0.7]))
  (printf "simple integer computation signal:\n")
  (check-signal sig))

(let ()
  (define sig 
    (network ()
      [ctr <= (simple-ctr 0 1)]
      [out = (cond [(< (modulo ctr 143) 70) 0.7]
                   [else -0.7])]))
  (printf "simple integer computation signal using network form:\n")
  (check-signal (network-init sig)))


(let ()
  (define sig 
    (network ()
      [ctr <= (simple-ctr 0 1)]
      [out = (cond [(< (modulo ctr 143) 70) 0.7]
                   [else -0.7])]
      [out2 <= reverb out]))
  (printf "simple integer computation signal using network form and reverb:\n")
  (check-signal (network-init sig)))





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
  (report cpu-msec)))