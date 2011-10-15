#lang racket

(require (prefix-in rs: "rsound.rkt"))
;; these versions of the functions should do lots of error-checking:

;; I'd love to use contracts for this, but they don't work with
;; beginner.

;; okay, I'm stopping in the middle to work on other things...
(provide (rename-out [rs:rsound-start rsound-start]
                     [rs:rsound-stop rsound-stop]
                     [rs:rsound-sample-rate rsound-sample-rate]
                     [rs:rsound-frames rsound-frames]
                     #;([channels]
                     [play]
                     [signal?]
                     [signal-play]
                     [signal/block?]
                     [signal/block-play]
                     [signal/block-play/unsafe]
                     [rsound-loop]
                     [stop]
                     [rs-ith/left/s16]
                     [rs-ith/right/s16])))

#;(provide rsound-start
         
         rs-ith/left
         rs-ith/right
         set-rs-ith/left!
         set-rs-ith/right!
         #;rsound-scale
         rsound-equal?
         clip
         rs-append
         rs-append*
         assemble
         default-sample-rate
         mono-signal->rsound
         signals->rsound
         signal->rsound/filtered
         silence
         rs-read
         rs-read/clip
         rs-read-frames
         rs-read-sample-rate
         rs-write
         rs-largest-frame/range/left
         rs-largest-frame/range/right
         rs-largest-sample
         ;; for testing...
         same-sample-rate-check)

(define (positive-integer? n)
  (and (integer? n) (< 0 n)))

(define (nonnegative-integer? n)
  (and (integer? n) (<= 0 n)))

(define (rs-ith/left/s16 sound frame)
  (rsound-extractor sound frame #t (lambda (x) x)))

;; return the nth sample of an rsound's right channel
(define (rs-ith/right/s16 sound frame)
  (rsound-extractor sound frame #f (lambda (x) x)))

(define (rs-ith/left sound frame)
  (rsound-extractor sound frame #t rs:s16->real))

(define (rs-ith/right sound frame)
  (rsound-extractor sound frame #f rs:s16->real))

;; the abstraction behind the last four functions...
(define (rsound-extractor rsound frame left? scale-fun)
  (unless (rs:rsound? rsound)
    (raise-type-error 'rsound-extractor "rsound" 0 rsound frame))
  (unless (nonnegative-integer? frame)
    (raise-type-error 'rsound-extractor "nonnegative integer" 1 rsound frame))
  (unless (< frame (rs:rsound-frames rsound))
    (raise-type-error 'rsound-extractor (format "frame index less than available # of frames ~s" (rs:rsound-frames rsound)) 1 rsound frame))
  (rs:rsound-extractor rsound frame left? scale-fun))


