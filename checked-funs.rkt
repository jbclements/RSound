#lang racket

(require (prefix-in r: "rsound.rkt"))
;; these versions of the functions should do lots of error-checking:

;; I'd love to use contracts for this, but they don't work with
;; beginner.

;; I'm only including the functions that could plausibly be used in 
;; inner loops; these are the ones that we don't want to check when 
;; self-calls are made... though honestly, types provide a better 
;; solution.

(define (positive-integer? n)
  (and (integer? n) (< 0 n)))

(define (nonnegative-integer? n)
  (and (integer? n) (<= 0 n)))

(provide rs-ith/left/s16
         rs-ith/right/s16
         rs-ith/left
         rs-ith/right
         set-rs-ith/left/s16!
         set-rs-ith/right/s16!
         set-rs-ith/left!
         set-rs-ith/right!)

(define (rs-ith/left/s16 sound frame)
  (ref-arg-checker 'rs-ith/left/s16 sound frame)
  (r:rsound-extractor sound frame #t (lambda (x) x)))

(define (rs-ith/right/s16 sound frame)
  (ref-arg-checker 'rs-ith/right/s16 sound frame)
  (r:rsound-extractor sound frame #f (lambda (x) x)))

(define (rs-ith/left sound frame)
  (ref-arg-checker 'rs-ith/left sound frame)
  (r:rsound-extractor sound frame #t r:s16->real))

(define (rs-ith/right sound frame)
  (ref-arg-checker 'rs-ith/right sound frame)
  (r:rsound-extractor sound frame #f r:s16->real))

(define (ref-arg-checker fun-name sound frame)
  (unless (r:rsound? sound)
    (raise-type-error fun-name "rsound" 0 sound frame))
  (unless (nonnegative-integer? frame)
    (raise-type-error fun-name "integer" 1 sound frame))
  (unless (< frame (r:rs-frames sound))
    (raise-type-error
     fun-name
     (format
      "frame index less than available # of frames ~s"
      (r:rs-frames sound)) 1 sound frame)))

;; mutators

(define (set-rs-ith/left! . args)
  (apply mutator-arg-checker 'set-rs-ith/left! args)
  (apply r:set-rs-ith/left! args))

(define (set-rs-ith/right! . args)
  (apply mutator-arg-checker 'set-rs-ith/right! args)
  (apply r:set-rs-ith/right! args))

(define (set-rs-ith/left/s16! . args)
  (apply mutator-arg-checker 'set-rs-ith/left/s16! args)
  (apply r:set-rs-ith/left/s16! args))

(define (set-rs-ith/right/s16! . args)
  (apply mutator-arg-checker 'set-rs-ith/right/s16! args)
  (apply r:set-rs-ith/right/s16! args))



(define (mutator-arg-checker fun-name sound frame new-val)
  (unless (r:rsound? sound)
    (raise-type-error fun-name "rsound" 0 sound frame new-val))
  (unless (nonnegative-integer? frame)
    (raise-type-error fun-name "nonnegative integer" 1 sound frame new-val))
  (unless (< frame (r:rs-frames sound))
    (raise-type-error 
     fun-name
     (format "frame index less than available # of frames ~s" (r:rs-frames sound)) 1 sound frame new-val))
  (unless (real? new-val)
    (raise-type-error fun-name "real" 2 sound frame new-val)))




