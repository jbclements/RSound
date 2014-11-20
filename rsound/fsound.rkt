#lang racket/base

;; floating point double sounds. parallel development, for now. 

(require (only-in ffi/unsafe memcpy _double memset ptr-add ctype-sizeof)
         ffi/vector
         ;"read-wav.rkt"
         ;"write-wav.rkt"
         ;"network.rkt"
         "rsound.rkt"
         "define-argcheck.rkt"
         "common.rkt"
         (only-in "rsound-commander.rkt" channels)
         ;; TODO : C code for adding doubles
         #;"private/double-add.rkt"
         ;racket/contract
         ;racket/match
         #;racket/list)

(provide (struct-out fsound)
         fs-frames
         vec->fsound
         fs-ith/left      fs-ith/right
         set-fs-ith/left! set-fs-ith/right!
         rsound->fsound
         fsound->rsound
         fs-equal?
         )

;; will we actually enforce clipping at this value?
(define SOUNDMAX 1.0)
(define -SOUNDMAX -1.0)
(define DOUBLESIZE (ctype-sizeof _double))

;; an fsound provides a representation for sounds
;; that leaves them packed as C data, in double (floating point) form.

;; are two fsounds equal?
(define/argcheck (fs-equal? [r1 fsound? "fsound"]
                            [r2 fsound? "fsound"])
  (and (= (fs-frames r1)
          (fs-frames r2))
       (= (fsound-sample-rate r1)
          (fsound-sample-rate r2))
       ;; possible shortcut for 'eq?' f64vectors
       (or (eq? (fsound-data r1) (fsound-data r2))
           (for/and ([i (in-range (fs-frames r1))])
             (and (= (fs-ith/left r1 i) (fs-ith/left r2 i))
                  (= (fs-ith/right r1 i) (fs-ith/right r2 i)))))))

;; should these hash functions use a mask to guarantee fixnum-ness?

;; a pair of hashing functions for fsounds
(define HASH-CONSTANT-3 106184275)
(define (fsound-hash-1 fs recursive-equal-hash)
  (+ HASH-CONSTANT-3 
     (f64vector-hash-1 (fsound-data fs) recursive-equal-hash)
     (* #x1 (fsound-start fs))
     (* #x100 (fsound-stop fs))
     (* #x10000 (inexact->exact (round (fsound-sample-rate fs))))))

(define HASH-CONSTANT-4 1535131902)
(define (fsound-hash-2 fs recursive-equal-hash)
  (+ HASH-CONSTANT-4 
     (f64vector-hash-2 (fsound-data fs) recursive-equal-hash)
     (* #x10000 (fsound-start fs))
     (* #x100 (fsound-stop fs))
     (* #x1 (inexact->exact (round (fsound-sample-rate fs))))))

;; a pair of hashing functions for f64vectors that represent fsounds
(define HASH-CONSTANT-1 1102259063)
(define (f64vector-hash-1 v1 recursive-equal-hash)
  (+ HASH-CONSTANT-1
     (cond [(= (f64vector-length v1) 0) 0]
           [else
            (define quarter-len (/ (f64vector-length v1) 4))
            (+ (* #x80 (f64vector-length v1))
               (f64vector-ref v1 0)
               (* #x10000 (f64vector-ref v1 (floor quarter-len)))
               (f64vector-ref v1 (floor (* 2 quarter-len)))
               (* #x10000 (f64vector-ref v1 (floor (* 3 quarter-len)))))])))

(define HASH-CONSTANT-2 1073452368)
(define (f64vector-hash-2 v1 recursive-equal-hash)
  (+ HASH-CONSTANT-2
     (cond [(= (f64vector-length v1) 0) 0]
           [else
            (define quarter-len (/ (f64vector-length v1) 4))
            (+ (* #x100 (f64vector-length v1))
               (f64vector-ref v1 0)
               (f64vector-ref v1 (floor quarter-len))
               (* #x10000 (f64vector-ref v1 (floor (* 2 quarter-len))))
               (* #x10000 (f64vector-ref v1 (floor (* 3 quarter-len)))))])))

;; a fsound is (fsound f64vector positive-integer positive-integer positive)
(struct fsound (data start stop sample-rate) 
  #:transparent
  ;#:property prop:equal+hash
  ;(list fsound=? fsound-hash-1 fsound-hash-2)
  #:methods gen:equal+hash
  [(define equal-proc (lambda (fs1 fs2 _) (fs-equal? fs1 fs2)))
   (define hash-proc fsound-hash-1)
   (define hash2-proc fsound-hash-2)]
  )


;; how many frames long is the sound?
(define/argcheck (fs-frames [fsound fsound? "fsound"])
  (- (fsound-stop fsound) (fsound-start fsound)))

;; fill in 0 and max-frames for a newly created fsound
(define (vec->fsound f64vec sample-rate)
  (when (= (f64vector-length f64vec) 0)
    (raise-argument-error 'fsound/all "f64vector of length > 0" 0 f64vec sample-rate))
  (fsound f64vec 0 (/ (f64vector-length f64vec) channels) sample-rate))


;; return the nth sample of an fsound's left channel as a real number
(define (fs-ith/left sound frame)
  (fs-extractor sound frame #t))

;; return the nth sample of an fsound's right channel
;; as a real number in the range -1 <= x <= 1
(define (fs-ith/right sound frame)
  (fs-extractor sound frame #f))

;; the abstraction behind the last four functions...
(define (fs-extractor fsound frame left?)
  (f64vector-ref (fsound-data fsound) 
                 (frame->sample (+ (fsound-start fsound) frame)
                                left?)))



;; set the ith frame of the left channel to be new-val
(define (set-fs-ith/left! sound frame new-val)
  (fs-mutator sound frame #t new-val))

;; set the ith frame of the right channel to be new-val
(define (set-fs-ith/right! sound frame new-val)
  (fs-mutator sound frame #f new-val))

;; a mutation abstraction:
(define (fs-mutator fsound frame left? new-val)
  (f64vector-set! (fsound-data fsound)
                  (frame->sample (+ (fsound-start fsound) frame) left?)
                  new-val))

;; CONVERSION

;; turn an rsound into an fsound (only from start to stop)
(define (rsound->fsound rs)
  (define len (rs-frames rs))
  (define new-vec (make-f64vector (* channels len)))
  (for ([i len])
    (f64vector-set! new-vec (frame->sample i #t) (rs-ith/left rs i))
    (f64vector-set! new-vec (frame->sample i #f) (rs-ith/right rs i)))
  (vec->fsound new-vec (rsound-sample-rate rs)))

;; turn an fsound into an rsound (only from start to stop)
(define (fsound->rsound fs)
  (define len (fs-frames fs))
  (define new-vec (make-s16vector (* channels len)))
  (for ([i len])
    (s16vector-set! new-vec (frame->sample i #t)
                    (real->s16 (fs-ith/left fs i)))
    (s16vector-set! new-vec (frame->sample i #f) 
                    (real->s16 (fs-ith/right fs i))))
  (vec->rsound new-vec (fsound-sample-rate fs)))