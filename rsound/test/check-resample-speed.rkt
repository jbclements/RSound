#lang typed/racket


(require/typed
 ffi/vector
 [#:opaque S16Vector s16vector?]
 [s16vector-length (S16Vector -> Natural)]
 [s16vector-ref (S16Vector Natural -> Fixnum)]
 [s16vector-set! (S16Vector Natural Fixnum -> Void)]
 [make-s16vector (Natural -> S16Vector)]
 )

(require/typed
 racket/unsafe/ops
 
 [unsafe-s16vector-ref (S16Vector Natural -> Fixnum)]
 [unsafe-s16vector-set! (S16Vector Natural Fixnum -> Void)])

(define CHANNELS 2)

(define (resample/interp [factor : Positive-Flonum] [sound : S16Vector])
  ;; add the 'min 1' to the other code no matter what else happens...
  ;; done
  (define sound-frames (floor (/ (s16vector-length sound)
                                 CHANNELS)))
  (when (< sound-frames 1)
    (error 'zzzsanout3n))
  (define sound-frames-pos (assert
                            (assert sound-frames fixnum?)
                            positive?))
  (define new-sound-len
    ;; cast shouldn't be necessary? Not in inner loop, though.
    (assert
     (inexact->exact (floor (/ sound-frames-pos
                               factor)))
     exact-nonnegative-integer?))
  ;; after this new
  (unless (< 0 new-sound-len)
    (raise-argument-error 'resample "sound long enough to be shortened by the given factor"
                          1 factor sound))
  (define (the-sig [extractor : (S16Vector Natural -> Fixnum)])
    (lambda ([i : Natural]) : Flonum
      (define virtual-source-sample (min (sub1 sound-frames-pos)
                                         (* factor i)))
      (define lower-index (inexact->exact (floor virtual-source-sample)))
      (define fractional-part
        (exact->inexact (- virtual-source-sample lower-index)))
      ;; eliminate this cast hopefully:
      (define lower-index-nat (assert lower-index
                                      exact-nonnegative-integer?))
      (cond [(= fractional-part 0)
             (exact->inexact (extractor sound lower-index-nat))]
            [else
             (exact->inexact
              (+ (* (- 1.0 fractional-part)
                    (exact->inexact
                     (extractor sound lower-index-nat)))
                 (* fractional-part
                    (extractor sound (add1 lower-index-nat)))))])))
  (rs-generate new-sound-len
               (the-sig vec-ith/left)
               (the-sig vec-ith/right)))





(define FRAME-RATE 48000)

(define VOLUME 0.1)
(define FREQUENCY 430)

(: build-sound (Natural (Natural -> Flonum) -> S16Vector))
(define (build-sound frames generator)
  (rs-generate frames generator generator))

(define (vec-ith/left [s : S16Vector] [idx : Natural]) : Fixnum
  (unsafe-s16vector-ref s (* idx CHANNELS)))

(define (vec-ith/right [s : S16Vector] [idx : Natural]) : Fixnum
  (unsafe-s16vector-ref s (add1 (* idx CHANNELS))))

;; given a number of frames and a left and right generator and a sample
;; rate, produce a sound.
(define (rs-generate [frames : Natural]
                     [left-fun : (Natural -> Flonum)]
                     [right-fun : (Natural -> Flonum)]) : S16Vector
  (define vec (make-s16vector (* CHANNELS frames)))
  (for ([i frames])
    (unsafe-s16vector-set! vec (* CHANNELS i) (real->s16 (left-fun i)))
    (unsafe-s16vector-set! vec (add1 (* CHANNELS i)) (real->s16 (right-fun i))))
  vec)

(define s16max #x7fff)
(define -s16max (- s16max))
(define s16max/i (exact->inexact #x7fff))

(: real->s16 (Real -> Fixnum))
(define (real->s16 x)
  ;; avoidable cast?
  (min s16max (max -s16max (assert (inexact->exact (round (* s16max/i x))) fixnum?))))
 
(define (sine-tone [f : Natural]) : Flonum
  ;; eliminate the exact->inexact?
  (* VOLUME (exact->inexact (sin (* (exact->inexact 2) pi (exact->inexact FREQUENCY) (exact->inexact (/ f FRAME-RATE)))))))
 
(define test-sound
  (time
  (let ()
    (define frames (* 15 44100))
    (define vec (make-s16vector (* CHANNELS frames)))
    (for ([i frames])
      (s16vector-set! vec (* CHANNELS i) (real->s16 (sine-tone i)))
      (s16vector-set! vec (add1 (* CHANNELS i)) (real->s16 (sine-tone i))))
    vec)))

(time (resample/interp
       (assert (assert (exact->inexact (/ 48000 44100)) flonum?)
               positive?)
       test-sound))