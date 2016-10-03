#lang racket

(require ffi/vector
         rsound/rsound
         rsound/private/s16vector-add
         (only-in rsound/util indexed-signal)
         (only-in rsound/common default-sample-rate)
         rackunit)

(module+ main
(define sr (default-sample-rate))

;; 1 second of noise:
(define test-rsound
  (signal->rsound 
   sr
   (indexed-signal
    (lambda (t)
      (- (* 2.0 (random)) 1.0)))))

(define (gc3) 
  (collect-garbage)
  (collect-garbage)
  (collect-garbage))

(define trials 5)

(define-syntax (rtavg stx)
  (syntax-case stx ()
    [(_ e)
     #'(rtavg/helper (lambda () e))]))

(define (rtavg/helper proc)
  (readable
   (mean
    (for/list ([i (in-range trials)])
      (gc3)
      (let-values ([(dc cpu real gc) (time-apply proc '())])
        real)))))

(define (mean l)
  (/ (apply + l)
     (length l)))

(define (readable n)
  (printf "~s seconds\n" (exact->inexact (/ (round n) 1000))))

#|
;; test times appearing here gathered on my macbook air:

(define t1 (for/list ([i (in-range 1000)]) test-rsound))

;; rsound-append*...
(gc3)
(printf "rsound-append*\n0.2 sec expected, 0.0002 generation ratio\n")
(rtavg (rsound-append* t1))
(printf "\n\n")

(printf "signal->rsound of simple constant value\n")
(rtavg (signal->rsound (* 100 sr) sr (lambda (t) 0.243)))
(printf "expected mean 0.8 seconds, 0.008 generation ratio\n")
(printf "could be improved substantially by caching.\n")
(printf "\n\n")
|#

#|
(printf "rsound-overlay*\n")
(printf "about 10 simultaneous streams\n")
(define quiet-noise 
  (signal->rsound sr sr
                  (lambda (t)
                    (- (* 0.02 (random)) 1.0))))
(rtavg (let ([offsets (for/list ([i (in-range 100)])
                        (list quiet-noise (inexact->exact
                                           (round (* sr 10.0 (random))))))])
         (rsound-overlay* offsets)))
(printf "about 1.5 seconds, 0.15 generation ratio overall, 0.015 per streamsec\n")

(printf "result of experiments: values much faster than vectors.\n\n")

(printf "simplified rsound-overlay* test\n")
(rtavg (let ([offsets (for/list ([i (in-range 10)])
                        (list quiet-noise 0))])
         (rsound-overlay* offsets)))


|#

(printf "s16buffer-add!/c\n")
(printf "100 x 1000 frames\n")
(printf "looks like about 0.01 seconds\n")
(let ()
  (define s16vec1 (make-s16vector 1000 0))
  (define s16ptr1 (s16vector->cpointer s16vec1))
  (define s16vec2 (make-s16vector 1000 2))
  (define s16ptr2 (s16vector->cpointer s16vec2))
  (rtavg
   (for ([i (in-range 100)])
     (s16buffer-add!/c s16ptr1 s16ptr2 1000)))))



