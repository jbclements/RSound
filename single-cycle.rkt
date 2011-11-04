#lang racket/base

(require "rsound.rkt"
         "util.rkt"
         "envelope.rkt"
         racket/match
         (for-syntax racket/base)
         (for-syntax syntax/parse))

(provide synth-note)

;; single-cycle-sounds with envelope

(define wave (rs-read "/tmp/AKWF/AKWF_0001/AKWF_0066.wav"))
(define native-len (rsound-frames wave))
(define native-pitch (/ 44100 300))

(define single-cycle-table (make-hash))

;; TWO-ARG MEMOIZATION
;; (I would use Dave Herman's planet package, but the dependencies
;; are deadly)
(define-syntax (define/memo stx)
  (syntax-parse stx
    [(_ (name:id arg1:id arg2:id)
        body:expr ...)
     #`(define name
         (let ()
           (define the-hash (make-hash))
           (lambda (arg1 arg2)
             (define hash-key (list arg1 arg2))
             (define hash-lookup (hash-ref the-hash hash-key #f))
             (cond [(not hash-lookup)
                    (define result (let () body ...))
                    (hash-set! the-hash hash-key result)
                    result]
                   [else hash-lookup]))))]))


;; given a factor and a sound, resample the sound (using simple rounding)
;; to obtain a new one. Using e.g. factor of 2 will make the sound one
;; octave higher and half as long.
(define (resample factor sound)
  (define (left i) (rs-ith/left sound 
                                (inexact->exact (round (* factor i)))))
  (define (right i) (rs-ith/right sound 
                                  (inexact->exact (round (* factor i)))))
  (parameterize ([default-sample-rate 
                   (rsound-sample-rate sound)])
    (signals->rsound (inexact->exact
                      (floor (/ (rsound-frames sound) factor)))
                     left
                     right)))

(define resample-hash (make-hash))
(define (resample/memo factor sound)
  (match (hash-ref resample-hash (list factor sound) #f)
    [#f (define result (resample factor sound))
        (hash-set! resample-hash (list factor sound) result)
        result]
    [other other]))

;; given an rsound and a duration in frames, make enough copies of the rsound
;; (possibly less than 1) to make a sound of the given duration
(define (single-cycle->dur rsound duration)
  (let ()
    (define duration/int (inexact->exact (floor duration)))
    (define num-whole-copies (quotient duration/int (rsound-frames rsound)))
    (define leftover-frames (remainder duration/int (rsound-frames rsound)))
    (rs-append* (append 
                     (for/list ([i (in-range num-whole-copies)])
                       rsound)
                     (list (clip rsound 0 leftover-frames))))))

(define my-env (adsr/exp 200 0.5 2000 0.25 1000))

(define/memo (synth-note note-num duration)
  (define env (my-env (floor duration)))
  (define pitch (midi-note-num->pitch note-num))
  (define single-cycle (resample/memo (/ pitch native-pitch) wave))
  (define longer (single-cycle->dur single-cycle duration))
  (define result
    (rs-mult env longer))
  result)

