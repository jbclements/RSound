#lang racket/base

(require "rsound.rkt"
         "util.rkt"
         "envelope.rkt"
         "contrib/adventure-kid-waveforms.rkt"
         racket/match
         (for-syntax racket/base)
         (for-syntax syntax/parse)
         "define-memo.rkt"
         racket/runtime-path)

;; this module provides access to single-cycle waveforms from 
;; Adventure Kid (used by permission). Thanks, Adventure Kid!


(provide synth-note
         synth-note/raw)

(define-runtime-path main-wave-path "./contrib/AKWF_0001/")
(define-runtime-path vgame-wave-path "./contrib/AKWF_vgame/")

;; given a family and a number/spec, return a single cycle rsound
(define/memo (wave-lookup family spec)
  (match family
    ["main" (adventure-kid-waveform #f spec)]
    ["vgame" (adventure-kid-waveform "vgame" spec)]
    ["path" (rs-read spec)]))



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

;; a memoized version of the above. 
(define (resample/memo factor sound)
  (match (hash-ref resample-hash (list factor sound) #f)
    [#f (define result (resample factor sound))
        (hash-set! resample-hash (list factor sound) result)
        result]
    [other other]))
(define resample-hash (make-hash))

(define my-env (adsr/exp 200 0.5 2000 0.25 1000))

;; given a family, a number/path, a midi note number, and a duration in frames,
;; produce an rsound.
(define/memo (synth-note family wave-spec note-num duration)
  (define env (my-env (floor duration)))
  (define raw-sound (synth-note/raw family wave-spec note-num duration))
  (rs-mult env raw-sound))

;; same as above, but no envelope:
(define (synth-note/raw family wave-spec note-num duration)
  (define wave (wave-lookup family wave-spec))
  (define native-pitch (/ (default-sample-rate) (rsound-frames wave)))
  (define pitch (midi-note-num->pitch note-num))
  (define single-cycle 
    (resample/memo (/ pitch native-pitch) wave))
  (tile-to-len single-cycle duration))

;; generating menus:
(define (menu group max-idx)
  (rs-append*
   (for/list ([i (in-range 100)])
     (define num (add1 i))
     (rs-append
      (synth-note "main" num 47 6000)
      (cond [(= 0 (modulo num 3)) (silence 6000)]
            [else (silence 0)])))))

#;(rs-write (menu "main" 100) "/tmp/menu1.wav")
#;(rs-write (menu "vgame" 137) "/tmp/menu-vgame.wav")
