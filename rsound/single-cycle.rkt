#lang racket/base

(require "rsound.rkt"
         "common.rkt"
         "util.rkt"
         "envelope.rkt"
         "contrib/adventure-kid-waveforms.rkt"
         racket/match
         (for-syntax racket/base)
         (for-syntax syntax/parse)
         memoize
         racket/runtime-path)

;; this module provides access to single-cycle waveforms from 
;; Adventure Kid (used by permission). Thanks, Adventure Kid!


(provide synth-note
         synth-note/raw
         synth-waveform)

(define-runtime-path main-wave-path "./contrib/AKWF_0001/")
(define-runtime-path vgame-wave-path "./contrib/AKWF_vgame/")

;; given a family and a number/spec, return a single cycle rsound
(define/memo (wave-lookup family spec)
  (match family
    ["main" (adventure-kid-waveform #f spec)]
    ["vgame" (adventure-kid-waveform "vgame" spec)]
    ["path" (rs-read spec)]
    [other (raise-argument-error 'wave-lookup "string that is 'main', 'vgame', or 'path'" 0 family spec)]))

;; a memoized version of resample.
(define/memo (resample/memo factor sound)
  (resample factor sound))

(define my-env (adsr 200 0.5 1 0.25 1000))

;; given a family, a number/path, a midi note number, and a duration in frames,
;; produce an rsound.
(define/memo (synth-note family wave-spec note-num duration)
  (define raw-sound (synth-note/raw family wave-spec note-num duration))
  (define env (my-env (floor duration)))
  (rs-mult env raw-sound))

;; same as above, but no envelope:
(define (synth-note/raw family wave-spec note-num duration)
  (define wave (wave-lookup family wave-spec))
  (define native-pitch (/ (default-sample-rate) (rs-frames wave)))
  (define pitch (midi-note-num->pitch note-num))
  (define single-cycle 
    (resample/memo (/ pitch native-pitch) wave))
  (tile-to-len single-cycle duration))

;; generate a 1Hz waveform:
(define (synth-waveform family wave-spec)
  (define wave (wave-lookup family wave-spec))
  (define native-pitch (/ (default-sample-rate) (rs-frames wave)))
  (resample/interp (/ 1 native-pitch) wave))

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
