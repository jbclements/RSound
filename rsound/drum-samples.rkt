#lang racket/base

(require "rsound.rkt"
         "common.rkt"
         (only-in "util.rkt" resample-to-rate)
         racket/runtime-path)

(provide (all-defined-out))

(define-runtime-path sample-dir "./contrib/drum-samples/")

(define (rs s)
  (resample-to-rate (default-sample-rate) s))

(define kick (rs (rs-read (build-path sample-dir "kick.wav"))))
(define bassdrum (rs (rs-read (build-path sample-dir "bassdrum.wav"))))
(define bassdrum-synth (rs (rs-read (build-path sample-dir "bassdrum-synth.wav"))))

(define o-hi-hat (rs (rs-read (build-path sample-dir "o-hi-hat.wav"))))
(define c-hi-hat-1 (rs (rs-read (build-path sample-dir "c-hi-hat-1.wav"))))
(define c-hi-hat-2 (rs(rs-read (build-path sample-dir "c-hi-hat-2.wav"))))

(define clap-1 (rs (rs-read (build-path sample-dir "clap-1.wav"))))
(define clap-2 (rs (rs-read (build-path sample-dir "clap-2.wav"))))
(define crash-cymbal (rs (rs-read (build-path sample-dir "crash-cymbal.wav"))))

(define snare (rs (rs-read (build-path sample-dir "snare.wav"))))



