#lang racket/base

(require "rsound.rkt"
         racket/runtime-path)

(provide (all-defined-out))

(define-runtime-path sample-dir "./contrib/drum-samples/")

(define kick (rs-read (build-path sample-dir "kick.wav")))
(define bassdrum (rs-read (build-path sample-dir "bassdrum.wav")))
(define bassdrum-synth (rs-read (build-path sample-dir "bassdrum-synth.wav")))

(define o-hi-hat (rs-read (build-path sample-dir "o-hi-hat.wav")))
(define c-hi-hat-1 (rs-read (build-path sample-dir "c-hi-hat-1.wav")))
(define c-hi-hat-2 (rs-read (build-path sample-dir "c-hi-hat-2.wav")))

(define clap-1 (rs-read (build-path sample-dir "clap-1.wav")))
(define clap-2 (rs-read (build-path sample-dir "clap-2.wav")))
(define crash-cymbal (rs-read (build-path sample-dir "crash-cymbal.wav")))

(define snare (rs-read (build-path sample-dir "snare.wav")))



