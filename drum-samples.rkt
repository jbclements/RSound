#lang racket

(require "rsound.rkt"
         racket/runtime-path)

(provide (all-defined-out))

(define-runtime-path sample-dir "./contrib/drum-samples/")

(define kick (rsound-read (build-path sample-dir "kick.wav")))
(define bassdrum (rsound-read (build-path sample-dir "bassdrum.wav")))
(define bassdrum-synth (rsound-read (build-path sample-dir "bassdrum-synth.wav")))

(define o-hi-hat (rsound-read (build-path sample-dir "o-hi-hat.wav")))
(define c-hi-hat-1 (rsound-read (build-path sample-dir "c-hi-hat-1.wav")))
(define c-hi-hat-2 (rsound-read (build-path sample-dir "c-hi-hat-2.wav")))

(define clap-1 (rsound-read (build-path sample-dir "clap-1.wav")))
(define clap-2 (rsound-read (build-path sample-dir "clap-2.wav")))
(define crash-cymbal (rsound-read (build-path sample-dir "crash-cymbal.wav")))

(define snare (rsound-read (build-path sample-dir "snare.wav")))



