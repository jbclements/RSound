#lang racket/base

(require "rsound.rkt"
         "common.rkt"
         (only-in "util.rkt" resample-to-rate)
         racket/runtime-path)

(provide (all-defined-out))

(define-runtime-path sample-dir "./contrib/drum-samples/")

(define (load-sample s)
  (resample-to-rate (default-sample-rate)
                    (rs-read (build-path sample-dir (string-append s ".wav")))))

(define kick           (load-sample "kick"))
(define bassdrum       (load-sample "bassdrum"))
(define bassdrum-synth (load-sample "bassdrum-synth"))

(define o-hi-hat     (load-sample "o-hi-hat"))
(define c-hi-hat-1   (load-sample "c-hi-hat-1"))
(define c-hi-hat-2   (load-sample"c-hi-hat-2"))

(define clap-1       (load-sample "clap-1"))
(define clap-2       (load-sample "clap-2"))
(define crash-cymbal (load-sample "crash-cymbal"))

(define snare        (load-sample "snare"))

(define click-1      (load-sample "click1"))
(define click-2      (load-sample "click2"))



