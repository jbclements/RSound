#lang racket

(require racket/date
         racket/runtime-path)

(define-runtime-path version-path "./version.rktd")

(with-output-to-file version-path
  #:exists 'truncate
    (lambda ()
      (printf "~s\n" (date->string (seconds->date (current-seconds)) #t))))

(system "raco pack --collect --replace /tmp/rsound.plt rsound")
