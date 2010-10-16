#lang racket

(require (prefix-in link: "private/drracket-link.rkt")
         "private/rsound-commander.rkt"
         "rsound.rkt"
         "util.rkt"
         racket/runtime-path)

(define-runtime-path version-path "./version.rktd")
(provide rsound-version)
(define (rsound-version)
  (file->value version-path))

;; if init hasn't happened, do it now.
(when (semaphore-try-wait? link:init-semaphore)
  (let ([saved-custodian (unbox link:link)])
  (unless (custodian? saved-custodian)
    (error 'rsound-init "expected to find custodian in link, got: ~e" saved-custodian))
  (set-box! link:link (new rsound-commander% (master-custodian saved-custodian)))))


(provide (all-from-out "rsound.rkt")
         (all-from-out "util.rkt"))