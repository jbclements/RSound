#lang racket

(require "../private/portaudio.rkt"
         rackunit
         rackunit/text-ui)

;; 

(run-tests
(test-suite "portaudio"
(let ()
(check-not-exn (lambda () (pa-get-version)))
(check-not-exn (lambda () (pa-get-version-text)))

;; this can change, as long as it's something sensible:
(check-equal? (pa-get-error-text 'paBufferTooBig) "Buffer too big"))))


;; almost everything untested.