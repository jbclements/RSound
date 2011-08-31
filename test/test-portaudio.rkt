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
  (check-equal? (pa-get-error-text 'paBufferTooBig) "Buffer too big")
  
  
  
  (check-exn (lambda (exn) (string=? (exn-message exn) "PortAudio not initialized"))
             (lambda () (pa-get-host-api-count)))
  (check-exn (lambda (exn) (string=? (exn-message exn) "PortAudio not initialized"))
             (lambda () (pa-get-default-host-api)))
  
  (check-not-exn (lambda () (pa-initialize)))
  
  (check < 0 (pa-get-host-api-count))
  (check <= 0 (pa-get-default-host-api))
  
  (check-not-exn (lambda () (pa-terminate))))))

;; almost everything untested.