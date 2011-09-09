#lang racket

(require "portaudio.rkt"
         "portaudio-utils.rkt"
         ffi/vector
         rackunit
         rackunit/text-ui)

(define twopi (* 2 pi))

(run-tests
(test-suite "portaudio"
(let ()
  (check-not-exn (lambda () (pa-get-version)))
  (check-not-exn (lambda () (pa-get-version-text)))
  
  ;; this can change, as long as it's something sensible:
  (check-equal? (pa-get-error-text 'paBufferTooBig) "Buffer too big")
  
  (check-equal? (pa-terminate-completely) #t)
  (check-exn (lambda (exn) (string=? (exn-message exn) "PortAudio not initialized"))
             (lambda () (pa-get-host-api-count)))
  (check-exn (lambda (exn) (string=? (exn-message exn) "PortAudio not initialized"))
             (lambda () (pa-get-default-host-api)))

  (check-equal? (pa-initialized?) #f)
  (check-not-exn (lambda () (pa-initialize)))
  (check-equal? (pa-initialized?) #t)
  (check-not-exn (lambda () (pa-terminate)))
  (check-equal? (pa-initialized?) #f)
  (check-not-exn (lambda () (pa-initialize)))
  (check-not-exn (lambda () (pa-initialize)))
  (check-not-exn (lambda () (pa-initialize)))
  (check-not-exn (lambda () (pa-initialize)))
  (check-equal? (pa-initialized?) #t) 
  (check-not-exn (lambda () (pa-terminate)))
  (check-equal? (pa-initialized?) #t)
  (check-equal? (pa-terminate-completely) #t)
  (check-not-exn (lambda () (pa-initialize)))
  
  ;; on different platforms, the results will be different....
  (check < 0 (pa-get-host-api-count))
  (check <= 0 (pa-get-default-host-api))
  (check-not-exn (lambda () (pa-get-host-api-info 0)))
  (check-exn exn:fail? 
             (lambda () (pa-get-host-api-info (+ 14 (pa-get-host-api-count)))))
  

  
  ;; the remainder of these require 2-channel output
  
  (define (open-test-stream callback)
    (pa-open-default-stream
     0             ;; input channels
     2             ;; output channels
     'paInt16      ;; sample format
     44100.0       ;; sample rate
     1000          ;;frames-per-buffer  ;; frames per buffer
     callback ;; callback (NULL means just wait for data)
     #f))
  
  (define channels 2)
  (define s16max 32767)
  
  ;; first, test the copying-callback.

  (define response-channel (make-channel))
  
  ;; 1/2 second of 330
  (define tone-buf-330 (make-s16vector (* 22050 channels)))
  (for ([i (in-range 0 22050)])
    (define sample (inexact->exact (round (* s16max 0.1 (sin (* i 1/44100 330 2 pi))))))
    (s16vector-set! tone-buf-330 (* i 2) sample)
    (s16vector-set! tone-buf-330 (add1 (* i 2)) sample))
  (let ()
    (define abort-box (box #f))
    (define callback
      (make-copying-callback (s16vector->cpointer tone-buf-330)
                             22050
                             response-channel abort-box))
    (define stream (open-test-stream callback))
    (printf "1/2 second @ 330 Hz\n")
    (sleep 2)
    (printf "starting now... ")
    (pa-start-stream stream)
    (sleep 0.5)
    (printf "...ending now.\n")
    (sleep 2))
  
  
  
  
  )))

#|;; multiple streams? 

(define (tone-1 t)
  (* 0.1 (sin (* t 1/44100 440 twopi))))

(define tone-1-callback ((make-generating-callback tone-1) (make-channel)))

(define stream-1
  (pa-open-default-stream
                0             ;; input channels
                2             ;; output channels
                'paInt16      ;; sample format
                44100.0       ;; sample rate
                1000          ;;frames-per-buffer  ;; frames per buffer
                tone-1-callback ;; callback (NULL means just wait for data)
                #f))

(collect-garbage)
(collect-garbage)
(collect-garbage)
(pa-start-stream stream-1)

(sleep 3)

(define (tone-2 t)
  (* 0.1 (sin (* t 1/44100 550 twopi))))

(define tone-2-callback ((make-generating-callback tone-2) (make-channel)))


(define stream-2
  (pa-open-default-stream
                0             ;; input channels
                2             ;; output channels
                'paInt16      ;; sample format
                44100.0       ;; sample rate
                1000          ;;frames-per-buffer  ;; frames per buffer
                tone-2-callback ;; callback (NULL means just wait for data)
                #f))

(pa-start-stream stream-2)

(sleep 3)
|#

