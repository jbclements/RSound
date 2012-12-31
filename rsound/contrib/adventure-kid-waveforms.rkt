#lang racket/base

(require racket/runtime-path
         "../rsound.rkt")

(define-runtime-path here ".")

(provide adventure-kid-waveform)

(define (adventure-kid-waveform family index)
  (when (not (exact-nonnegative-integer? index))
    (raise-argument-error 'adventure-kid-waveform
                          "exact nonnegative integer"
                          1
                          family
                          index))
  (define wav-path
    (cond [(not family)
           (format "AKWF_~a/AKWF_~a.wav" 
                   (pad-to-4 
                    (ceiling (/ index 100)))
                   (pad-to-4 index))]          
          [(string? family)
           (format "AKWF_~a/AKWF_~a_~a.wav" 
                   family
                   family
                   (pad-to-4 index))]))
  (rs-read (build-path here wav-path)))

(define (pad-to-4 n)
  (define s (number->string n))
  (define spaces
    (for/list ([i (in-range (-  4 (string-length s)))])
      #\0))
  (string-append (apply string spaces) s))