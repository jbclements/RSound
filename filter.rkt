#lang racket/base

(require (only-in racket pi)
         racket/match
         "rsound.rkt"
         "filter-typed.rkt"
         racket/flonum)

(provide (except-out (all-defined-out)
                      twopi i))

(define i (sqrt -1))
(define twopi (* 2 pi))

;; poly : a transfer function
;; coefficients : a list of coefficients to use in a transfer function
;; roots / zeros : places where the transfer function is zero
;; poles : places where the transfer function is infinite

;; FIR filters

;; fir-filter : (listof (list/c delay amplitude)) -> signal -> signal
;; filter the input signal using the delay values and amplitudes given for an FIR filter
(define (fir-filter params)
  (match params
    [`((,delays ,amplitudes) ...)
     (unless (andmap (lambda (d) (and (exact-integer? d) (<= 0 d))) delays)
       (raise-type-error 'fir-filter "exact integer delays greater than zero" 0 params))
     (unless (andmap real? amplitudes)
       (raise-type-error 'fir-filter "real number amplitudes" 0 params))
     (lambda (signal)
       ;; enough to hold delayed and current, rounded up to next power of 2:
       (let* ([max-delay (up-to-power-of-two (+ 1 (apply max delays)))]
              ;; set up buffer to delay the signal
              [delay-buf (make-vector max-delay 0.0)]
              [next-idx 0]
              ;; ugh... we must be called sequentially:
              [last-t -1])
         (lambda (t)
           (unless (= t (add1 last-t))
             (error 'fir-filter "called with t=~s, expecting t=~s. Sorry about that limitation." 
                    t
                    (add1 last-t)))
           (let ([this-val (signal t)])
             (begin
               (vector-set! delay-buf next-idx this-val)
               (define result
                 (for/fold ([sum 0])
                   ([d (in-list delays)]
                    [a (in-list amplitudes)])
                   (+ sum (* a (vector-ref delay-buf (modulo (- next-idx d) max-delay))))))
               (set! last-t (add1 last-t))
               (set! next-idx (modulo (add1 next-idx) max-delay))
               result)))))]
    [other (raise-type-error 'fir-filter "(listof (list number number))" 0 params)]))





;; IIR filters

;; iir-filter : (listof (list/c delay amplitude)) -> signal -> signal
;; filter the input signal using the delay values and amplitudes given for an IIR filter
;; the only difference here is that we put the final result in the delay line, rather than
;; the input signal.
(define (iir-filter params)
  (match params
    [`((,delays ,amplitudes) ...)
     (unless (andmap (lambda (d) (and (exact-integer? d) (< 0 d))) delays)
       (raise-type-error 'iir-filter "exact integer delays greater than zero" 0 params))
     (unless (andmap real? amplitudes)
       (raise-type-error 'iir-filter "real number amplitudes" 0 params))
     (lambda (signal)
       (let* ([max-delay (up-to-power-of-two (+ 1 (apply max delays)))]
              ;; set up buffer to delay the signal
              [delay-buf (make-vector max-delay 0.0)]
              [next-idx 0]
              ;; ugh... we must be called sequentially:
              [last-t -1])
         (lambda (t)
           (unless (= t (add1 last-t))
             (error 'fir-filter "called with t=~s, expecting t=~s. Sorry about that limitation." 
                    t
                    (add1 last-t)))
           (let* ([this-val (signal t)]
                  [new-val (for/fold ([sum this-val])
                                     ([d (in-list delays)]
                                      [a (in-list amplitudes)])
                                     (+ sum (* a (vector-ref delay-buf (modulo (- next-idx d) max-delay)))))])
             (begin0
               new-val
               (vector-set! delay-buf next-idx new-val)
               (set! last-t (add1 last-t))
               (set! next-idx (modulo (add1 next-idx) max-delay)))))))]
    [other (raise-type-error 'iir-filter "(listof (list number number))" 0 params)]))

;; lti-filter : rsound (listof (list/c number? number?)) (listof (list/c number? number?)) -> rsound
;; given coefficients for an FIR and an IIR filter, apply
;; the given filter to the sound.
#;(define (lti-filter gain fir-coefficients iir-coefficients)
  (unless (real? gain)
    (raise-type-error 'lti-filter "real number" 0 gain fir-coefficients
                      iir-coefficients))
  (unless (and (list? fir-coefficients)
               (andmap (lambda (x) (and (list? x)
                                        (= (length x) 2)
                                        (nonnegative-integer? (car x))
                                        (real? (cadr x))))
                       fir-coefficients))
    (raise-type-error 'lti-filter "list of delays and coefficients" 1 
                      snd fir-coefficients iir-coefficients))
  (unless (and (list? iir-coefficients)
               (andmap (lambda (x) (and (list? x)
                                        (= (length x) 2)
                                        (nonnegative-integer? (first x))
                                        (real? (second x))))
                       iir-coefficients))
    (raise-type-error 'lti-filter "list of delays and coefficients" 2
                      snd fir-coefficients iir-coefficients))
  ;; must normalize, include gain...
  (define the-fir (fir-filter fir-coefficients))
  (define the-iir (iir-filter iir-coefficients))
  (signals->rsound (rs-frames snd)
                   (the-iir (the-fir (rsound->signal/left snd)))
                   (the-iir (the-fir (rsound->signal/right snd)))))

(define filter-param-update-interval 32)

;; we want to be able to change the filter dynamically...

;; the param-signal must accept a time and return three values: the 
;; fir terms, the iir terms, and the gain. The input and output-tap-len
;; specify the length of the vectors used for these terms. Then
;; there's the input signal....
(define (dynamic-lti-signal param-signal input-tap-len output-tap-len
                            input-signal)
  (define input-buf-len (max 1 input-tap-len))
  (define output-buf-len (max 1 output-tap-len))
  ;; enough to hold delayed and current, rounded up to next power of 2:
  (define saved-input-buf (make-flvector input-buf-len))
  (define saved-output-buf (make-flvector output-buf-len))
  (define next-idx 0)
  ;; ugh... we must be called sequentially:
  (define last-t -1)
  (define saved-fir-terms #f)
  (define saved-iir-terms #f)
  (define saved-gain #f)
  (lambda (t)
    (when (< t (add1 last-t))
      (error 'fir-filter "called with t=~s, expecting t >= ~s. out of order!." 
             t
             (add1 last-t)))
    (when (< (add1 last-t) t)
      (fprintf (current-error-port) "forward jump; resetting tap buffers.\n")
      (for ([i input-buf-len]) (flvector-set! saved-input-buf i 0.0))
      (for ([i output-buf-len]) (flvector-set! saved-output-buf i 0.0))
      (set! last-t (sub1 t)))
    ;; only update the filter parameters every 32 samples
    (when (= (modulo t filter-param-update-interval) 0)
      (define-values (fir-terms iir-terms gain) (param-signal t))
      (unless (and (flvector? fir-terms)
                   (= (flvector-length fir-terms)
                      input-tap-len))
        (error 'dynamic-lti-signal 
               "expected vector of length ~s for fir-terms, got vector of length ~s"
               input-tap-len (flvector-length fir-terms)))
      (unless (and (flvector? iir-terms)
                   (= (flvector-length iir-terms)
                      output-tap-len))
        (error 'dynamic-lti-signal 
               "expected vector of length ~s for iir-terms, got vector of length ~s"
               output-tap-len (flvector-length iir-terms)))
      (set! saved-fir-terms fir-terms)
      (set! saved-iir-terms iir-terms)
      (set! saved-gain gain))
    
    (define fir-sum
      (for/fold ([sum 0.0])
        ([i (in-range input-tap-len)])
        (fl+ sum
             (fl* (flvector-ref saved-fir-terms i)
                  (flvector-ref saved-input-buf 
                                (modulo (- t i 1) input-buf-len))))))
    (define iir-sum
      (for/fold ([sum 0.0])
        ([i (in-range output-tap-len)])
        (fl+ sum
             (fl* (flvector-ref saved-iir-terms i)
                  (flvector-ref saved-output-buf 
                                (modulo (- t i 1) output-buf-len))))))
    (define next-val (fl* saved-gain (exact->inexact (input-signal t))))
    (flvector-set! saved-input-buf (modulo t input-buf-len) next-val)
    (define output-val (fl+ next-val (fl+ fir-sum iir-sum)))
    (flvector-set! saved-output-buf (modulo t output-buf-len) output-val)
    (set! last-t (add1 last-t))
    output-val))

(define max-scale-val 3.0)
(define min-scale-val 0.00)
(define perceptible-interval 0.01)
(define coefficient-table (make-vector (inexact->exact
                                        (floor 
                                         (/ (- max-scale-val
                                               min-scale-val) 
                                            perceptible-interval))) 
                                       #f))

;; dynamic low-pass filter: the first argument is a signal that controls
;; the filter cutoff, the second is the signal being filtered.
(define (lpf/dynamic scale-signal input-signal)
  (dynamic-lti-signal
   (lambda (t)
     (define scale (scale-signal t))
     (when (not (<= min-scale-val scale max-scale-val))
       (error 'dynamic-lpf "scale value ~s not between ~s and ~s"
              scale
              min-scale-val
              max-scale-val))
     (define table-index (inexact->exact
                          (round
                           (/ (- scale min-scale-val)
                              perceptible-interval))))
     (define tap-mults
       (match (vector-ref coefficient-table table-index)
         [#f (define coefficients (lpf-coefficients scale))
             (define new-table-entry 
               (apply flvector 
                      (map (lambda (x) (* x -1.0))
                           coefficients)))
             (vector-set! coefficient-table table-index new-table-entry)
             new-table-entry]
         [other other]))
     (define gain (+ 1.0 (fl- 0.0 (flvector-sum tap-mults))))
     (values (flvector)
             tap-mults
             gain))
   0 4
   input-signal))

(define (flvector-sum vec)
  (for/fold ([sum 0.0]) ([f (in-flvector vec)]) (fl+ sum f)))


;; it looks like 1/100 is close enough not to notice. This
;; is totally a guess on my part


