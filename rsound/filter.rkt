#lang racket/base

(require (only-in racket pi)
         racket/match
         "rsound.rkt"
         "network.rkt"
         "filter-typed.rkt"
         "reverb.rkt"
         racket/flonum)

(provide fir-filter
         iir-filter
         dynamic-lti-signal
         lpf-sig
         lpf/dynamic
         (all-from-out "reverb.rkt"))

(define i (sqrt -1))
(define twopi (* 2 pi))

;; poly : a transfer function
;; coefficients : a list of coefficients to use in a transfer function
;; roots / zeros : places where the transfer function is zero
;; poles : places where the transfer function is infinite

;; FIR filters

;; fir-filter : (listof (list/c delay amplitude)) -> 1/1/network
;; filter the input signal using the delay values and amplitudes given for an FIR filter
(define (fir-filter params)
  (match params
    [`((,delays ,amplitudes) ...)
     (unless (andmap (lambda (d) (and (exact-integer? d) (<= 0 d))) delays)
       (raise-type-error 'fir-filter "exact integer delays greater than zero" 0 params))
     (unless (andmap real? amplitudes)
       (raise-type-error 'fir-filter "real number amplitudes" 0 params))
     ;; enough to hold delayed and current, rounded up to next power of 2:
     [define max-delay (up-to-power-of-two (+ 1 (apply max delays)))]
     ;; set up buffer to delay the signal
     [define delay-buf (make-vector max-delay 0.0)]
     [define next-idx 0]
     (define (wraparound-add1 idx)
       (define next (add1 idx))
       (cond [(<= max-delay next) 0]
             [else next]))
     (lambda (this-val)
       (vector-set! delay-buf next-idx this-val)
       (define result
         (for/fold ([sum 0])
           ([d (in-list delays)]
            [a (in-list amplitudes)])
           (+ sum (* a (vector-ref delay-buf (modulo (- next-idx d) max-delay))))))
       (set! next-idx (wraparound-add1 next-idx))
       result)]
    [other (raise-type-error 'fir-filter "(listof (list number number))" 0 params)]))





;; IIR filters

;; iir-filter : (listof (list/c delay amplitude)) -> 1/1/networkt
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
     (let* ([max-delay (up-to-power-of-two (+ 1 (apply max delays)))]
            ;; set up buffer to delay the signal
            [delay-buf (make-vector max-delay 0.0)]
            [next-idx 0])
       (lambda (this-val)
         ;; could be a lot faster:
         (let* ([new-val (for/fold ([sum this-val])
                           ([d (in-list delays)]
                            [a (in-list amplitudes)])
                           ;; FIXME: get rid of this modulo:
                           (+ sum (* a (vector-ref delay-buf 
                                                   (modulo (- next-idx d) max-delay)))))])
           (begin0
             new-val
             (vector-set! delay-buf next-idx new-val)
             (set! next-idx (modulo (add1 next-idx) max-delay))))))]
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

;; (nat nat -> 2/1/network)
;; accepts a tap-length, produces
;; a network with two inputs: the parameter signal and the input 
;; signal. The parameter signal must produce a vector of three
;; things: the input tap vector, the output tap vector, and the gain.
;; sadly, there's going to be some inevitable checking of vector
;; bounds here. 
;; NB: requires input & output taps to be of the same length. This 
;; is pretty normal.

;; this whole thing could be *way* more optimized.
(define (dynamic-lti-signal tap-len)
  (unless (< 0 tap-len)
    (raise-argument-error 'dynamic-lti-signal "number greater than zero" 0 tap-len))
  (define saved-input-buf (make-flvector tap-len))
  (define saved-output-buf (make-flvector tap-len))
  (define (wraparound idx)
       (cond [(<= tap-len idx) 0]
             [else idx]))
  (define next-idx 0)
  (lambda (fir-terms iir-terms gain this-val)
    ;; don't want to do this check every time...
    #;(unless (and (flvector? fir-terms)
                   (= (flvector-length fir-terms)
                      input-tap-len))
        (error 'dynamic-lti-signal 
               "expected vector of length ~s for fir-terms, got vector of length ~s"
               input-tap-len (flvector-length fir-terms)))
    ;; don't want to do this check every time....
    #;(unless (and (flvector? iir-terms)
                   (= (flvector-length iir-terms)
                      output-tap-len))
        (error 'dynamic-lti-signal 
               "expected vector of length ~s for iir-terms, got vector of length ~s"
               output-tap-len (flvector-length iir-terms)))
    
    (define fir-sum
      (for/fold ([sum 0.0])
        ([i (in-range tap-len)])
        (fl+ sum
             (fl* (flvector-ref fir-terms i)
                  (flvector-ref saved-input-buf 
                                (modulo (- next-idx i 1) tap-len))))))
    (define iir-sum
      (for/fold ([sum 0.0])
        ([i (in-range tap-len)])
        (fl+ sum
             (fl* (flvector-ref iir-terms i)
                  (flvector-ref saved-output-buf 
                                (modulo (- next-idx i 1) tap-len))))))
    (define next-val (fl* gain (exact->inexact this-val)))
    (flvector-set! saved-input-buf next-idx next-val)
    (define output-val (fl+ next-val (fl+ fir-sum iir-sum)))
    (flvector-set! saved-output-buf next-idx output-val)
    (set! next-idx (wraparound (add1 next-idx)))
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

;; A network that maps scale values into fir/iir vectors 
(define lpf-sig
  (lambda (theta)
    (when (not (<= min-scale-val theta max-scale-val))
      (error 'dynamic-lpf "scale value ~s not between ~s and ~s"
             theta
             min-scale-val
             max-scale-val))
    (define table-index (inexact->exact
                         (round
                          (/ (- theta min-scale-val)
                             perceptible-interval))))
    (define results
      (match (vector-ref coefficient-table table-index)
        [#f (define result-vec (lpf-tap-vectors theta))
            (vector-set! coefficient-table table-index result-vec)
            result-vec]
        [other other]))
    (values (vector-ref results 0)
            (vector-ref results 1)
            (vector-ref results 2))))

;; dynamic low-pass filter: the first argument is a signal that controls
;; the filter cutoff, the second is the signal being filtered.
(define lpf/dynamic  
  (network (lpf-control audio-sig)
           [(fir-terms iir-terms gain) <= lpf-sig lpf-control]
           [out <= (dynamic-lti-signal 4) fir-terms iir-terms gain audio-sig]))



