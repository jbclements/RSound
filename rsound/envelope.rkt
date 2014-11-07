#lang racket/base

(require "rsound.rkt"
         "common.rkt"
         "util.rkt"
         "network.rkt"
         (only-in racket/math pi)
         (only-in racket/list last))

(provide adsr
         #;adsr/exp
         envelope-signal
         sine-window
         hann-window)

;; unused, now...
(define (straight-line frames a b)
  (define (sig f) (+ a (* f (/ (- b a) frames))))
  (signal->rsound frames (indexed-signal sig)))

;; unused, now.... we'll see if we need exponential envelopes again.
#;(define (exp-line frames a b)
  (define a-log (log (max a 1e-2)))
  (define b-log (log (max b 1e-2)))
  (define (sig f) (exp (+ a-log (* f (/ (- b-log a-log) frames)))))
  (signal->rsound frames (indexed-signal sig)))



;; if total is less than 
#;(define (((adsr/helper line-maker) a ah d dh r) total)
  (clip (rs-append* 
         (list
          (line-maker a 0 ah)
          (line-maker d ah dh)
          (line-maker (max 0 (- total a d r)) dh dh)
          (line-maker r dh 0)))
        0 total))

;; adsr : frames value frames value frames -> frames -> rsound
(define ((adsr a ah d dh r) total)
  (signal->rsound
   total
   (envelope-signal `((0 0.0)
                      (,a ,ah)
                      (,(+ a d) ,dh)
                      (,(- total r) ,dh)
                      (,total 0.0)))))

#;(define adsr/exp (adsr/helper exp-line))


(module+ test
  (require rackunit)
  (check-= (rs-ith/left (straight-line 30 0.2 0.3) 15) 0.25 1e-3))


;; GOOD LORD... HOW MANY OF THESE DID I HAVE?

;; ADSR envelope (actually more of an ADS envelope)
#;(define (unused-adsr frames attack-len attack-height decay-len decay-height)
  (define slope1 (exact->inexact (/ attack-height attack-len)))
  (define slope2 (exact->inexact (/ (- decay-height attack-height) decay-len)))
  (define (ramp idx p)
    (cond [(<= idx attack-len) (+ p slope1)]
          [(<= idx decay-len) (+ p slope2)]
          [(<= idx frames) p]
          [else 0]))
  (network ()
           (frame ((simple-ctr 0 1)))
           (volume (ramp frame (prev volume 0.0)))))


;; envelope-signal : given a list of envelope-points, produce a signal that follows that envelope.
(define (envelope-signal lop)
  (unless (and (list? lop)
               (< 1 (length lop))
               (andmap env-point? lop))
    (raise-argument-error 'envelope-signal "list of two-element lists containing index and value of length > 1"
                          0 lop))
  (for ([p (map car lop)]
        [q (cdr (map car lop))])
    (when (<= q p)
      (raise-argument-error 'envelope-signal "list of envelope points with strictly increasing frame indices"
                            0 lop)))
  (unless (= (car (car lop)) 0)
    (raise-argument-error 'envelope-signal "list of envelope points whose first point has frame 0"
                          0 lop))
  ;; precompute the slopes, add a special last element which indicates termination.
  (define slopes
    (for/list ([p lop]
               [q (cdr lop)])
      (define slope (/ (- (cadr q) (cadr p))
                       (- (car q)  (car p))))
      (list (car p) (cadr p) slope)))
  (define halt-frame (car (last lop)))
  (define f 0)
  (lambda ()
    (define result
      (cond [(<= halt-frame f) 0.0]
            [else
             (when (and (pair? (cdr slopes))
                        (< (caadr slopes) f))
               (set! slopes (cdr slopes)))
             (define the-point (car slopes))
             (define point-f (car the-point))
             (define point-val (cadr the-point))
             (define point-slope (caddr the-point))
             (+ point-val (* point-slope (- f point-f)))]))
    (set! f (add1 f))
    result))

;; an envelope-point contains a frame-number and a real-valued envelope value
;; matches (list/c nonnegative-integer? number?)
(define (env-point? p)
  (and (pair? p)
       (nonnegative-integer? (car p))
       (pair? (cdr p))
       (number? (cadr p))
       (null? (cddr p))))

;; a simple sine envelope for use in windowing. total length is len + fade-in
(define (sine-window len fade-in)
  (when (< len fade-in)
    (raise-argument-error 'sine-window "len < fade-in" 0 len fade-in))
  (signal->rsound (+ fade-in len)
                  (indexed-signal
                   (lambda (t)
                     (cond [(< t fade-in) (/ (- 1 (cos (* pi (/ t fade-in)))) 2)]
                           [(< t len) 1]
                           [else (/ (+ 1.0 
                                       (cos (* pi (/ (- t len) fade-in)))) 2)])))))

(module+ test
  (define sw (sine-window 3000 400))
  (check-= (rs-ith/left sw 0) 0.0 1e-4)
  (check-= (rs-ith/left sw 200) 0.5 1e-4)
  (check-= (rs-ith/right sw 400) 1.0 1e-4)
  (check-= (rs-ith/right sw 1500) 1.0 1e-4)
  (check-= (rs-ith/right sw 3000) 1.0 1e-4)
  (check-= (rs-ith/right sw 3200) 0.5 1e-4)
  (check-= (rs-ith/right sw 3399) 0.0 1e-4)
  )

;; a "hann" window is a special case of the sine window where the fade-in is 
;; followed immediately by the fade-out. In other words, the envelope is just
;; a simple sine wave.
(define (hann-window len)
  (sine-window (floor (/ len 2)) (floor (/ len 2))))

(module+ test
  (define hw (hann-window 300))
  (check-equal? (rs-frames hw) 300)
  (check-= (rs-ith/left hw 0) 0.0 1e-4)
  (check-= (rs-ith/left hw 75) 0.5 1e-4)
  (check-= (rs-ith/right hw 150) 1.0 1e-4)
  (check-= (rs-ith/right hw 225) 0.5 1e-4)
  (check-= (rs-ith/left hw 299) (- 1.0 (cos (* 2 pi 1/300))) 1e-4)
  )