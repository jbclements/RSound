#lang racket

(require 2htdp/universe
         2htdp/image
         rsound
         rackunit
         rsound/draw)

(define ps (make-pstream))

(define NUM-BANDS 10)

(define vocal-band-sounds
  (for/list ([i (in-range NUM-BANDS)])
    (rs-scale
     2.0
     (rs-read (build-path "/tmp"
                          (~a "vocband-"i".wav"))))))
(define vocal-sum (rs-overlay* vocal-band-sounds))

(define piano-band-sounds
  (for/list ([i (in-range NUM-BANDS)])
    (rs-scale
     7.0
     (rs-read (build-path "/Users/clements/brinckerhoff.org/public_html/tmp/"
                          (~a "band-"i".wav"))))))
(define piano-sum (rs-overlay* piano-band-sounds))

(define band-sounds vocal-band-sounds)

;; a world is a list of booleans, representing whether the corresponding
;; harmonic is enabled.

;; draw a world
;; world -> image
(define (draw-world w)
  (apply
   beside
   (map (λ (b) (if b ON-RECT OFF-RECT)) w)))

(define RECT-WIDTH 100)
(define RECT-HEIGHT 100)
(define ON-COLOR (color (+ 128 (random 128))
                        (+ 128 (random 128))
                        (+ 128 (random 128))))
(define OFF-COLOR (color (random 128)
                         (random 128)
                         (random 128)))
(define ON-RECT (rectangle RECT-WIDTH RECT-HEIGHT "solid" ON-COLOR))
(define OFF-RECT (rectangle RECT-WIDTH RECT-HEIGHT "solid" OFF-COLOR))

(check-equal? (draw-world '(#t #t #f))
              (beside ON-RECT ON-RECT OFF-RECT))


;; given a mouse click, toggle the appropriate box
;; world number number mouse-event -> world
(define (handle-mouse w x y evt)
  (cond [(string=? evt "button-down") (toggle-box w x)]
        [else w]))

;; given a list of booleans and an x offset, toggle the appropriate
;; box
(define (toggle-box lob x)
  (cond [(empty? lob)
         (raise-argument-error 'toggle-box
                               "x coordinate not too far right"
                               1 lob x)]
        [(< x RECT-WIDTH) (cons (not (first lob)) (rest lob))]
        [else (cons (first lob) (toggle-box (rest lob) (- x RECT-WIDTH)))]))

(check-equal? (toggle-box '(#f #f #t) (/ RECT-WIDTH 2)) '(#t #f #t))
(check-equal? (toggle-box '(#f #f #t) (* RECT-WIDTH 3/2)) '(#f #t #t))

;; on space, assemble and play the appropriate sound.
(define (key-handler w k)
  (cond [(string=? k " ")
         (both (pstream-play ps (overlay-sounds w band-sounds))
               w)]
        [(string=? k "0")
         (kbd-toggle w 9)]
        [(member k (map string (string->list "123456789")))
         (kbd-toggle w (sub1 (string->number k)))]
        [else w]))

;; given a number from 0 to 9, toggle the corresponding element of
;; the list
;; list-of-boolean nat-num -> list-of-boolean
(define (kbd-toggle lob n)
  (cond [(empty? lob)
         (raise-argument-error 'kbd-toggle
                               "index less than length of list"
                               1 lob n)]
        [(= n 0) (cons (not (first lob)) (rest lob))]
        [else (cons (first lob) (kbd-toggle (rest lob) (sub1 n)))]))


(check-equal? (kbd-toggle '(#f #t #t #f) 2)
              '(#f #t #f #f))

;; given a list of booleans and a list of sounds, overlay the sounds
;; for which the corresponding element is true
(define (overlay-sounds lob los)
  (cond [(empty? lob) (silence 1)]
        [else
         (cond [(first lob) (rs-overlay (first los)
                                        (overlay-sounds (rest lob)
                                                        (rest los)))]
               [else (overlay-sounds (rest lob) (rest los))])]))

(check-equal? (overlay-sounds '(#f #t #t) band-sounds)
              (rs-overlay (second band-sounds)
                          (third band-sounds)))


;; bleah
(define (both a b) b)

(define (draw-all-bands)
 (for/list ([s (in-list band-sounds)])
  (rs-draw s)))

(define (main)
  (big-bang (for/list ([i (in-range NUM-BANDS)]) #t)
            [on-mouse handle-mouse]
            [to-draw draw-world]
            [on-key key-handler]))