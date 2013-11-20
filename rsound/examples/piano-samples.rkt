#lang racket

(require rsound/piano-tones
         rsound
         2htdp/universe
         2htdp/image)

(time (piano-tone 60))
(time (piano-tone 60))
(time (piano-tone 72))
(time (piano-tone 72))

;; the "piano key" keys, in order of half-step, starting from
;; C4.
(define keyseq "a,o.euyifdghtrnls-=")
(define starting-note 48)
;; turn these into a map from key to pitch
(define key-hash
  (apply hasheq (apply 
                 append
                 (for/list ([k (string->list keyseq)]
                            [i (in-range starting-note
                                         (+ starting-note 
                                            (string-length keyseq)))])
                   (list k i)))))


(define (both a b) b)

;; determine what note to play based on the key, and play it.
(define (play-key w k)
  (define s
    (cond [(= (string-length k) 1)
           (match (piano-tone
                   (hash-ref key-hash (string-ref k 0) #f)
                   #f)
             [#f -1]
             [note (both (play note)
                         (hash-ref key-hash (string-ref k 0) #f))])]
          [else "nothing to play"]))
  w)


(define (nth-triad-note center n)
  (cond [(= n 0) center]
        [(= n 1) (+ center 4)]
        [(= n 2) (+ center 7)]
        [(= n 3) (+ center 12)]))

(define (play-rand-note center)
  (play (piano-tone (nth-triad-note center (random 4))))
  center)

(define (change-center w k)
  (cond [(key=? k "1") 60]
        [(key=? k "2") 62]
        [(key=? k "3") 64]
        [(key=? k "4") 65]
        [(key=? k "5") 67]
        [else w]))

(define (draw-world w)
  (overlay (text (number->string w) 60 "black")
           (empty-scene 100 100)))

(draw-world 59)
(big-bang 60
          [on-key play-key]
          [to-draw draw-world])
;; start the program running.
#;(big-bang 60
          [on-tick play-rand-note 0.25]
          [on-key change-center]
          [to-draw draw-world])

