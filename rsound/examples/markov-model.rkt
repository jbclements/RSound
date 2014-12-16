;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname markov-model) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))

(require rsound)


;; pick-trans : pick one of the transitions. ASSUMES THAT THE 
;; PROBABILITIES OF THE TRANSES ADD UP TO ONE
;; number list-of-transes -> idx
(define (pick-trans rand transes)
  (cond [(empty? transes) (error "ran out of transes!\n")]
        [else (local [(define first-prob
                        (trans-prob (first transes)))]
                (cond [(< rand first-prob) 
                       (trans-idx (first transes))]
                      [else 
                       (pick-trans (- rand first-prob)
                                   (rest transes))]))]))

;; generate a table of 4 random states
(define num-states 4)
(define (random-states)
  (build-list 4 random-state))

;; how many random transitions should come from each note?
(define NUM-TRANSES 4)

(define (random-state _)
  (make-state (make-pd (list-ref tones-list 
                                 (random (length tones-list)))
                       (list-ref durs-list
                                 (random (length durs-list))))
              (build-list NUM-TRANSES random-trans)))

;; make a random transition
(define (random-trans _)
  (make-trans (/ 1 NUM-TRANSES) (random num-states)))

;; the list of notes chosen from
(define tones-list (list 60 62 64 65 67 69 71 72))

;; the list of durations chosen from
(define durs-list (list 2 1 1 1 1/2 1/2 1/4))




;; a pd is [make-pd midi-note-num beats]
(define-struct pd (pitch duration))

;; a trans is [make-trans number number]
;; representing a transition to state 'idx' with
;; probability 'prob'
(define-struct trans (prob idx))

;; a state is [make-state pd transes]
(define-struct state (pd transes))


;; iter-state : number number model 
(define (iter-state dur cur-idx model)
  (cond [(<= dur 0) empty]
        [else 
         (local
           ((define cur-state (list-ref model cur-idx)))
           (cons (state-pd cur-state)
                 (iter-state (- dur (pd-duration
                                     (state-pd cur-state)))
                             (pick-trans (random) (state-transes cur-state))
                             model)))]))


;;; EXAMPLES:

(define sample-states
  (list (make-state (make-pd 60 1) (list (make-trans 1 1)))
        (make-state (make-pd 62 1) (list (make-trans 1 2)))
        (make-state (make-pd 64 1) (list (make-trans 1 3)))
        (make-state (make-pd 65 1) (list (make-trans 1 0)))))

(define awesome-states
  (list (make-state (make-pd 60 1) (list (make-trans 1 1)))
        (make-state (make-pd 62 1) (list (make-trans 1 2)))
        (make-state (make-pd 64 1) (list (make-trans 1 3)))
        (make-state (make-pd 65 1) (list (make-trans 1/2 0)
                                         (make-trans 1/4 1)
                                         (make-trans 1/4 2)))))

#;(random-seed 282887)
(define rand-states
  (random-states))

(define bass-states
  (list (make-state (make-pd 48 2) (list (make-trans 1 1)))
        (make-state (make-pd 55 2) (list (make-trans 1 2)))
        (make-state (make-pd 53 2) (list (make-trans 1 3)))
        (make-state (make-pd 55 2) (list (make-trans 1 0)))))

(check-expect (iter-state 0 0 sample-states) empty)
(check-expect (iter-state 1 0 sample-states) 
              (list (make-pd 60 1)))
(check-expect (iter-state 2 0 sample-states) 
              (list (make-pd 60 1)
                    (make-pd 62 1)))
(check-expect (iter-state 3 0 sample-states)
              (list (make-pd 60 1)
                    (make-pd 62 1)
                    (make-pd 64 1)))

(define BPM 120)
(define beat-secs (/ 60 BPM))
(define eighth-secs (* beat-secs 1/2))
(define sixteenth-secs (* beat-secs 1/4))
(define beat-frames (round (* 44100 beat-secs)))
(define quarter-frames beat-frames)


(define (make-note pd)
  (cond [(number? (pd-pitch pd))
         (rs-overlay (synth-note "main"
                                 43
                                 (pd-pitch pd)
                                 (round 
                                  (* quarter-frames 
                                     (pd-duration pd))))
                     (synth-note "main"
                                 43
                                 (* 1.00025 (pd-pitch pd))
                                 (round 
                                  (* quarter-frames 
                                     (pd-duration pd)))))]
        [else
         (silence (round
                   (* quarter-frames
                      (pd-duration pd))))]))




(define song
  (rs-overlay
   (rs-scale #i0.4
             (rs-append*
   (map make-note (iter-state 16 0 rand-states #;awesome-states))))
   (rs-scale #i0.4 
             (rs-append*
   (map make-note (iter-state 16 0 bass-states))))))

(define (looper sound)
  (local [(define len (rs-frames sound))]
    (network ()
      [ctr <= (loop-ctr len 1)]
      [out = (rs-ith/left sound ctr)])))


(define the-signal 
  (network ()
    [s <= (looper song)]
    [r <= reverb s]))

;(play (signal->rsound (* 10 44100) the-signal))
(signal-play the-signal)


