#lang racket

(require rsound
         rackunit)

;; this compiles but probably still won't run, I see lots of unnecessary sample rate args

;; no testing required...
(module* test racket/base)

(provide (all-defined-out))

(define volume (make-parameter 0.5))
(define tempo (make-parameter 200))
(define measures (make-parameter 24))

(define beats-per-measure 4)
(define frames-per-second 44100)


(define sample-path "/Users/clements/Renoise2 Sample Library/Samples")

(define (rsound-badoverlay sound1 sound2)
  (unless (= (rsound-sample-rate sound1) (rsound-sample-rate sound2))
    (error 'rsound-mix "expected two sounds with the same sample rate, got sounds with sample rates ~s and ~s"
           (rsound-sample-rate sound1) (rsound-sample-rate sound2)))
  (define (left i) (+ (rs-ith/left sound1 i) (rs-ith/left sound2 i)))
  (define (right i) (+ (rs-ith/right sound1 i) (rs-ith/right sound2 i)))
  (signals->rsound (rs-frames sound1)
                   (rsound-sample-rate sound1)
                   left
                   right))

;; read the wav file, scale it down to avoid clipping
(define (sample-load path)
  (rs-scale 0.05 (rs-read path)))

(define misc08 (sample-load (build-path sample-path "Misc/Misc 08.wav")))
(define clap06 (sample-load (build-path sample-path "Clap/Clap 06.wav")))
(define hi-hat04 (sample-load (build-path sample-path "Hi Hats/HiHat 04.wav")))

(define square1 (sample-load (build-path sample-path
                                                "Single Cycle/AnaSquareEmu.01.wav")))

(define f (/ 44100 678))

(define square3 (sample-load (build-path sample-path
                                                "Single Cycle/AnaSquareEmu.03.wav")))

(define instrument (make-parameter square1))

(define (rs-overlay sound1 sound2)
  (rs-overlay* (list (list sound1 0) (list sound2 0))))


;; given an rsound and a duration in seconds, make enough copies of the rsound
;; (possibly less than 1) to make a sound of the given duration
(define (single-cycle->dur rsound dur)
  (let ()
    (define num-frames (round (* (rsound-sample-rate rsound) dur)))
    (define num-whole-copies (quotient num-frames (rs-frames rsound)))
    (define leftover-frames (remainder num-frames (rs-frames rsound)))
    (rs-append* (append 
                     (for/list ([i (in-range num-whole-copies)])
                       rsound)
                     (list (clip rsound 0 leftover-frames))))))

;; quick test case:
;; old-style-signal will fail...
(let* ([saw3 (signal->rsound 4 44100 (indexed-signal (lambda (x) (/ x 4))))]
       [extended-saw (single-cycle->dur saw3 0.01)])
  (check-equal? (rs-frames extended-saw) 441)
  (check-= (rs-ith/left extended-saw 402) 0.5 0.001))

(define (single-cycle->tone rsound native-pitch desired-pitch dur)
  (define resample-rate (/ desired-pitch native-pitch))
  (define resampled (resample resample-rate rsound))
  (single-cycle->dur resampled dur))

(define (single-cycle->note rsound native-pitch note-num dur)
  (define desired-pitch (midi-note-num->pitch note-num))
  (rs-overlay 
   (single-cycle->tone rsound native-pitch (* 1.01 desired-pitch) dur)
   (single-cycle->tone rsound native-pitch desired-pitch dur)))

(define (frac num)
  (- num (floor num)))


(define (note-num-sequence beats lon)
  (define beat-dur (/ 60 (tempo)))
  (define frames-per-beat (* frames-per-second beat-dur))  
  (define dur (* beat-dur beats))
  (define instr (instrument))
  (rs-append*
   (map (lambda (note-num)
          (if note-num
              (single-cycle->note instr f note-num dur)
              (silence 
               (round (* (rsound-sample-rate instr) dur))
               (rsound-sample-rate instr))))
        lon)))

(define (note-num-64ths lon) (note-num-sequence 1/16 lon))
(define (note-num-32nds lon) (note-num-sequence 1/8 lon))
(define (note-num-16ths lon) (note-num-sequence 1/4 lon))
(define (note-num-8ths lon) (note-num-sequence 1/2 lon))
(define (note-num-4ths lon) (note-num-sequence 1 lon))
(define (note-num-2s lon) (note-num-sequence 2 lon))

(define (transpose half-steps note-seq)
  (map (lambda (note-num) (+ half-steps note-num)) note-seq))


(define (n-times n pat)
  (apply append (for/list ([i (in-range n)]) pat)))

(define mintriad '(60 63 67))
(define minarpeg '(60 63 67 72))
(define melody1
  (note-num-8ths (n-times 20 (append mintriad minarpeg))))
(define melody2
  (rs-append 
   (note-num-8ths (n-times 4 '(75 #f 75 #f 74 #f 74 #f)))
   (note-num-8ths (n-times 4 '(79 75 79 75 77 74 77 74)))))

(define (random-elt l)
  (list-ref l (random (length l))))

(define minor-scale (list 60 62 63 65 67 68 70 72))
#;(define rand-minor-seq (for/list ([i (in-range 16)]) (random-elt minor-scale)))
(define rand-minor-seq (append (shuffle minor-scale) (shuffle minor-scale)))
(define melody3
  (note-num-64ths (n-times 16 rand-minor-seq)))
(define melody4
  (note-num-32nds (n-times 16 rand-minor-seq)))
(define melody5
  (note-num-16ths (n-times 16 (transpose -12 rand-minor-seq))))
(define melody6
  (note-num-8ths (n-times 16 (transpose -12 rand-minor-seq))))
(define melody7
  (note-num-4ths (n-times 12 (transpose -24 rand-minor-seq))))
(define melody8 
  (note-num-2s (n-times 7 (transpose -24 rand-minor-seq))))


#;(define melody-axel-f '(60 #f #f #f 
                        63 #f #f 60 
                        #f 60 65 #f
                        60 #f 58 #f
                        60 #f #f #f
                        67 #f #f 60 
                        #f 60 68 #f
                        67 #f 63 #f 
                        60 #f 67 #f 
                        72 #f 60 58 
                        #f 58 55 #f 
                        63 #f 60 #f))

#;(define s (note-num-8ths melody-axel-f))

(define (measure/beat->frame measure beat frames-per-beat)
  (define beats (+ (* measure beats-per-measure) (- beat 1)))
  (round (* beats frames-per-beat)))

(define (((on-beat frames-per-beat) beat) sound)
  (for/list ([measure (in-range (measures))])
    (list (rs-scale (volume) sound)
          (measure/beat->frame measure beat frames-per-beat))))


(define (on-beats beats)
  (define beat-dur (/ 60 (tempo)))
  (define frames-per-beat (* frames-per-second beat-dur))  
  (lambda (sound)
    (apply append (map (lambda (f) (f sound)) (map (on-beat frames-per-beat) beats)))))

(define on-1 (on-beats '(1)))
(define on-1-and (on-beats '(1 1.5)))
(define on-2 (on-beats '(2)))
(define on-1-3 (on-beats '(1 3)))
(define on-2-4 (on-beats '(2 4)))
(define on-3.5 (on-beats '(3.5)))
(define eighths  '(1 1.5 2 2.5 3 3.5 4 4.5))
(define on-eighths (on-beats eighths))
(define hi-hat-pattern (append  eighths '(4.75)))

(define (string->perc-pattern str)
  (for/list ([ch (string->list str)]
             [i (in-naturals)]
             #:when (not (eq? ch #\space)))
    (+ 1 (/ i 4))))

(check-equal? (string->perc-pattern ".   .  ..   .   ")
              (list 1 2 (+ 2 3/4) 3 4))

(define (on-str str)
  (on-beats (string->perc-pattern str)))

(define on-offbeats (on-beats '(1.5 2.5 3.5 4.5)))

(define (pl scripts)
  (rs-overlay* (apply append scripts)))

#;(define song
 (rs-overlay*
  (list (list (pl (list
                   #;(on-offbeats misc08)
                   #;       "1   2   3   4   "
                   ((on-str ". .   .   .     ") kick)
                   ((on-str "    .       .   ") clap06)
                   ((on-str ". . . . . . . ..") hi-hat04)
                   ((on-str "                ") misc08)
                   #;(on-1-3 kick)
                   #;(on-2-4 clap06)))
              0)
        #;(list melody3 (* measure-frames 4))
        #;(list melody4 (* measure-frames 8))
        #;(list melody5 (* measure-frames 12))
        #;(list melody6 (* measure-frames 16))
        #;(list melody7 (* measure-frames 20))
        #;(list melody8 (* measure-frames 24))
        (list melody1 (* measure-frames 4))
        (list melody2 (* measure-frames 8))
        (list melody2 (* measure-frames 16)))))

#;(rs-write song "/tmp/minor-key-beeps.wav")

#;(play song)


#;(pl #;(on-offbeats misc08)
    #;((on-beats '(1 1.5 2.5 3.5)) kick)
    #;(on-2-4 clap06)
    #;((on-beats hi-hat-pattern) hi-hat04)
    (on-1-3 kick)
    #;(on-2-4 clap06))

(define (delay-beats n)
  (define beat-dur (/ 60 (tempo)))
  (define frames-per-beat (* frames-per-second beat-dur))
  (* frames-per-beat n))
