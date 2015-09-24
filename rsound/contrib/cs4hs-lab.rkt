#lang racket

(require (except-in "../main.rkt" kick resample)
         rackunit)

#;(play 
 (rs-append
  ding
  ding))

(define sample-path "/Users/clements/Renoise2 Sample Library/Samples")

;; read the wav file, scale it down to avoid clipping
(define (sample-load path)
  (rs-scale 0.05 (rs-read path)))

(define kick (sample-load (build-path sample-path "Kicks/Kick 13.wav")))
(define misc08 (sample-load (build-path sample-path "Misc/Misc 08.wav")))
(define clap06 (sample-load (build-path sample-path "Clap/Clap 06.wav")))
(define hi-hat04 (sample-load (build-path sample-path "Hi Hats/HiHat 04.wav")))

(define anasquareemu03 (sample-load (build-path sample-path
                                                "Single Cycle/AnaSquareEmu.01.wav")))




#;(define (rsound-overlay sound1 sound2)
  (rsound-overlay* (list (list sound1 0) (list sound2 0))))


(define (resample factor sound)
  (define (left i) (rs-ith/left sound (round (* factor i))))
  (define (right i) (rs-ith/right sound (round (* factor i))))
  (signals->rsound (round (/ (rs-frames sound) factor))
                       (rsound-sample-rate sound)
                       left
                       right))

(define asqhi (resample 2.0 anasquareemu03))

;; given an rsound and a duration in seconds, make enough copies of the rsound
;; (possibly less than 1) to make a sound of the given duration
(define (single-cycle->tone rsound dur)
  (let ()
    (define num-frames (round (* (rsound-sample-rate rsound) dur)))
    (define num-whole-copies (quotient num-frames (rs-frames rsound)))
    (define leftover-frames (remainder num-frames (rs-frames rsound)))
    (rs-append* (append 
                     (for/list ([i (in-range num-whole-copies)])
                       rsound)
                     (list (clip rsound 0 leftover-frames))))))

(let* ([saw3 (signal->rsound 4 44100 (lambda (x) (/ x 4)))]
       [extended-saw (single-cycle->tone saw3 0.01)])
  (check-equal? (rs-frames extended-saw) 441)
  (check-= (rs-ith/left extended-saw 402) 0.5 0.001))

(define (single-cycle->note rsound native-pitch note-num dur)
  (let ()
    (define desired-pitch (midi-note-num->pitch note-num))
    (define resample-rate (/ desired-pitch native-pitch))
    (define resampled (resample resample-rate rsound))
    (single-cycle->tone resampled dur)))

(define (frac num)
  (- num (floor num)))

(define anasquareemu03/1sec
  (single-cycle->tone anasquareemu03 1.0))

(require "../draw.rkt")
(rs-draw anasquareemu03)
#;(vector-draw/real/imag (rs-fft/left anasquareemu03))
(rs-frames anasquareemu03/1sec)
#;(play anasquareemu03/1sec)
#;(play (single-cycle->tone asqhi 1.0))

(define tempo 200)
(define measures 36)
(define beats-per-measure 4)
(define frames-per-second 44100)

(define beat-dur (/ 60 tempo))
(define frames-per-beat (* frames-per-second beat-dur))

(define measure-frames (* frames-per-beat beats-per-measure))
(define 8th-note-dur (/ beat-dur 2))

(define (note-num-sequence beats lon)
  (define dur (* beat-dur beats))
  (rs-append*
   (map (lambda (note-num)
          (if note-num
              (single-cycle->note anasquareemu03 f note-num dur)
              (silence 
               (round (* (rsound-sample-rate anasquareemu03) dur))
               (rsound-sample-rate anasquareemu03))))
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
(define f (/ 44100 678))
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



(define (measure/beat->frame measure beat)
  (define beats (+ (* measure beats-per-measure) (- beat 1)))
  (round (* beats frames-per-beat)))

(define ((on-beat beat) sound)
  (for/list ([measure (in-range measures)])
    (list sound (measure/beat->frame measure beat))))

(define ((on-beats beats) sound)
  (apply append (map (lambda (f) (f sound)) (map on-beat beats))))

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

(define song
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
        (list melody2 (* measure-frames 8)))))

#;(rs-write song "/tmp/slowing-down-song.wav")

(play song)

#;(play
 (pl (list ((on-str "| | | | | | | ||") hi-hat04)
           ((on-str "| |     |       ") kick)
           ((on-str "    |       |   ") clap06)
           ((on-str "            |   ") ding))))

#;(pl #;(on-offbeats misc08)
    #;((on-beats '(1 1.5 2.5 3.5)) kick)
    #;(on-2-4 clap06)
    #;((on-beats hi-hat-pattern) hi-hat04)
    (on-1-3 kick)
    #;(on-2-4 clap06))