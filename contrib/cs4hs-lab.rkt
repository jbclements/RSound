#lang racket

(require (planet clements/rsound))

#;(rsound-play 
 (rsound-append
  ding
  ding))

(define sample-path "/Users/clements/Renoise2 Sample Library/Samples")

(define (rsound-scale scale sound)
  (define (left i) (* scale (rsound-ith/left sound i)))
  (define (right i) (* scale (rsound-ith/right sound i)))
  (funs->stereo-rsound (rsound-frames sound)
                       (rsound-sample-rate sound)
                       left
                       right))

;; read the wav file, scale it down to avoid clipping
(define (sample-load path)
  (rsound-scale 0.05 (rsound-read path)))

(define kick (sample-load (build-path sample-path "Kicks/Kick 13.wav")))
(define misc08 (sample-load (build-path sample-path "Misc/Misc 08.wav")))
(define clap06 (sample-load (build-path sample-path "Clap/Clap 06.wav")))
(define hi-hat04 (sample-load (build-path sample-path "Hi Hats/HiHat 04.wav")))

(define anasquareemu03 (sample-load (build-path sample-path
                                                "Single Cycle/AnaSquareEmu.03.wav")))


(define (resample factor sound)
  (define (left i) (rsound-ith/left sound (* factor i)))
  (define (right i) (rsound-ith/right sound (* factor i)))
  (funs->stereo-rsound (round (/ (rsound-frames sound) factor))
                       (rsound-sample-rate sound)
                       left
                       right))

(define asqhi (resample 2.0 anasquareemu03))

;; given an rsound and a duration in seconds, make enough copies of the rsound
;; (possibly less than 1) to make a sound of the given duration
(define (single-cycle->tone rsound dur)
  (let ()
    (define num-frames (* (rsound-sample-rate rsound) dur))
    (define num-whole-copies (quotient num-frames (rsound-frames rsound)))
    (define leftover-frames (remainder num-frames (rsound-frames rsound)))
    (rsound-append* (append 
                     (for/list ([i (in-range num-whole-copies)])
                       rsound)
                     (list (rsound-clip rsound 0 leftover-frames))))))



(define (frac num)
  (- num (floor num)))

(define anasquareemu03/1sec
  (single-cycle->tone anasquareemu03 1.0))

#;(require (planet clements/rsound/draw))
#;(rsound-draw anasquareemu03)
#;(vector-draw/real/imag (rsound-fft/left anasquareemu03))
(rsound-frames anasquareemu03/1sec)
#;(rsound-play anasquareemu03/1sec)
(rsound-play (single-cycle->tone asqhi 1.0))

(define tempo 240)
(define measures 10)
(define beats-per-measure 4)
(define frames-per-second 44100)

(define frames-per-beat (* frames-per-second (/ 60 tempo)))

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
(define hi-hat-pattern (append  eighths #;'(4.75)))

(define on-offbeats (on-beats '(1.5 2.5 3.5 4.5)))

(define (pl . scripts)
  (rsound-play (rsound-overlay* (apply append scripts))))

#;(pl #;(on-offbeats misc08)
    ((on-beats '(1 1.5 2.5 3.5)) kick)
    (on-2-4 clap06)
    ((on-beats hi-hat-pattern) hi-hat04)
    #;(on-1-3 kick)
    #;(on-2-4 clap06))

#;(pl #;(on-offbeats misc08)
    #;((on-beats '(1 1.5 2.5 3.5)) kick)
    #;(on-2-4 clap06)
    #;((on-beats hi-hat-pattern) hi-hat04)
    (on-1-3 kick)
    #;(on-2-4 clap06))