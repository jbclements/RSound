#lang racket

(require (planet clements/rsound))

#;(rsound-play 
 (rsound-append
  ding
  ding))

(define sample-path "/tmp/Renoise2 Sample Library/Samples")

;; read the wav file, scale it down by 10 to avoid clipping
(define (sample-load path)
  (rsound-read path))

(define kick (sample-load (build-path sample-path "Kicks/Kick 13.wav")))
(define misc08 (sample-load (build-path sample-path "Misc/Misc 08.wav")))
(define clap06 (sample-load (build-path sample-path "Clap/Clap 06.wav")))
(define hi-hat04 (sample-load (build-path sample-path "Hi Hats/HiHat 04.wav")))


(define tempo 60)
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
(define hi-hat-pattern (append  eighths '(4.55 4.6 4.65 4.8 4.9)))

(define on-offbeats (on-beats '(1.5 2.5 3.5 4.5)))

(define (pl . scripts)
  (rsound-play (rsound-overlay* (apply append scripts))))

#;(require (planet clements/rsound/draw))
#;(rsound-draw clap06)
(pl #;(on-offbeats misc08)
    ((on-beats '(1 1.5 2.5 3.5)) kick)
    (on-2-4 clap06)
    ((on-beats hi-hat-pattern) hi-hat04)
    #;(on-1-3 kick)
    #;(on-2-4 clap06))