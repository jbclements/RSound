#lang racket

;; hastily assembled demo for Foaad Khosmood's class

(require rsound
         rsound/draw
         "quantize-looper.rkt"
         ffi/vector
         
         2htdp/universe
         2htdp/image)

(define song-chunk
  (rs-read/clip
   "/Users/clements/clements/teaching/video-game-music/earworm-2011.wav"
   0 (* 44100 30)))

(go song-chunk)

(define (my-draw w)
  (empty-scene 320 320))

(define key-table (map (lambda (ch) (string ch))
                       (string->list "123456789")))
(define t-key-table (map (lambda (ch) (string ch))
                         (string->list "',.pyfgcrl")))

(define (my-key w k)
  (cond [(equal? k "x")
         (stop)]
        [(member k key-table) 
         (update-q! (sub1 (string->number k)))]
        [(member k t-key-table)
         =>
         (lambda (k)
           (update-time-q! (- (length t-key-table) (length k))))])
  w)

(big-bang #f
          (to-draw my-draw)
          (on-key my-key))

#|
#;(play song-chunk)

(define msr mono-signal->rsound)

(define (quantize q num)
  (/ (/ (floor (* q (* num 32768.0))) q) 32768.0))

;; given a sound and a multiplier, do "fake resampling"
;; by choosing a rounded sample from the original

;; given a factor and a sound, resample the sound (using simple rounding)
;; to obtain a new one. Using e.g. factor of 2 will make the sound one
;; octave higher and half as long.
(define (my-resample sound new-freq q)
  (define factor (let ([ans (/ new-freq (rsound-sample-rate sound))])
                   (printf "~s\n" ans)
                   ans))
  (define (left i) (quantize 
                    q
                    (rs-ith/left sound 
                                 (inexact->exact (floor 
                                                  (/ (floor (* factor i))
                                                     factor))))))
  (define (right i) (quantize
                     q
                     (rs-ith/right sound 
                                   (inexact->exact (floor
                                                    (/ (floor (* factor i)) 
                                                       factor))))))
  (parameterize ([default-sample-rate 
                   (rsound-sample-rate sound)])
    (signals->rsound (rs-frames sound)
                     left
                     right)))


(define start-clip (* 10 44100))

(rs-draw (clip song-chunk start-clip (+ start-clip 500)))

(define s (my-resample song-chunk 44100 1.0))

(rs-draw (clip s start-clip (+ start-clip 500)))
(play s)

|#