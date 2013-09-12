#lang racket

; By Mustafa Khafateh & John Clements 

(require rsound)

(require racket/runtime-path)

; rrhythm
; drum patterns: 16 steps, multiple samples.

; sample rate
(define sr 44100)

; beats per minute
(define bpm 130)

; number of steps in one loop
(define num-steps 12)

; number of steps in one beat
(define beat-steps 4)

(unless (integer? (/ num-steps beat-steps))
  (error 'num-steps
         "the number of steps (~s) is not divisible by the steps per beat (~s)."
         num-steps
         beat-steps))

;; the length of a beat (in seconds)
(define beat-length (/ 60 bpm))

; lenght of step in samples
(define sps (* (/ beat-length beat-steps) sr))

; overlays a bunch of rsounds at 0
(define  (rsound-overlay/z rsounds)
  (rsound-overlay* (map (lambda (l) (list l 0)) rsounds)))

;; loop-at: rsound number -> rsound
;; create a new rsound by overlaying the end of the first onto the beginning;
;; this allows creation of loops that overlap
(define (loop-at rsound loop-len)
  (let ([rlen (rs-frames rsound)])
    (unless (< 0 loop-len rlen)
      (error 'loop-at
             "expected a number of frames between 0 and the length of the rsound (~s)." 
             (rs-frames rsound)))
    (unless (<= (/ rlen 2) loop-len)
        (error 'loop-at
               (format
                (string-append
                 "the given loop-length (~s) is less than half the length of the "
                 "sound (~s). This would cause triple overlap.") 
                loop-len
                rlen)))
    (rsound-overlay/z (list (rsound-clip rsound 0 loop-len)
                            (rsound-clip rsound loop-len rlen)))))




; interprets the "switches" and overlays the rsound according to them
(define (s sound . switches)
  (rsound-overlay*   
   (for/list ([b (in-list switches)]
              [i (in-naturals)]
              #:when (= b 1))
     (list sound (round (* i sps))))
   
   ))

; load the samples
(define-runtime-path drum-samples "./drum-samples")

(define kick   (rsound-read (build-path drum-samples "bassdrum-synth.wav")))
(define ohihat (rsound-read (build-path drum-samples "ohihat.wav")))
(define chihat (rsound-read (build-path drum-samples "chihat.wav")))
(define clap   (rsound-read (build-path drum-samples "clap.wav")))
(define crash  (rsound-read (build-path drum-samples "crash_cymbal.wav")))


(define pattern1
  (rsound-overlay/z
   (list
    (s kick   1 0 0 0  1 0 0 0  1 0 0 0  1 0 0 0)
    (s ohihat 0 0 0 0  0 0 1 0  0 0 1 0  0 0 1 0)
    (s chihat 0 1 1 1  0 1 1 1  0 1 1 1  0 1 0 1)
    (s clap   0 0 0 0  1 0 0 0  0 0 0 0  1 0 0 0))))

(define pattern2
  (rsound-overlay/z
   (list
    (s kick     1 0 0 0  1 0 0 0  1 0 0 0  1 0 0 0)
    (s ohihat	0 0 1 0  0 0 1 0  0 0 1 0  0 0 1 0)
    (s chihat	0 1 1 0  0 0 1 1  0 1 0 1  0 1 0 1)
    (s clap     0 0 0 0  1 0 0 1  0 0 0 0  1 0 0 0))
                ;(s crash	0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
                ))

(define (random-row prob)
  (build-list num-steps (lambda (dc) (if (< (random) prob) 1 0))))

(define pattern3
  (rsound-overlay/z
   (list
    (apply s kick (random-row 0.2))
    (apply s chihat (random-row 0.8))
    (apply s ohihat (random-row 0.4))
    (apply s crash (random-row 0.1)))))

(define pat4
  (rsound-overlay/z
   (list
    (s kick   1 0 0  0 0 0  1 0 0  0 0 0)
    (s chihat 0 1 0  0 1 1  0 1 0  0 1 1))))


#;(define pattern3
	(rsound-overlay/z
         (list
          (s kick       1 0 0 0  1 0 0 0  1 0 0 0  1 0 0 0)
          (s ohihat	0 0 1 0  0 0 1 0  0 0 1 0  0 0 1 0)
          (s chihat	0 1 0 1  0 1 0 1  0 1 1 1  0 1 0 1)
          (s clap       0 0 0 0  1 0 0 0  0 0 0 0  1 0 0 0)
          (s crash	0 0 0 0  0 0 0 0  0 0 0 0  1 0 0 0))))

; overlays patterns in step with a pattern's length (num-steps * sps)

(define (overlay-patterns . patterns)
  (rsound-overlay*
   (for/list ([p (in-list patterns)]
              [i (in-naturals)])
     (list p (round (* i (* num-steps sps)))))))


(rsound-loop (loop-at (overlay-patterns pat4 pat4 pat4)
                      (floor (* sps num-steps 3))))





