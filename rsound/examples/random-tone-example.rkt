#lang racket

;; play a bunch of long tones; each one is 4 seconds long, fades in and out
;; slowly, and consists of two waveforms from Adventure Kid's samples, 
;; differing in pitch by a small random amount. The notes are chosen from 
;; a scale built on a specified set of intervals. The code is written in
;; a somewhat HTDP-friendly way. 

(require rsound)
(require rsound/envelope)
(require rackunit)

(define SR 44100)
(define (s sec) (* SR sec))

(define major-triad-intervals (list 4 3 5))
(define major-scale-intervals (list 2 2 1 2 2 2 1))
(define micro-tonal-intervals (list 0.25))
(define intervals major-scale-intervals)

;; given a list of intervals, start at the given
;; note and build a scale by jumping using the given
;; intervals (repeated) until reaching or passing the
;; given stop note.
(define (build-scale intervals start stop)
  (build-scale-helper intervals intervals start stop))

;; given a list of remaining intervals *and* the original
;; list of intervals and the start and stop, build
;; the scale as described above
(define (build-scale-helper intervals orig-intervals start stop)
  (cond [(>= start stop) empty]
        [else
         (cons start
         (cond [(empty? intervals)
                ;; time to start the loop again:
                (build-scale-helper
                 (rest orig-intervals)
                 orig-intervals
                 (+ start (first orig-intervals))
                 stop)]
               [else 
                ;; continue with this set of intervals:
                (build-scale-helper
                 (rest intervals)
                 orig-intervals
                 (+ start (first intervals))
                 stop)]))]))

;; start = stop, no notes:
(check-equal? (build-scale-helper (list 2 19) (list 1 2 19) 34 34)
              empty)
;; only two notes
(check-equal? (build-scale-helper (list 2 19) (list 1 2 19) 34 37)
              (list 34 36))
;; four notes (will wrap)
(check-equal? (build-scale-helper (list 2 19) (list 1 2 19) 34 58)
              (list 34 36 55 56))


(define scale-notes (build-scale intervals 36 80))

(define ATTACK-TIME (s 1))
(define ENVELOPE-LEVEL 0.05)
(define SUSTAIN-LEVEL 0.05)
(define RELEASE-TIME (s 2))
(define NOTE-LEN (s 4))
(define the-env ((adsr ATTACK-TIME ENVELOPE-LEVEL (s 0.2)
                       SUSTAIN-LEVEL RELEASE-TIME)
                 NOTE-LEN))


;; clear the decks to avoid long GC pauses:
(collect-garbage)
(collect-garbage)
(collect-garbage)

;; define a pstream with a long buffer to tolerate
;; GC pauses.
(define ps (make-pstream #:buffer-time 0.3))

;; how many notes to play:
(define NUM-NOTES 60)
;; how far apart to space them:
(define PLAY-INTERVAL (s 1))
;; lead time to get notes queued
(define LEAD-TIME (s 10))

;; queue up a bunch of random notes:
;; number -> pstream
(define (go notes-queued)
  (cond [(= notes-queued NUM-NOTES) ps]
        [else
         (local
           [
            (define instr (add1 (random 90)))
            (define tone
              (list-ref scale-notes (random (length scale-notes)))
              #;(+ 30 (random 50)))
            (define offby (/ (random 5) 30))
            (define _1 (printf "instr: ~v tone: ~v offby: ~v \n"
                               instr tone offby))
            (define sound
              (time
               (rs-mult the-env
                        (rs-overlay
                         (synth-note/raw "main" instr tone (s 5))
                         (synth-note/raw "main" (add1 instr) (+ tone offby) (s 5))))))
            ]
           (begin
             (pstream-queue
              ps
              sound
              (* PLAY-INTERVAL notes-queued))
             (go (add1 notes-queued))))]))

(go 0)

;; when running at the command-line, don't exit right away:
(sleep 60)
