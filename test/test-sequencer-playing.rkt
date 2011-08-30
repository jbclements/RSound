#lang racket

(require "../rsound.rkt"
         "../sequencer.rkt"
         "../util.rkt"
         mred)

(define tone (make-tone 330 0.2 30000 44100))
(define tone2 (make-tone 440 0.2 15000 44100))

(define unplayed-heap (make-unplayed-heap))
(queue-for-playing! unplayed-heap tone 0)
(queue-for-playing! unplayed-heap tone 44100)
(for ([i (in-range 50)])
  (queue-for-playing! unplayed-heap tone2 (+ 1000 (* i 22050))))


(define-values (signal/block last-time)
  (heap->signal/block unplayed-heap))

(define (g p)
  (queue-for-playing! unplayed-heap 
                      (make-harm3tone p 0.2 44100 44100)
                      (last-time)))

(signal/block-play signal/block 44100)


(define kbd-frame
  (new frame% [label "foo"]
       [width 200]
       [height 200]))

(define kbd-canvas%
  (class canvas%
    (define/override (on-char evt)
      (match (send evt get-key-code)
        [#\; (g (midi-note-num->pitch 60))]
        [#\o (g (midi-note-num->pitch 61))]
        [#\q (g (midi-note-num->pitch 62))]
        [#\e (g (midi-note-num->pitch 63))]
        [#\j (g (midi-note-num->pitch 64))]
        [#\k (g (midi-note-num->pitch 65))]
        [#\i (g (midi-note-num->pitch 66))]
        [#\x (g (midi-note-num->pitch 67))]
        [#\b (g (midi-note-num->pitch 69))]
        [other #f]))
    (super-new)))

(define kbd-canvas
  (new kbd-canvas% [parent kbd-frame]
       [min-width 200]
       [min-height 200]))

(send kbd-frame show #t)

