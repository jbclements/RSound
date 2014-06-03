#lang racket

(require rackunit/text-ui
         mred
         "../main.rkt")


(define tone (make-tone 330 0.2 30000))
(define tone2 (make-tone 440 0.2 15000))

(define ps (make-pstream))

(define (g p)
  (pstream-play ps 
                (signal->rsound 44100 
                                (signal-scale 0.2 (fixed-inputs harm3-wave p)))))
(define kbd-frame
  (new frame% [label "foo"]
       [width 200]
       [height 200]))

(define (temper-comparison char)
  (match char
    [#\1 (g 300)]
    [#\2 (g (* 9/8 300))]
    [#\3 (g (* 5/4 300))]
    [#\4 (g (* 4/3 300))]
    [#\5 (g (* 3/2 300))]
    [#\6 (g (* 2 300))]
    [#\7 (g (* 9/4 300))]
    [#\8 (g (* 5/2 300))]
    [#\9 (g (* 8/3 300))]
    [#\0 (g (* 3 300))]
    [#\[ (g (* 4 300))]
    [#\' (g 300)]
    [#\, (g (* (expt 2 2/12) 300))]
    [#\. (g (* (expt 2 4/12) 300))]
    [#\p (g (* (expt 2 5/12) 300))]
    [#\y (g (* (expt 2 7/12) 300))]
    [#\f (g (* (expt 2 12/12) 300))]
    [#\g (g (* (expt 2 14/12) 300))]
    [#\c (g (* (expt 2 16/12) 300))]
    [#\r (g (* (expt 2 17/12) 300))]
    [#\l (g (* (expt 2 19/12) 300))]
    [#\/ (g (* (expt 2 24/12) 300))]
    [other #f]))

(define independent-root 100)
(define fourth-root (* 4/3 independent-root))
(define fifth-root (* 3/2 independent-root))
(define (independent-scales char)
  (match char
    [#\1 (g (* 1 independent-root))]
    [#\2 (g (* 2 independent-root))]
    [#\3 (g (* 3 independent-root))]
    [#\4 (g (* 4 independent-root))]
    [#\5 (g (* 5 independent-root))]
    [#\6 (g (* 6 independent-root))]
    [#\7 (g (* 7 independent-root))]
    [#\8 (g (* 8 independent-root))]
    [#\9 (g (* 9 independent-root))]
    [#\0 (g (* 10 independent-root))]
    [#\[ (g (* 11 independent-root))]
    [#\] (g (* 12 independent-root))]
    [#\; (g (* 13 independent-root))]
    [#\q (g (* 14 independent-root))]
    [#\' (g fourth-root)]
    [#\, (g (* 2 fourth-root))]
    [#\. (g (* 3 fourth-root))]
    [#\p (g (* 4 fourth-root))]
    [#\y (g (* 5 fourth-root))]
    [#\f (g (* 6 fourth-root))]
    [#\g (g (* 7 fourth-root))]
    [#\c (g (* 8 fourth-root))]
    [#\r (g (* 9 fourth-root))]
    [#\l (g (* 10 fourth-root))]
    [#\a (g fifth-root)]
    [#\o (g (* 2 fifth-root))]
    [#\e (g (* 3 fifth-root))]
    [#\u (g (* 4 fifth-root))]
    [#\i (g (* 5 fifth-root))]
    [#\d (g (* 6 fifth-root))]
    [#\h (g (* 7 fifth-root))]
    [#\t (g (* 8 fifth-root))]
    [#\n (g (* 9 fifth-root))]
    [#\s (g (* 10 fifth-root))]
    [other #f]
    ))

(define kbd-canvas%
  (class canvas%
    (define/override (on-char evt)
      (independent-scales
       #;temper-comparison
       (send evt get-key-code)))
    (super-new)))

(define kbd-canvas
  (new kbd-canvas% [parent kbd-frame]
       [min-width 200]
       [min-height 200]))

(send kbd-frame show #t)

