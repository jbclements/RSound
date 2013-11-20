#lang racket/base

(require "main.rkt"
         ffi/unsafe
         ffi/vector
         rackunit
         racket/match
         racket/list)

(define (loop->signal/block/unsafe sound-start-stop-box)
  (match-define (list current-sound current-sound-start-offset current-sound-finish-offset)
    (check-sound-stop-start (unbox sound-start-stop-box)))
  (define current-sound-start 0)
  (define (signal/block/unsafe ptr frames idx)
    (define t (* frames idx))
    (define sound-frames (- current-sound-finish-offset
                            current-sound-start-offset))
    (define used-frames (- t current-sound-start))
    (define frames-to-copy (min frames (- sound-frames used-frames)))
    (define src-ptr (ptr-add (s16vector->cpointer (rsound-data current-sound))
                             (* (+ current-sound-start-offset used-frames) CHANNELS)
                             _sint16))
    (memcpy ptr src-ptr (* frames-to-copy CHANNELS) _sint16)
    (when (< frames-to-copy frames)
      ;; start next sound
      (match-define (list new-current-sound 
                          new-current-sound-start-offset
                          new-current-sound-finish-offset)
        (check-sound-stop-start (unbox sound-start-stop-box)))
      (set! current-sound new-current-sound)
      (set! current-sound-start-offset new-current-sound-start-offset)
      (set! current-sound-finish-offset new-current-sound-finish-offset)
      (set! current-sound-start (+ t frames-to-copy))
      (define new-sound-len (- new-current-sound-finish-offset
                               new-current-sound-start-offset))
      (when (< new-sound-len frames)
        (error 'buffer-filler
               "given sound with ~s frames < buffer-frames ~s"
               new-sound-len
               frames))
      (define frames-remaining (- frames frames-to-copy))
      (define more-frames-to-copy (min frames-remaining 
                                       new-sound-len))
      (define tgt-ptr (ptr-add ptr
                               (* frames-to-copy CHANNELS) 
                               _sint16))
      (define src-ptr (ptr-add (s16vector->cpointer (rsound-data current-sound))
                               (* new-current-sound-start-offset CHANNELS)
                               _sint16))
      (memcpy tgt-ptr 
              src-ptr 
              (* CHANNELS more-frames-to-copy)
              _sint16)))
  signal/block/unsafe)

(define (check-sound-stop-start l)
  (unless (<= 0 (second l) (third l) (rs-frames (first l)))
    (error 'check-sound-stop-start
           "expected 0 < start < finish < sound-len, given 0 < ~s < ~s < ~s"
           (second l) (third l) (rs-frames (first l))))
  l)

#|
(define well (rs-read/clip "/tmp/higher-ground.wav" 0 (* 20 44100)))

(define b (box (list well (* 1 44100) (* 3 44100))))

(match-define (list timer stopper)
  (signal/block-play/unsafe (loop->signal/block/unsafe b) 44100))

#;(define (updatest))
 

|#