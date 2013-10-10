#lang racket/base

(require "./rsound.rkt"
         "./sequencer.rkt")

;; a stream-based sound player built on top of the sequencer.
;; this solves problems on platforms such as windows that don't
;; like it when you open lots of streams.

(provide make-pstream
         pstream-queue
         pstream-play
         pstream-current-frame)

;; a pstream bundles a sound-heap that's attached
;; to a playing stream and a time-checker that returns
;; the most recently requested frame.
(struct pstream (sound-heap time-checker))

;; make (and start) a pstream
(define (make-pstream)
  (define pstream-heap (make-unplayed-heap))
  ;; the signal for playing the heap's sounds, and
  ;; the time-checker
  (define-values (signal/block/unsafe current-time/s)
    (heap->signal/block/unsafe pstream-heap))
  (signal/block-play/unsafe signal/block/unsafe (default-sample-rate))
  (pstream pstream-heap current-time/s))

;; return the last-requested frame from the pstream
(define (pstream-current-frame pstream)
  (unless (pstream? pstream)
    (raise-argument-error 'pstream-current-frame "pstream" 0 pstream))
  ((pstream-time-checker pstream)))

;; queue 'snd' for playing on 'pstream' at 'frame'
(define (pstream-queue pstream snd frame)
  (unless (pstream? pstream)
    (raise-argument-error 'pstream-queue "pstream" 0 pstream snd frame))
  (unless (rsound? snd)
    (raise-argument-error 'pstream-queue "rsound" 1 pstream snd frame))
  (unless (exact-nonnegative-integer? frame)
    (raise-argument-error 'pstream-queue "exact nonnegative integer" 2 pstream snd frame))
  (queue-for-playing! (pstream-sound-heap pstream) 
                      snd 
                      frame)
  pstream)

;; queue 'snd' for playing at the current frame
(define (pstream-play pstream snd)
  (unless (pstream? pstream)
    (raise-argument-error 'pstream-play "pstream" 0 pstream snd))
  (unless (rsound? snd)
    (raise-argument-error 'pstream-play "rsound" 1 pstream snd))
  (pstream-queue pstream snd (pstream-current-frame pstream)))


