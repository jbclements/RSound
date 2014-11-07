#lang racket/base

(require "rsound.rkt"
         "common.rkt"
         "sequencer.rkt")

;; PSTREAMS

;; a stream-based sound player built on top of the sequencer.
;; this provides a reasonable way to stitch together sounds 
;; seamlessly for playback, so you're not relying on the precision
;; of "sleep" to get your sequential sounds lined up correctly.

(provide make-pstream
         pstream-queue
         pstream-play
         pstream-current-frame
         pstream-queue-callback
         pstream-volume
         pstream-set-volume!)

;; a pstream bundles a sound-heap that's attached
;; to a playing stream and a time-checker that returns
;; the most recently requested frame.
(struct pstream (sound-heap callback-heap time-checker frame-rate volume-box))

;; make (and start) a pstream
(define (make-pstream #:buffer-time [buffer-time #f])
  (define frame-rate (default-sample-rate))
  (define sound-heap (make-unplayed-heap))
  (define callback-heap (make-uncallbacked-heap))
  ;; the signal for playing the heap's sounds, and
  ;; the time-checker
  (define-values (signal/block/unsafe current-time/s volume-box)
    (heap->signal/block/unsafe sound-heap callback-heap))
  (signal/block-play/unsafe signal/block/unsafe frame-rate #:buffer-time buffer-time)
  (pstream sound-heap callback-heap current-time/s frame-rate volume-box))

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
  (unless (= (pstream-frame-rate pstream) (rsound-sample-rate snd))
    (raise-argument-error 
     'pstream-queue 
     (format "rsound matching pstream's frame rate (~a)"
             (pstream-frame-rate pstream))
     1 pstream snd frame))
  (unless (nonnegative-integer? frame)
    (raise-argument-error 'pstream-queue "exact nonnegative integer" 2 pstream snd frame))
  (define exact-frame (inexact->exact frame))
  (queue-for-playing! (pstream-sound-heap pstream) 
                      snd 
                      exact-frame)
  "sound is queued")

;; queue a callback to run at a particular frame
(define (pstream-queue-callback pstream callback frame)
  (unless (pstream? pstream)
    (raise-argument-error 'pstream-queue-callback "pstream" 0 pstream callback frame))
  (unless (and (procedure? callback)
               (procedure-arity-includes? callback 0))
    (raise-argument-error 'pstream-queue-callback
                          "procedure that accepts no arguments"
                          1 pstream callback frame))
  (unless (exact-nonnegative-integer? frame)
    (raise-argument-error 'pstream-queue-callback
                          "exact nonnegative integer" 2 pstream callback frame))
  (queue-for-callbacking! (pstream-callback-heap pstream) 
                          callback
                          frame)
  "callback is queued")

;; queue 'snd' for playing at the current frame
(define (pstream-play pstream snd)
  (unless (pstream? pstream)
    (raise-argument-error 'pstream-play "pstream" 0 pstream snd))
  (unless (rsound? snd)
    (raise-argument-error 'pstream-play "rsound" 1 pstream snd))
  (pstream-queue pstream snd (pstream-current-frame pstream))
  "sound is queued")

;; check the pstream's volume
(define (pstream-volume pstream)
  (unless (pstream? pstream)
    (raise-argument-error 'pstream-volume "pstream" 0 pstream))
  (unbox (pstream-volume-box pstream)))

;; set the pstream's volume
(define (pstream-set-volume! pstream volume)
  (unless (pstream? pstream)
    (raise-argument-error 'pstream-set-volume! "pstream" 0 pstream volume))
  (unless (number? volume)
    (raise-argument-error 'pstream-set-volume! "number" 1 pstream volume))
  (set-box! (pstream-volume-box pstream) (exact->inexact volume))
  pstream)


