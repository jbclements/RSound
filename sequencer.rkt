#lang racket

(require "rsound.rkt"
         "util.rkt"
         data/heap
         rackunit)

;; a simple rsound sequencer

;; 
(define (rsound-play-at-intervals rsound interval)
  (define rsound-len (rsound-frames rsound))
  (define the-signal
    (lambda (t)
      (define t-rel (modulo t interval))
      (cond [(< t-rel (rsound-frames rsound)) (rsound-ith/left rsound t-rel)]
            [else 0])))
  (signal-play the-signal (rsound-sample-rate rsound)))

#;(signal-play (sine-wave 440 44100) 4100)

#;(rsound-play-at-intervals ding 4100)

;; a sound-entry is a list of an rsound, a start time, and an end time.

;; the check-interval is the largest # of frames that the player will go without checking for new
;; sounds added to the queue.  If the queue cannot be mutated, this can be arbitrarily
;; large.
(define check-interval 1000)

(define (rsound-heap-play unplayed sample-rate)
  ;; the "playing" heap is ordered by end time, to facilitate removal
  (define playing (make-heap (lambda (a b) (<= (third a) (third b)))))
  ;; invariant: playing-vec contains the same elements as the "playing" heap.
  (define playing-vec (vector))
  (define next-check-time 0)
  (signal-play
   (lambda (t)
     (when (<= next-check-time t)
       (define-values (sounds-added? next-starting-time) (add-new-sounds unplayed playing t))
       (define-values (sounds-removed? next-ending-time) (clear-ended-sounds playing t))
       (when (or sounds-removed? sounds-added?)
         (set! playing-vec (heap->vector playing)))
       (set! next-check-time (min next-starting-time next-ending-time (+ t check-interval)))
       )
     (sum-of-currently-playing playing-vec))
   sample-rate))

;; given a heap (ordered by ending time) and a current time, remove
;; those sounds whose ending times are <= the current time
(define (clear-ended-sounds playing-heap current-time)
  (let loop ()
    (cond [(= (heap-count playing-heap) 0) 'done]
          [else
           (define earliest-ending (heap-min playing-heap))
           (cond [(<= (third earliest-ending) current-time) 
                  (heap-remove-min! playing-heap)
                       (loop)]
                 [else 'done])])))

(let ()
  (define playing-heap (make-heap (lambda (a b) (<= (third a) (third b)))))
  (heap-add-all! playing-heap (list (list 'a 14 29)
                                    (list 'b 16 21)
                                    (list 'c 5 25)
                                    (list 'd 3 25)))
  (check-equal? (heap-count playing-heap) 4)
  (check-equal? (heap-min playing-heap) (list 'b 16 21))
  (check-equal? (clear-ended-sounds playing-heap 22) 'done)
  (check-equal? (heap-count playing-heap) 3)
  (check-equal? (clear-ended-sounds playing-heap 25) 'done)
  (check-equal? (heap-count playing-heap) 1)
  (check-equal? (clear-ended-sounds playing-heap 30) 'done)
  (check-equal? (heap-count playing-heap) 0))

(define (add-new-sounds queued-heap playing-heap current-time)
  (let loop ()
  (cond [(= (heap-count queued-heap) 0) 'done]
        [else
         (define earliest-to-play (heap-min queued-heap))
         (cond [(<= (second earliest-to-play) current-time)
                (heap-add! playing-heap earliest-to-play)
                (heap-remove-min! queued-heap)
                (loop)]
               [else 'done])])))

(let ()
  (define queued-heap (make-heap (lambda (a b) (<= (second a) (second b)))))
  (heap-add-all! queued-heap (list (list 'a 14 29)
                                    (list 'b 16 21)
                                    (list 'c 5 25)
                                    (list 'd 3 25)))
  (define playing-heap (make-heap (lambda (a b) (<= (third a) (third b)))))
  (check-equal? (heap-count queued-heap) 4)
  (check-equal? (heap-min queued-heap) (list 'd 3 25))
  (check-equal? (add-new-sounds queued-heap playing-heap 0) 'done)
  (check-equal? (heap-count queued-heap) 4)
  (check-equal? (heap-count playing-heap) 0)
  (check-equal? (add-new-sounds queued-heap playing-heap 3) 'done)
  (check-equal? (heap-count queued-heap) 3)
  (check-equal? (heap-count playing-heap) 1)
    (check-equal? (add-new-sounds queued-heap playing-heap 5) 'done)
  (check-equal? (heap-count queued-heap) 2)
  (check-equal? (heap-count playing-heap) 2)
    (check-equal? (add-new-sounds queued-heap playing-heap 16) 'done)
  (check-equal? (heap-count queued-heap) 0)
  (check-equal? (heap-count playing-heap) 4))


(define (sum-of-currently-playing playing-queue current-time)
  )

