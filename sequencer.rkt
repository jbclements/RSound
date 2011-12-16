#lang racket

(require "rsound.rkt"
         "util.rkt"
         "paste-util.rkt"
         data/heap
         ffi/vector
         rackunit)

;; a simple rsound sequencer

(provide (all-defined-out))



;; an entry is a vector of an rsound, a start frame, and an end frame.
(struct entry (sound start finish) #:transparent)

;; a heap of entries, ordered by start time.
(define (make-unplayed-heap)
  (make-heap (lambda (a b)
               (<= (entry-start a) (entry-start b)))))

;; given a heap and a sound and a start, add the sound to the
;; heap with the given start and a computed end
(define (queue-for-playing! heap sound start)
  (heap-add! heap (entry sound start (+ start (rs-frames sound)))))


;; this accepts a heap of input sound entries and produces a "sensitive"
;; signal/block that plays them, and a thunk that can be used to determine the
;; most recent value of t.  It's sensitive in the sense that if you don't 
;; call it with monotonically non-decreasing time values, it's going to 
;; behave badly. It returns a thunk that knows the most recent value of t

(define (heap->signal/block/unsafe unplayed)
  ;; the "playing" heap is ordered by end time, to facilitate removal
  (define playing (make-heap (lambda (a b) (<= (entry-finish a) (entry-finish b)))))
  ;; invariant: playing-vec contains the same elements as the "playing" heap.
  (define playing-vec (vector))
  (define last-t 0)
  (define (get-last-t) last-t)
  (define (signal/block cpointer frames t)
    (define new-last-t (+ frames t))
    (when (< new-last-t last-t)
      (error 'sequencer "new value of last-t ~s is less than old value ~s."
             new-last-t
             last-t))
    (set! last-t new-last-t)
    ;; remove sounds that end before the start
    (define sounds-removed? (clear-ended-sounds playing t))
    ;; add sounds that start before the end
    (define sounds-added? (add-new-sounds unplayed playing (+ t frames)))
    (when (or sounds-removed? sounds-added?)
      (set! playing-vec (heap->vector playing)))
    (combine-onto! cpointer t frames playing-vec))
  (values 
   signal/block
   get-last-t))

;; zero the target, and copy the appropriate regions of the 
;; source sounds onto them.
(define (combine-onto! cpointer t len playing-vec)
  (zero-buffer! cpointer len)
  (for ([e (in-vector playing-vec)])
    (add-from-buf! cpointer t len e)))

;; given a buffer in which to assemble the sounds, a frame number t,
;; a number of frames len, and a playing entry e, add the appropriate
;; section of the entry to the buffer.
;; required: entries have finish later than start, len > 0.
(define (add-from-buf! ptr t len e)
  (match-define (entry sound start finish) e)
  ;; in global time:
  (define copy-start (max t start))
  (define copy-finish (min (+ t len) finish))
  (define copy-len (- copy-finish copy-start))
  ;; relative to source buffer:
  (define src-start (- copy-start start))
  ;; relative to target buffer:
  (define tgt-start (- copy-start t))
  (rs-copy-add! ptr   tgt-start
                sound src-start
                copy-len len))

;; given a heap (ordered by ending time) and a current time, remove
;; those sounds whose ending times are <= the current time
(define (clear-ended-sounds playing-heap current-time)
  (let loop ([removed? #f])
    (cond [(= (heap-count playing-heap) 0) removed?]
          [else
           (define earliest-ending (heap-min playing-heap))
           (cond [(<= (entry-finish earliest-ending) current-time) 
                  (heap-remove-min! playing-heap)
                  (loop #t)]
                 [else removed?])])))


;; given a heap of queued sounds (ordered by starting time), 
;; a heap of playing sounds (ordered by ending time), and
;; a time, add the sounds that begin before the given time.
(define (add-new-sounds queued-heap playing-heap current-time)
  (let loop ([added? #f])
  (cond [(= (heap-count queued-heap) 0) added?]
        [else
         (define earliest-to-play (heap-min queued-heap))
         (cond [(<= (entry-start earliest-to-play) current-time)
                (heap-add! playing-heap earliest-to-play)
                (heap-remove-min! queued-heap)
                (loop #t)]
               [else added?])])))



;; these tests refer to a number of internals, and it's hard
;; to peel them out.

(define-test-suite
  sequencer-internals


;; test of queue
(let ()
  (define h (make-unplayed-heap))
  (queue-for-playing! h (clip ding 0 10000) 20000)
  (queue-for-playing! h ding 18000)
  (check-equal? (heap-count h) 2)
  (check-equal? (heap-min h) (entry ding 18000 (+ 18000 44100)))
  (heap-remove-min! h)
  (check rsound-equal?
         (entry-sound (heap-min h))
         (clip ding 0 10000))
  (check-equal? (entry-start (heap-min h)) 20000)
  (check-equal? (entry-finish (heap-min h)) 30000))

(let ()
  (define playing-heap (make-heap (lambda (a b) 
                                    (<= (entry-finish a) (entry-finish b)))))
  (heap-add-all! playing-heap (list (entry 'a 14 29)
                                    (entry 'b 16 21)
                                    (entry 'c 5 25)
                                    (entry 'd 3 25)))
  (check-equal? (heap-count playing-heap) 4)
  (check-equal? (heap-min playing-heap) (entry 'b 16 21))
  (check-equal? (clear-ended-sounds playing-heap 22) #t)
  (check-equal? (heap-count playing-heap) 3)
  (check-equal? (clear-ended-sounds playing-heap 25) #t)
  (check-equal? (heap-count playing-heap) 1)
  (check-equal? (clear-ended-sounds playing-heap 30) #t)
  (check-equal? (heap-count playing-heap) 0)
  (check-equal? (clear-ended-sounds playing-heap 40) #f))




(let ()
  (define queued-heap (make-unplayed-heap))
  (heap-add-all! queued-heap (list (entry 'a 14 29)
                                    (entry 'b 16 21)
                                    (entry 'c 5 25)
                                    (entry 'd 3 25)))
  (define playing-heap (make-heap 
                        (lambda (a b)
                          (<= (entry-finish a) (entry-finish b)))))
  (check-equal? (heap-count queued-heap) 4)
  (check-equal? (heap-min queued-heap) (entry 'd 3 25))
  (check-equal? (add-new-sounds queued-heap playing-heap 0) #f)
  (check-equal? (heap-count queued-heap) 4)
  (check-equal? (heap-count playing-heap) 0)
  (check-equal? (add-new-sounds queued-heap playing-heap 3) #t)
  (check-equal? (heap-count queued-heap) 3)
  (check-equal? (heap-count playing-heap) 1)
    (check-equal? (add-new-sounds queued-heap playing-heap 5) #t)
  (check-equal? (heap-count queued-heap) 2)
  (check-equal? (heap-count playing-heap) 2)
    (check-equal? (add-new-sounds queued-heap playing-heap 16) #t)
  (check-equal? (heap-count queued-heap) 0)
  (check-equal? (heap-count playing-heap) 4)
  (check-equal? (add-new-sounds queued-heap playing-heap 20) #f))





(let ()
  (define src1 (rsound (make-s16vector (* channels 200) 1) 0 200 44100))
  (define entry1 (entry src1 50 250))
  (define entry2 (entry src1 65 265))
  (define dst1 (make-s16vector (* channels 10) 0))
  (check-equal? (s16vector->list dst1) (list 0 0 0 0 0 0 0 0 0 0
                                             0 0 0 0 0 0 0 0 0 0))
  (add-from-buf! (s16vector->cpointer dst1) 45 10 entry1)
  (check-equal? (s16vector->list dst1) (list 0 0 0 0 0 0 0 0 0 0
                                             1 1 1 1 1 1 1 1 1 1))
  
  (define dst2 (make-s16vector 20 0))
  (add-from-buf! (s16vector->cpointer dst2) 60 10 entry1)
  (add-from-buf! (s16vector->cpointer dst2) 60 10 entry2)
  (check-equal? (s16vector->list dst2) (list 1 1 1 1 1 1 1 1 1 1 
                                             2 2 2 2 2 2 2 2 2 2))
  
  (define dst3 (make-s16vector 20 0))
  (define src3 (rsound (make-s16vector 10 2) 1 5 44100))
  (define entry3 (entry src3 70 74))
  (add-from-buf! (s16vector->cpointer dst3) 68 10 entry1)
  (add-from-buf! (s16vector->cpointer dst3) 68 10 entry3)
  (check-equal? (s16vector->list dst3) (list 1 1 1 1 
                                             3 3 3 3 3 3 3 3
                                             1 1 1 1 1 1 1 1))

  
  )

(let ()
  (define s1 (rsound (make-s16vector 20 2) 0 10 44100))
  (define s2 (rsound (make-s16vector 4 3) 0 2 44100))
  (define s3 (rsound (make-s16vector 30 4) 0 15 44100))
  (define unplayed-heap (make-unplayed-heap))
  (queue-for-playing! unplayed-heap s1 15)
  (queue-for-playing! unplayed-heap s1 17)
  (queue-for-playing! unplayed-heap s3 37)
  (queue-for-playing! unplayed-heap s2 41)
  (define tgt (make-s16vector 20 123))
  (define tgt-ptr (s16vector->cpointer tgt))
  (define-values (test-signal/block last-time) (heap->signal/block/unsafe unplayed-heap))
  (test-signal/block tgt-ptr 10 0)
  (check-equal? (s16vector->list tgt)
                (list 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0))
  (check-equal? (last-time) 10)
  (test-signal/block tgt-ptr 10 10)
  (check-equal? (s16vector->list tgt)
                (list 0 0 0 0 0 0 0 0 0 0
                      2 2 2 2 4 4 4 4 4 4))
  (test-signal/block tgt-ptr 10 20)
  (check-equal? (s16vector->list tgt)
                (list 4 4 4 4 4 4 4 4 4 4
                      2 2 2 2 0 0 0 0 0 0))
  (test-signal/block tgt-ptr 10 30)
  (check-equal? (s16vector->list tgt)
                (list 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 4 4 4 4 4 4))
  (test-signal/block tgt-ptr 10 40)
  (check-equal? (s16vector->list tgt)
                (list 4 4 7 7 7 7 4 4 4 4
                      4 4 4 4 4 4 4 4 4 4))
  (check-equal? (last-time) 50)))