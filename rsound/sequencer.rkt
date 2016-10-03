#lang racket/base

(require "rsound.rkt"
         "common.rkt"
         "util.rkt"
         "paste-util.rkt"
         data/heap
         ffi/vector
         racket/contract
         racket/match)

;; a simple rsound sequencer.

;; this is (now) the basis for the 'pstream' abstraction

;; FIXME check for same sample rate!

(provide/contract
 [make-unplayed-heap (-> heap?)]
 [make-uncallbacked-heap (-> heap?)]
 [queue-for-playing! (-> heap? rsound? nonnegative-integer? void?)]
 [queue-for-callbacking! (-> heap? (-> any) nonnegative-integer? void?)]
 [heap->signal/block/unsafe
  (-> heap? heap? (values procedure? procedure? box?))]
 [clear-all-sounds! (-> heap? void?)])


;; with occasional #f's sneaking into the heap, it looks like I need to
;; protect my heaps against race conditions. I think it should be performant
;; to have one semaphore per invocation, rather than one per heap.
(define heap-sema (make-semaphore 1))


;; an entry is a vector of an rsound, a start frame, and an end frame.
(define-struct entry (sound start finish) #:transparent)

;; a callback contains a semaphore and a time-to-trigger
(struct callback (sema trigger-t))

;; a heap of entries, ordered by start time.
(define (make-unplayed-heap)
  (make-heap (lambda (a b)
               (<= (entry-start a) (entry-start b)))))

;; given a heap and a sound and a start, add the sound to the
;; heap with the given start and a computed end
(define (queue-for-playing! heap sound start)
  (when (heap-has-false? heap)
    (error 'queue-for-playing "heap has false in it: ~s"
           (heap->vector heap)))
  (call-with-semaphore 
   heap-sema
   (lambda ()
     (heap-add! heap (make-entry sound start (+ start (rs-frames sound)))))))

;; a heap of callbacks, ordered by trigger time
(define (make-uncallbacked-heap)
  (make-heap (lambda (a b)
               (<= (callback-trigger-t a) 
                   (callback-trigger-t b)))))

;; given a callback-heap and a procedure of no args and a frame,
;; add a callback to the heap that will trigger the procedure
;; at (or soon after) the given frame
(define (queue-for-callbacking! heap cb frame)
  (define sema (make-semaphore))
  (call-with-semaphore
   heap-sema
   (lambda ()
     (heap-add! heap (callback sema frame))))
  (thread
   (lambda ()
     (semaphore-wait sema)
     (cb)))
  (void))

(define (heap-has-false? heap)
  (not (for/and ([elt (heap->vector heap)]) elt)))

;; this accepts a heap of input sound entries and produces a 
;; signal/block that plays them, a thunk that can be used to determine the
;; most recent value of t, and a box containing the volume of the 
;; sound (1.0 represents no change in volume)

(define (heap->signal/block/unsafe unplayed uncallbacked)
  ;; the "playing" heap is ordered by end time, to facilitate removal
  (define playing (make-heap (lambda (a b) (<= (entry-finish a) (entry-finish b)))))
  ;; invariant: playing-vec contains the same elements as the "playing" heap.
  (define playing-vec (vector))
  ;; this mutable variable contains the last requested frame.
  (define last-t 0)
  (define (get-last-t) last-t)
  (define volume-box (box 1.0))
  (define (signal/block/unsafe cpointer frames)
    (when (< frames 0)
      (error 'sequencer "callback called with frames < 0: ~e\n" frames))
    (define next-last-t (+ frames last-t))
    ;; remove sounds that end before the start:
    (define sounds-removed? (clear-ended-sounds! playing last-t))
    ;; add sounds that start before the end:
    (define sounds-added? (add-new-sounds unplayed playing last-t next-last-t))
    (when (or sounds-removed? sounds-added?)
      (set! playing-vec (heap->vector playing)))
    (combine-onto! cpointer last-t frames playing-vec (unbox volume-box))
    (trigger-ready-semaphores! uncallbacked next-last-t)
    (set! last-t next-last-t))
  (values 
   signal/block/unsafe
   get-last-t
   volume-box))

;; zero the target, and copy the appropriate regions of the 
;; source sounds onto them.
(define (combine-onto! cpointer t len playing-vec volume)
  (zero-buffer! cpointer len)
  (for ([e (in-vector playing-vec)])
    (add-from-buf! cpointer t len e volume)))

;; given a buffer in which to assemble the sounds, a frame number t,
;; a number of frames len, a playing entry e, and a volume, add the appropriate
;; section of the entry to the buffer.
;; required: entries have finish later than start, len > 0.
(define (add-from-buf! ptr t len e volume)
  (match-define (entry sound start finish) e)
  ;; in global time:
  (define copy-start (max t start))
  (define copy-finish (min (+ t len) finish))
  ;; must have finish later than start:
  (define copy-len (- copy-finish copy-start))
  ;; relative to source buffer:
  (define src-start (- copy-start start))
  ;; relative to target buffer:
  (define tgt-start (- copy-start t))
  (rs-copy-mult-add! ptr   tgt-start
                     sound src-start
                     copy-len len
                     volume))

;; given a heap (ordered by ending time) and a current time, remove
;; those sounds whose ending times are <= the current time
(define (clear-ended-sounds! playing-heap current-time)
  (let loop ([removed? #f])
    (cond [(= (heap-count playing-heap) 0) removed?]
          [else
           (define earliest-ending (heap-min playing-heap))
           (cond [(<= (entry-finish earliest-ending) current-time) 
                  (call-with-semaphore
                   heap-sema
                   (lambda ()
                     (heap-remove-min! playing-heap)))
                  (loop #t)]
                 [else removed?])])))

;; given a heap (ordered by ending time), remove
;; all sounds
(define (clear-all-sounds! playing-heap)
  (let loop ([removed? #f])
    (cond [(= (heap-count playing-heap) 0) (void)]
          [else
                  (call-with-semaphore
                   heap-sema
                   (lambda ()
                     (heap-remove-min! playing-heap)))
                  (loop #t)
                  (void)
                  ] )))

;; given a callback heap and a current time, post to all the
;; semaphores whose trigger time precedes the current time
(define (trigger-ready-semaphores! callback-heap t)
  (let loop ()
    (cond [(= (heap-count callback-heap) 0) (void)]
          [else
           (define f (heap-min callback-heap))
           (cond [(<= (callback-trigger-t f) t)
                  (call-with-semaphore
                   heap-sema
                   (lambda ()
                     (heap-remove-min! callback-heap)))
                  (semaphore-post (callback-sema f))
                  (loop)]
                 [else
                  (void)])])))

;; given a heap of queued sounds (ordered by starting time), 
;; a heap of playing sounds (ordered by ending time), and
;; a time, add the sounds that begin before or on the given ending
;; time, unless they end before the given starting time.
;; return a boolean indicating whether any sounds were added.
(define (add-new-sounds queued-heap playing-heap start-time stop-time)
  (let loop ([added? #f])
  (cond [(= (heap-count queued-heap) 0) added?]
        [else
         (define earliest-to-play (heap-min queued-heap))
         #;(log-debug (format "earliest: ~s\n" earliest-to-play))
         (cond [(<= (entry-finish earliest-to-play) start-time)
                (log-warning 
                 (format "missed a queued sound entirely, because ~e<=~e"
                         (entry-finish earliest-to-play) 
                         start-time))
                (call-with-semaphore
                 heap-sema
                 (lambda ()
                   (heap-remove-min! queued-heap)))
                (loop added?)]
               [(<= (entry-start earliest-to-play) stop-time)
                (call-with-semaphore
                 heap-sema
                 (lambda ()
                   (heap-add! playing-heap earliest-to-play)
                   (heap-remove-min! queued-heap)))                
                (loop #t)]
               [else added?])])))


(module+ the-test-suite

  (require rackunit)
  (provide the-test-suite)

  (define the-test-suite
  (test-suite
 "sequencer"
;; test of queue
(let ()
  (define h (make-unplayed-heap))
  (queue-for-playing! h (clip ding 0 10000) 20000)
  (queue-for-playing! h ding 18000)
  (check-equal? (heap-count h) 2)
  (check-equal? (heap-min h) (entry ding 18000 (+ 18000 (rs-frames ding))))
  (heap-remove-min! h)
  (check rs-equal?
         (entry-sound (heap-min h))
         (clip ding 0 10000))
  (check-equal? (entry-start (heap-min h)) 20000)
  (check-equal? (entry-finish (heap-min h)) 30000))

(let ()
  (define h (make-uncallbacked-heap))
  (define test-box (box #f))
  (queue-for-callbacking! h (lambda () (set-box! test-box #t)) 21)
  (trigger-ready-semaphores! h 15)
  (check-equal? (unbox test-box) #f)
  (trigger-ready-semaphores! h 30)
  ;; not sure I should actually test this...:
  (check-equal? (unbox test-box) #f)
  ;; sleep to allow semaphores to run:
  (sleep 0.001)
  (check-equal? (unbox test-box) #t))

(let ()
  (define playing-heap (make-heap (lambda (a b) 
                                    (<= (entry-finish a) (entry-finish b)))))
  (heap-add-all! playing-heap (list (entry 'a 14 29)
                                    (entry 'b 16 21)
                                    (entry 'c 5 25)
                                    (entry 'd 3 25)))
  (check-equal? (heap-count playing-heap) 4)
  (check-equal? (heap-min playing-heap) (entry 'b 16 21))
  (check-equal? (clear-ended-sounds! playing-heap 22) #t)
  (check-equal? (heap-count playing-heap) 3)
  (check-equal? (clear-ended-sounds! playing-heap 25) #t)
  (check-equal? (heap-count playing-heap) 1)
  (check-equal? (clear-ended-sounds! playing-heap 30) #t)
  (check-equal? (heap-count playing-heap) 0)
  (check-equal? (clear-ended-sounds! playing-heap 40) #f))

(let ()
  (define unplayed-heap (make-unplayed-heap))
  (heap-add-all! unplayed-heap (list (entry 'a 14 29)
                                    (entry 'b 16 21)
                                    (entry 'c 5 25)
                                    (entry 'd 3 25)))
  (check-equal? (heap-count unplayed-heap) 4)
  (clear-all-sounds! unplayed-heap)
  (check-equal? (heap-count unplayed-heap) 0)
  (clear-all-sounds! unplayed-heap))




(let ()
  (define queued-heap (make-unplayed-heap))
  (heap-add-all! queued-heap (list (entry 'a 14 29)
                                    (entry 'b 16 21)
                                    (entry 'e 26 27)
                                    (entry 'c 5 25)
                                    (entry 'd 3 25)))
  (define playing-heap (make-heap 
                        (lambda (a b)
                          (<= (entry-finish a) (entry-finish b)))))
  (check-equal? (heap-count queued-heap) 5)
  (check-equal? (heap-min queued-heap) (entry 'd 3 25))
  (check-equal? (add-new-sounds queued-heap playing-heap 0 2) #f)
  (check-equal? (heap-count queued-heap) 5)
  (check-equal? (heap-count playing-heap) 0)
  (check-equal? (add-new-sounds queued-heap playing-heap 2 4) #t)
  (check-equal? (heap-count queued-heap) 4)
  (check-equal? (heap-count playing-heap) 1)
  (check-equal? (add-new-sounds queued-heap playing-heap 4 13) #t)
  (check-equal? (heap-count queued-heap) 3)
  (check-equal? (heap-count playing-heap) 2)
  (check-equal? (add-new-sounds queued-heap playing-heap 13 19) #t)
  (check-equal? (heap-count queued-heap) 1)
  (check-equal? (heap-count playing-heap) 4)
  (check-equal? (add-new-sounds queued-heap playing-heap 19 25) #f)
  ;; oops, missed e completely:
  (check-equal? (add-new-sounds queued-heap playing-heap 30 35) #f)
  (check-equal? (heap-count queued-heap) 0)
  (check-equal? (heap-count playing-heap) 4))





(let ()
  (define src1 (rsound (make-s16vector (* CHANNELS 200) 1) 0 200 44123))
  (define entry1 (entry src1 50 250))
  (define entry2 (entry src1 65 265))
  (define dst1 (make-s16vector (* CHANNELS 10) 0))
  (check-equal? (s16vector->list dst1) (list 0 0 0 0 0 0 0 0 0 0
                                             0 0 0 0 0 0 0 0 0 0))
  (add-from-buf! (s16vector->cpointer dst1) 45 10 entry1 1.0)
  (check-equal? (s16vector->list dst1) (list 0 0 0 0 0 0 0 0 0 0
                                             1 1 1 1 1 1 1 1 1 1))
  
  (define dst2 (make-s16vector 20 0))
  (add-from-buf! (s16vector->cpointer dst2) 60 10 entry1 1.0)
  (add-from-buf! (s16vector->cpointer dst2) 60 10 entry2 1.0)
  (check-equal? (s16vector->list dst2) (list 1 1 1 1 1 1 1 1 1 1 
                                             2 2 2 2 2 2 2 2 2 2))
  
  (define dst3 (make-s16vector 20 0))
  (define src3 (rsound (make-s16vector 10 2) 1 5 44123))
  (define entry3 (entry src3 70 74))
  (add-from-buf! (s16vector->cpointer dst3) 68 10 entry1 1.0)
  (add-from-buf! (s16vector->cpointer dst3) 68 10 entry3 1.0)
  (check-equal? (s16vector->list dst3) (list 1 1 1 1 
                                             3 3 3 3 3 3 3 3
                                             1 1 1 1 1 1 1 1))

  
  )

(let ()
  (define s1 (rsound (make-s16vector 20 2442) 0 10 44123))
  (define unplayed-heap (make-unplayed-heap))
  (define uncallbacked-heap (make-uncallbacked-heap))
  (queue-for-playing! unplayed-heap s1 5)
  (define-values (test-signal/block last-time volume-box) 
    (heap->signal/block/unsafe unplayed-heap uncallbacked-heap))
  (set-box! volume-box 0.67)
  (define tgt (make-s16vector 200 123))
  (define tgt-ptr (s16vector->cpointer tgt))
  (test-signal/block tgt-ptr 100)
  (check-equal? (s16vector-ref tgt 8) 0)
  (check-equal? (s16vector-ref tgt 9) 0)
  (check-equal? (s16vector-ref tgt 10) 1636)
  (check-equal? (s16vector-ref tgt 28) 1636)
  (check-equal? (s16vector-ref tgt 30) 0)
  )
(let ()
  (define s1 (rsound (make-s16vector 20 2) 0 10 44123))
  (define s2 (rsound (make-s16vector 4 3) 0 2 44123))
  (define s3 (rsound (make-s16vector 30 4) 0 15 44123))
  (define unplayed-heap (make-unplayed-heap))
  (define uncallbacked-heap (make-uncallbacked-heap))
  (queue-for-playing! unplayed-heap s1 15)
  (queue-for-playing! unplayed-heap s1 17)
  (queue-for-playing! unplayed-heap s3 37)
  (queue-for-playing! unplayed-heap s2 41)
  (define test-box (box #f))
  (queue-for-callbacking! uncallbacked-heap (lambda () (set-box! test-box #t)) 21)
  (define tgt (make-s16vector 20 123))
  (define tgt-ptr (s16vector->cpointer tgt))
  (define-values (test-signal/block last-time volume-box) 
    (heap->signal/block/unsafe unplayed-heap uncallbacked-heap))
  (test-signal/block tgt-ptr 10)
  (check-equal? (s16vector->list tgt)
                (list 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0))
  (check-equal? (last-time) 10)
  (check-equal? (unbox test-box) #f)
  (test-signal/block tgt-ptr 10)
  (check-equal? (s16vector->list tgt)
                (list 0 0 0 0 0 0 0 0 0 0
                      2 2 2 2 4 4 4 4 4 4))
  (check-equal? (unbox test-box) #f)
  (check-equal? (last-time) 20)
  (test-signal/block tgt-ptr 10)
  (check-equal? (s16vector->list tgt)
                (list 4 4 4 4 4 4 4 4 4 4
                      2 2 2 2 0 0 0 0 0 0))
  (check-equal? (last-time) 30)
  ;; must sleep a bit to allow semaphore-triggered thunk to run:
  (sleep 0.001)
  (check-equal? (unbox test-box) #t)
  (test-signal/block tgt-ptr 10)
  (check-equal? (s16vector->list tgt)
                (list 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 4 4 4 4 4 4))
  (test-signal/block tgt-ptr 10)
  (check-equal? (s16vector->list tgt)
                (list 4 4 7 7 7 7 4 4 4 4
                      4 4 4 4 4 4 4 4 4 4))
  (check-equal? (last-time) 50))))

)

(module+ test
  (require rackunit/text-ui)
  (require (submod ".." the-test-suite))
  (run-tests the-test-suite))
