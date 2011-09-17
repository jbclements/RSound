#lang racket

;; to-do:
;; contracts everywhere?
;; change "rsound-" to "rs-"
;; check okay for inputs to be inexact integers?
;; go to time instead of frames?
;; tighter sanity checks on sample rate?

(require (only-in ffi/unsafe memcpy _sint16 memset ptr-add cpointer?)
         ffi/vector
         racket/unsafe/ops
         "read-wav.rkt"
         "write-wav.rkt"
         (prefix-in rc: "rsound-commander.rkt")
         "private/s16vector-add.rkt")

(define (positive-integer? n)
  (and (integer? n) (< 0 n)))

(define (nonnegative-integer? n)
  (and (integer? n) (<= 0 n)))

;; a rsound is (rsound rdata positive-integer)
(provide (struct-out rsound))
(struct rsound (data sample-rate) 
  #:transparent
  ;#:property prop:equal+hash
  ;(list rsound=? rsound-hash-1 rsound-hash-2)
  )


(provide/contract [rsound-frames (-> rsound? nonnegative-integer?)]
                  [channels positive-integer?])

(provide (struct-out rsound)
         rsound-play
         signal?
         signal-play
         signal/block?
         signal/block-play
         rsound-loop
         stop-playing
         #;change-loop
         ;; why is this necessary?
         #;rsound-nth-sample
         rsound-ith/left/s16
         rsound-ith/right/s16
         rsound-ith/left
         rsound-ith/right
         set-rsound-ith/left!
         set-rsound-ith/right!
         #;rsound-scale
         rsound-equal?
         rsound-clip
         rsound-append
         rsound-append*
         rsound-overlay*
         default-sample-rate
         mono-signal->rsound
         signals->rsound
         signal->rsound/filtered
         make-silence
         rsound-read
         rsound-read/clip
         rsound-read-frames
         rsound-read-sample-rate
         rsound-write
         rsound-largest-frame/range/left
         rsound-largest-frame/range/right
         rsound-largest-sample
         ;; for testing...
         sound-list-total-frames
         same-sample-rate-check)


(define s16max #x7fff)
(define -s16max (- s16max))
(define s16max/i (exact->inexact #x7fff))
(define s16-size 2)

(define channels rc:channels)
(define stop-playing rc:stop-playing)

;; used for creating sounds; specifying the 
;; sample rate every time is too much of a pain
;; for students.
(define default-sample-rate (make-parameter 44100))

;; an rsound (racket sound) provides a representation for sounds 
;; that leaves them packed as C data. For the moment, it's 
;; 2-channel float only. Also, it discards all meta-information
;; except length and sample-rate.


(define (rsound-frames rsound)
  (/ (s16vector-length (rsound-data rsound)) 2))

;; an rdata is either
;; an s16vector, 
;; a function from time to a pair of real numbers in the range -1 to 1, or
;; a function of no arguments that produces real numbers in the range -1 to 1.

(define (rsound-equal? r1 r2)
  (and (= (rsound-frames r1)
          (rsound-frames r2))
       (= (rsound-sample-rate r1)
          (rsound-sample-rate r2))
       (s16vector-equal? (rsound-data r1) (rsound-data r2))))

(define (s16vector-equal? v1 v2)
  (and (= (s16vector-length v1)
          (s16vector-length v2))
       (for/and ([i (in-range (s16vector-length v1))])
         (= (s16vector-ref v1 i) (s16vector-ref v2 i)))))

#;(define (rsound-hash-1 x y) 3)
#;(define (rsound-hash-2 x y) 3)

;; can this procedure be used as a signal? 
(define (signal? f)
  (and (procedure? f) (procedure-arity-includes? f 1)))

;; can this procedure be used as a signal/block?
(define (signal/block? f)
  (and (procedure? f) (procedure-arity-includes? f 3)))



;; ** FILE I/O **

;; just a wrapper around read-sound/floatblock
(define (rsound-read path)
  (unless (path-string? path)
    (raise-type-error 'rsound-read "path-string" 0 path))
  (match (read-sound/s16vector path 0 #f)
    [(list data sample-rate) (rsound data sample-rate)]))

;; read a portion of a sound
(define (rsound-read/clip path start-frame end-frame)
  (unless (path-string? path)
    (raise-type-error 'rsound-read "path-string" 0 path start-frame end-frame))
  (unless (nonnegative-integer? start-frame)
    (raise-type-error 'rsound-read "non-negative integer" 1 path start-frame end-frame))
  (unless (nonnegative-integer? end-frame)
    (raise-type-error 'rsound-read "non-negative integer" 2 path start-frame end-frame))
  (match (read-sound/s16vector path (inexact->exact start-frame) (inexact->exact end-frame))
    [(list data sample-rate) (rsound data sample-rate)]))

;; what is the sample-rate of a file?
(define (rsound-read-sample-rate path)
  (unless (path-string? path)
    (raise-type-error 'rsound-read-sample-rate "path-string" 0 path))
  (second (read-sound/formatting path)))

;; how many frames are in the file?
(define (rsound-read-frames path)
  (unless (path-string? path)
    (raise-type-error 'rsound-read-frames "path-string" 0 path))
  (first (read-sound/formatting path)))

;; just a wrapper around write-sound/floatblock
(define (rsound-write sound path)
  (unless (rsound? sound)
    (raise-type-error 'rsound-write "rsound" 0 sound path))
  (unless (path-string? path)
    (raise-type-error 'rsound-write "path" 1 sound path))
  (match sound
    [(struct rsound (data sample-rate))
     ;; don't blow out anyone's eardrums. takes about 
     ;; 1 sec per minute on my 2006 machine.
     ;; IRRELEVANT IN THE S16 WORLD
     #;(check-below-threshold data frames 4.0)
     (write-sound/s16vector data sample-rate path)]))

;; ** SP3AKeR I/O **


;; play a signal using portaudio:
(define (signal-play signal sample-rate)
  (unless (and (procedure? signal)
               (procedure-arity-includes? signal 1))
    (raise-type-error 'signal-play "signal" 0 signal sample-rate))
  (unless (positive-integer? sample-rate)
    (raise-type-error 'signal-play "sample rate (nonnegative exact integer)" 1 signal sample-rate))
  (signal-play signal sample-rate))

;; play a signal/block using portaudio:
(define (signal/block-play signal/block sample-rate)
  (error 'signal/block-play "not implemented")
  #;(unless (and (procedure? signal/block)
               (procedure-arity-includes? signal/block 3))
    (raise-type-error 'signal-play "signal/block" 0 signal/block sample-rate))
  #;(unless (positive-integer? sample-rate)
    (raise-type-error 'signal-play 
                      "sample rate (nonnegative exact integer)"
                      1 signal/block sample-rate))
  #;(rsignal/block-play signal/block sample-rate))

;; play a sound using portaudio:
(define ((rsound-play/helper loop?) sound)
  (match sound
    [(struct rsound (data sample-rate))
     ;; don't destroy anyone's eardrums:
     ;; IRRELEVANT IN THE S16 WORLD
     #;(check-below-threshold data frames 4.0)
     (if loop?
         (error 'rsound-play/helper "not implemented")
         (rc:buffer-play data sample-rate))]
    [other
     (error 'rsound-play/helper "expected an rsound, got: ~e" sound)]))

;; play an rsound
(define rsound-play 
  (rsound-play/helper #f))

;; loop an rsound endlessly
(define (rsound-loop sound)
  (when (= (rsound-frames sound) 0)
    (error 'rsound-loop "It's a bad idea to loop an empty sound."))
  ((rsound-play/helper #t) sound))
 
;; backup solution: play from a file:
#;(define (rsound-play sound)
  (let ([filename (make-temporary-file "tmpsound~a.wav")])
    ;; don't blow out anyone's eardrums. takes about 
    ;; 1 sec per minute of on my 2006 machine.
    (check-below-threshold sound 2.0)
    (rsound-write sound filename)
    (thread 
     (lambda ()
       (play-sound filename #f)
       (delete-file filename)))))

;; change the loop that's currently playing. Has no effect if a 
;; loop isn't currently playing.
;; ** ASSUMES SAMPLE-RATE IS UNCHANGED **
#;(define (change-loop sound)
  (unless (rsound? sound)
    (raise-type-error 'change-loop "rsound" 0 sound))
  (match sound 
    [(struct rsound (data frames sample-rate))
     (error 'change-loop "not currently implemented")]
    [other 
     (error 'change-loop "expected an rsound, got: ~e" sound)]))

;; return the nth sample of an rsound's left channel.
(define (rsound-ith/left/s16 sound frame)
  (rsound-extractor sound frame #t (lambda (x) x)))

;; return the nth sample of an rsound's right channel
(define (rsound-ith/right/s16 sound frame)
  (rsound-extractor sound frame #f (lambda (x) x)))

(define (rsound-ith/left sound frame)
  (rsound-extractor sound frame #t s16->real))

(define (rsound-ith/right sound frame)
  (rsound-extractor sound frame #f s16->real))


;; the abstraction behind the last four functions...
(define (rsound-extractor rsound frame left? scale-fun)
  (unless (rsound? rsound)
    (raise-type-error 'rsound-extractor "rsound" 0 rsound frame))
  (unless (nonnegative-integer? frame)
    (raise-type-error 'rsound-extractor "nonnegative integer" 1 rsound frame))
  (unless (< frame (rsound-frames rsound))
    (raise-type-error 'rsound-extractor (format "frame index less than available # of frames ~s" (rsound-frames rsound)) 1 rsound frame))
  (scale-fun (s16vector-ref (rsound-data rsound) (frame->sample frame left?))))

;; set the ith frame of the left channel to be new-val
(define (set-rsound-ith/left! sound frame new-val)
  (rsound-mutator sound frame #t new-val))

;; set the ith frame of the right channel to be new-val
(define (set-rsound-ith/right! sound frame new-val)
  (rsound-mutator sound frame #f new-val))

;; a mutation abstraction:
(define (rsound-mutator rsound frame left? new-val)
  (unless (rsound? rsound)
    (raise-type-error 'rsound-mutator "rsound" 0 rsound frame new-val))
  (unless (nonnegative-integer? frame)
    (raise-type-error 'rsound-mutator "nonnegative integer" 1 rsound frame new-val))
  (unless (< frame (rsound-frames rsound))
    (raise-type-error 'rsound-mutator (format "frame index less than available # of frames ~s" (rsound-frames rsound)) 1 rsound frame new-val))
  (unless (real? new-val)
    (raise-type-error 'rsound-mutator "real number" 2 rsound frame new-val))
  (s16vector-set! (rsound-data rsound)
                  (frame->sample frame left?)
                  (real->s16 new-val)))

;; translate a frame number and a channel into a sample number
(define (frame->sample f left?)
  (+ (* f rc:channels) (if left? 0 1)))

;; return the nth *sample* (not frame) of an rsound.
#;(define (rsound-nth-sample sound sample)
  (unless (rsound? sound)
    (raise-type-error 'rsound-nth-sample/right "rsound" 0 sound sample))
  (unless (positive-integer? sample)
    (raise-type-error 'rsound-nth-sample/right "positive integer" 1 sound sample))
  (match-let* ([(struct rsound (data frames sample-rate)) sound])
    (when (>= sample (* channels frames))
      (error 'rsound-nth-sample "requested sample # ~s greater than available # of samples ~s." sample (* channels (inexact->exact frames))))
    (s16vector-ref data sample)))

;; RSOUND OPERATIONS: subsound, append, overlay, etc...


#;(define (rsound-scale scale sound)
  (unless (rsound? sound)
    (raise-type-error 'rsound-clip "rsound" 0 sound start finish))
  (unless )
  (define (left i) (* scale (rsound-ith/left sound i)))
  (define (right i) (* scale (rsound-ith/right sound i)))
  (signal->rsound/stereo (rsound-frames sound)
                       (rsound-sample-rate sound)
                       left
                       right))

;; rsound-clip : rsound nat nat -> rsound
;; extract a chunk of an rsound, beginning at frame 'start'
;; and ending before frame 'end'. *Does* copy the memory.
;; (an alternate representation could avoid this, if important)
(define (rsound-clip sound start finish)
  (unless (rsound? sound)
    (raise-type-error 'rsound-clip "rsound" 0 sound start finish))
  (unless (nonnegative-integer? start)
    (raise-type-error 'rsound-clip "non-negative integer" 1 sound start finish))
  (unless (nonnegative-integer? finish)
    (raise-type-error 'rsound-clip "non-negative integer" 2 sound start finish))
  (unless (and (<= 0 start finish (rsound-frames sound)))
    (error 'rsound-clip 
           frames-out-of-range-msg
           start finish (rsound-frames sound)))
  (let* ([cblock (make-s16vector (* rc:channels (- finish start)))])
    (memcpy (s16vector->cpointer cblock) 0
            (s16vector->cpointer (rsound-data sound)) (* start rc:channels)
            (* rc:channels (- finish start)) _sint16)
    (rsound cblock (rsound-sample-rate sound))))

(define frames-out-of-range-msg
  (string-append "must have 0 < start < end < frames.  "
                 "You provided start ~s and end ~s for a sound with ~s frames."))

;; rsound-append : rsound rsound -> rsound
(define (rsound-append sound-a sound-b)
  (rsound-append* (list sound-a sound-b)))

;; rsound-append* : (listof rsound) -> rsound
(define (rsound-append* los)
  (unless (and (list? los) (andmap rsound? los))
    (raise-type-error 'rsound-append* "list of rsounds" 0 los))
  (same-sample-rate-check los)
  (define total-frames (apply + (map rsound-frames los)))
  (define cblock (make-s16vector (* rc:channels total-frames)))
  (for/fold ([offset-samples 0])
    ([sound (in-list los)])
    (let ([sound-samples (* rc:channels (rsound-frames sound))])
      (memcpy (s16vector->cpointer cblock) offset-samples
              (s16vector->cpointer (rsound-data sound)) 0
              sound-samples _sint16)
      (+ offset-samples sound-samples)))
  (rsound cblock (rsound-sample-rate (car los))))

;; rsound-overlay* : (listof (list/c rsound nat)) -> rsound
;; overlay all of the sounds at the specified offsets to form one
;; new sound.  
;; ** early implementations were too slow, so now I call out to C.
;;
;; N.B.: currently, summing to larger amplitudes will just wrap.
(define (rsound-overlay* sound&times)
  (unless (and (list? sound&times) 
               (andmap list? sound&times)
               (andmap rsound? (map first sound&times))
               (andmap nonnegative-integer? (map second sound&times)))
    (raise-type-error 'rsound-overlay* "list of lists containing rsounds and times" 0 sound&times))
  (same-sample-rate-check (map car sound&times))
  (let* ([total-frames (inexact->exact (round (sound-list-total-frames sound&times)))]
         [cblock (make-s16vector (* total-frames rc:channels))])
    (memset (s16vector->cpointer cblock) 0 #x00 (* total-frames rc:channels) _sint16)
    (for ([s&t (in-list sound&times)])
      (match-let* ([(list sound offset) s&t]
                   [(struct rsound (s16vec sample-rate)) sound])
        (define frames (/ (s16vector-length s16vec) rc:channels))
        (define dst-offset (* rc:channels (inexact->exact (round offset))))
        (define src-offset 0)
        (define num-samples (* rc:channels frames))
        (s16buffer-add!/c (ptr-add (s16vector->cpointer cblock)
                                   (* s16-size dst-offset))
                          (ptr-add (s16vector->cpointer s16vec)
                                   (* s16-size src-offset))
                          num-samples)))
    (rsound cblock (rsound-sample-rate (caar sound&times)))))



;; add-on-as-computed
(define (mono-fun->buffer-overlay sound offset fun overlay-frames)
  (define frames (rsound-frames sound))
  (match-let ([(struct rsound (data sample-rate)) sound])
    (let ([okay-frames (min overlay-frames (- frames offset))])
    (for ([i (in-range okay-frames)]
          [j (in-range offset (+ offset okay-frames))])
      (s16vector-set! data j 
                      (+ (s16vector-ref data j)
                         (fun i)))))))



;; sound-list-total-frames : (listof (list/c rsound nat)) -> nat
;; how long a sound is needed to hold all of the given sounds & offsets?
(define (sound-list-total-frames sound&times)
  (unless (andmap integer? (map second sound&times))
    (error 'sound-list-total-frames "all offets must be integers, given ~s" (map second sound&times)))
  (apply max (for/list ([s&t (in-list sound&times)])
               (+ (rsound-frames (car s&t)) (cadr s&t)))))


;; same-sample-rate-check : (listof rsound) -> (void)
;; check that the list is of length at least one, and
;; that they all have the same sample rate. Signal
;; an error if the check fails.
(define (same-sample-rate-check los)
  (when (null? los)
    (error 'same-sample-rate-check "can't use empty list (what would the sample rate be?)"))
  (unless (or (<= (length los) 1) (apply = (map rsound-sample-rate los)))
    (error 'same-sample-rate-check "sample rates must all be the same, given: ~s" (map rsound-sample-rate los))))

;; SOUND GENERATION

;; make a monaural sound of the given number of frames at the specified sample-rate
;; using the function 'f' applied to the frame number to generate each sample. It 
;; assumes that the result is a floating-point number between -1 and 1.
(define (mono-signal->rsound frames f)
  (define sample-rate (default-sample-rate))
  (unless (nonnegative-integer? frames)
    (raise-type-error 'signal->rsound "non-negative integer" 0 frames sample-rate f))
  (unless (and (procedure? f) (procedure-arity-includes? f 1))
    (raise-type-error 'signal->rsound "function of one argument" 2 frames sample-rate f)) 
  (let* ([int-frames (inexact->exact (round frames))]
         [int-sample-rate (inexact->exact (round sample-rate))]
         [cblock (make-s16vector (* rc:channels int-frames))])
    (for ([i (in-range int-frames)])
      (let* ([offset (* rc:channels i)]
             [sample (real->s16 (f i))])
        (s16vector-set! cblock offset       sample)
        (s16vector-set! cblock (+ offset 1) sample)))
    (rsound cblock sample-rate)))


;; make a monaural sound of the given number of frames at the specified sample-rate
;; using the function 'f' applied to the frame number to generate each sample. It 
;; assumes that the result is a floating-point number between -1 and 1.
(define (signals->rsound frames fleft fright)
  (define sample-rate (default-sample-rate))
  (unless (nonnegative-integer? frames)
    (raise-type-error 'signal->rsound/stereo "non-negative integer" 0 frames sample-rate fleft fright))
  (unless (and (procedure? fleft) (procedure-arity-includes? fleft 1))
    (raise-type-error 'signal->rsound/stereo "function of one argument" 2 frames sample-rate fleft fright))  
  (unless (and (procedure? fright) (procedure-arity-includes? fright 1))
    (raise-type-error 'signal->rsound/stereo "function of one argument" 3 frames sample-rate fleft fright)) 
  (let* ([cblock (make-s16vector (* rc:channels frames))])
    (for ([i (in-range frames)])
      (let* ([offset (* rc:channels i)])
        (s16vector-set! cblock offset       (real->s16 (fleft i)))
        (s16vector-set! cblock (+ offset 1) (real->s16 (fright i)))))
    (rsound cblock sample-rate)))

(define (signal->rsound/filtered frames filter f)
  (define sample-rate (default-sample-rate))
  (unless (nonnegative-integer? frames)
    (raise-type-error 'fun->filtered-mono-rsound "non-negative integer" 0 frames sample-rate f))
  (unless (and (procedure? f) (procedure-arity-includes? f 1))
    (raise-type-error 'fun->filtered-mono-rsound "function of one argument" 2 frames sample-rate f)) 
  (let* ([cblock (make-s16vector (* rc:channels frames))])
    (for ([i (in-range frames)])
      (let* ([offset (* rc:channels i)]
             [sample (real->s16 (filter (f i)))])
        (s16vector-set! cblock offset       sample)
        (s16vector-set! cblock (+ offset 1) sample)))
    (rsound cblock sample-rate)))


;; special-case silence (it's easy to generate):
(define (make-silence frames)
  (define sample-rate (default-sample-rate))
  (unless (nonnegative-integer? frames)
    (raise-type-error 'make-silence "non-negative integer" 0 frames sample-rate))
  (let* ([cblock (make-s16vector (* rc:channels frames))])
    (memset (s16vector->cpointer cblock) #x0 (* rc:channels frames) _sint16)
    (rsound cblock sample-rate)))


;; CONVERSIONS

(define (s16->real x)
  (/ (exact->inexact x) s16max/i))

(define (real->s16 x)
  (min s16max (max -s16max (inexact->exact (round (* s16max/i x))))))


;; UTILITY FUNCTION (should be defined in a util file?)

(define (rsound-largest-sample sound)
  (buffer-largest-sample (rsound-data sound) (rsound-frames sound)))

(define (rsound-largest-frame/range/left sound min-frame max-frame)
  (buffer-largest-sample/range/left (rsound-data sound) (rsound-frames sound) min-frame max-frame))

(define (rsound-largest-frame/range/right sound min-frame max-frame)
  (buffer-largest-sample/range/right (rsound-data sound) (rsound-frames sound) min-frame max-frame))

(define (buffer-largest-sample buffer frames)
  (buffer-largest-sample/range/helper buffer (* rc:channels frames) 0 (* rc:channels frames) 1))

;; what's the largest sample from min to max-1 ?


;; left-channel only
(define (buffer-largest-sample/range/left buffer frames min-frame max-frame)
  (frame-range-checks frames min-frame max-frame)
  (buffer-largest-sample/range/helper buffer
                                      (* rc:channels frames)
                                      (* rc:channels min-frame)
                                      (* rc:channels max-frame)
                                      2))

;; right channel only
(define (buffer-largest-sample/range/right buffer frames min-frame max-frame)
  (frame-range-checks frames min-frame max-frame)
  (buffer-largest-sample/range/helper buffer
                                      (* rc:channels frames)
                                      (add1 (* rc:channels min-frame))
                                      (add1 (* rc:channels max-frame))
                                      2))

;; sample-based, for internal use only:
(define (buffer-largest-sample/range/helper buffer samples min-sample max-sample increment)
  (for/fold ([max-so-far 0.0])
            ([i (in-range min-sample max-sample increment)])
     (max max-so-far (abs (s16vector-ref buffer i)))))

;; frame-checks
(define (frame-range-checks frames min-frame max-frame)
  (when (not (and (<= 0 min-frame) (<= 0 max-frame)
                  (<= min-frame frames) (<= max-frame frames)))
    (error 'frame-range-checks "range limits ~v and ~v not in range 0 - ~v" min-frame max-frame frames))
  (when (not (< min-frame max-frame))
    (error 'frame-range-checks "range limits ~v and ~v not in order and separated by at least 1" min-frame max-frame)))

;; check-below-threshold : rsound threshhold -> (void)
;; signals an error if any sample is above the threshold
;; IRRELEVANT IN THE S16 WORLD
#;(define (check-below-threshold buffer frames threshold)
  (when (> (buffer-largest-sample buffer frames) threshold)
    (error 'check-below-threshold "sound contains samples above threshold ~s." threshold)))


