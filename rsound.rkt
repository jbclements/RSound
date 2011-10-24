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
(struct rsound (data start stop sample-rate) 
  #:transparent
  ;#:property prop:equal+hash
  ;(list rsound=? rsound-hash-1 rsound-hash-2)
  )

(define s&t-list? (listof (list/c rsound? number?)))

(provide/contract 
                  ;; for testing...
                  [sound-list-total-frames (-> s&t-list? number?)])

(provide (except-out (all-defined-out) sound-list-total-frames))

(define s16max #x7fff)
(define -s16max (- s16max))
(define s16max/i (exact->inexact #x7fff))
(define s16-size 2)

(define channels rc:channels)
(define stop rc:stop-playing)

;; used for creating sounds; specifying the 
;; sample rate every time is too much of a pain
;; for students.
(define default-sample-rate (make-parameter 44100))

;; an rsound (racket sound) provides a representation for sounds 
;; that leaves them packed as C data. For the moment, it's 
;; 2-channel float only. Also, it discards all meta-information
;; except length and sample-rate.


(define (rsound-frames rsound)
  (- (rsound-stop rsound) (rsound-start rsound)))

;; fill in 0 and max-frames for a newly created rsound
(define (rsound/all s16vec sample-rate)
  (rsound s16vec 0 (/ (s16vector-length s16vec) channels) sample-rate))

;; an rdata is either
;; an s16vector, 
;; a function from time to a pair of real numbers in the range -1 to 1, or
;; a function of no arguments that produces real numbers in the range -1 to 1.

(define (rsound-equal? r1 r2)
  (and (= (rsound-frames r1)
          (rsound-frames r2))
       (= (rsound-sample-rate r1)
          (rsound-sample-rate r2))
       (for/and ([i (in-range (rsound-frames r1))])
         (and (= (rs-ith/left/s16 r1 i) (rs-ith/left/s16 r2 i))
              (= (rs-ith/right/s16 r1 i) (rs-ith/right/s16 r2 i))))))

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
(define (rs-read path)
  (unless (path-string? path)
    (raise-type-error 'rsound-read "path-string" 0 path))
  (match (read-sound/s16vector path 0 #f)
    [(list data sample-rate) (rsound/all data sample-rate)]))

;; read a portion of a sound
(define (rs-read/clip path start-frame end-frame)
  (unless (path-string? path)
    (raise-type-error 'rsound-read "path-string" 0 path start-frame end-frame))
  (unless (nonnegative-integer? start-frame)
    (raise-type-error 'rsound-read "non-negative integer" 1 path start-frame end-frame))
  (unless (nonnegative-integer? end-frame)
    (raise-type-error 'rsound-read "non-negative integer" 2 path start-frame end-frame))
  (match (read-sound/s16vector path (inexact->exact start-frame) (inexact->exact end-frame))
    [(list data sample-rate) (rsound/all data sample-rate)]))

;; what is the sample-rate of a file?
(define (rs-read-sample-rate path)
  (unless (path-string? path)
    (raise-type-error 'rsound-read-sample-rate "path-string" 0 path))
  (second (read-sound/formatting path)))

;; how many frames are in the file?
(define (rs-read-frames path)
  (unless (path-string? path)
    (raise-type-error 'rsound-read-frames "path-string" 0 path))
  (first (read-sound/formatting path)))

;; just a wrapper around write-sound/floatblock
(define (rs-write sound path)
  (unless (rsound? sound)
    (raise-type-error 'rsound-write "rsound" 0 sound path))
  (unless (path-string? path)
    (raise-type-error 'rsound-write "path" 1 sound path))
  (match sound
    [(struct rsound (data start stop sample-rate))
     (write-sound/s16vector data start stop sample-rate path)]))

;; play a signal using portaudio:
(define (signal-play signal sample-rate)
  (unless (and (procedure? signal)
               (procedure-arity-includes? signal 1))
    (raise-type-error 'signal-play "signal" 0 signal sample-rate))
  (unless (positive-integer? sample-rate)
    (raise-type-error 'signal-play "sample rate (nonnegative exact integer)" 1 signal sample-rate))
  (rc:signal/block-play/unsafe (rc:signal->signal/block/unsafe signal) sample-rate))

;; play a signal/block using portaudio:
(define (signal/block-play signal/block sample-rate)
  (rc:signal/block-play signal/block sample-rate))

;; play a signal/block using portaudio:
(define (signal/block-play/unsafe signal/block sample-rate)
  (rc:signal/block-play/unsafe signal/block sample-rate))

;; play a sound using portaudio:
(define ((rsound-play/helper loop?) sound)
  (match sound
    [(struct rsound (data start finish sample-rate))
     (if loop?
         (error 'rsound-play/helper "not implemented")
         (rc:buffer-play data start finish sample-rate))]
    [other
     (error 'rsound-play/helper "expected an rsound, got: ~e" sound)]))

;; play an rsound
(define play 
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
(define (rs-ith/left/s16 sound frame)
  (rsound-extractor sound frame #t (lambda (x) x)))

;; return the nth sample of an rsound's right channel
(define (rs-ith/right/s16 sound frame)
  (rsound-extractor sound frame #f (lambda (x) x)))

(define (rs-ith/left sound frame)
  (rsound-extractor sound frame #t s16->real))

(define (rs-ith/right sound frame)
  (rsound-extractor sound frame #f s16->real))

;; the abstraction behind the last four functions...
(define (rsound-extractor rsound frame left? scale-fun)
  (scale-fun (s16vector-ref (rsound-data rsound) (frame->sample (+ (rsound-start rsound) frame) left?))))


;; set the ith frame of the left channel to be new-val
(define (set-rs-ith/left! sound frame new-val)
  (rsound-mutator sound frame #t new-val real->s16))

;; set the ith frame of the right channel to be new-val
(define (set-rs-ith/right! sound frame new-val)
  (rsound-mutator sound frame #f new-val real->s16))

;; set the ith frame of the left channel to be new-val
(define (set-rs-ith/left/s16! sound frame new-val)
  (rsound-mutator sound frame #t new-val  (lambda (x) x)))

;; set the ith frame of the right channel to be new-val
(define (set-rs-ith/right/s16! sound frame new-val)
  (rsound-mutator sound frame #f new-val (lambda (x) x)))



;; a mutation abstraction:
(define (rsound-mutator rsound frame left? new-val scale-fun)
  #;(unless (rsound? rsound)
    (raise-type-error 'rsound-mutator "rsound" 0 rsound frame new-val))
  #;(unless (nonnegative-integer? frame)
    (raise-type-error 'rsound-mutator "nonnegative integer" 1 rsound frame new-val))
  #;(unless (< frame (rsound-frames rsound))
    (raise-type-error 'rsound-mutator (format "frame index less than available # of frames ~s" (rsound-frames rsound)) 1 rsound frame new-val))
  #;(unless (real? new-val)
    (raise-type-error 'rsound-mutator "real number" 2 rsound frame new-val))
  (s16vector-set! (rsound-data rsound)
                  (frame->sample (+ (rsound-start rsound) frame) left?)
                  (scale-fun new-val)))

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
  (define (left i) (* scale (rs-ith/left sound i)))
  (define (right i) (* scale (rs-ith/right sound i)))
  (signal->rsound/stereo (rsound-frames sound)
                       (rsound-sample-rate sound)
                       left
                       right))

;; rsound-clip : rsound nat nat -> rsound
;; extract a chunk of an rsound, beginning at frame 'start'
;; and ending before frame 'end'. 
(define (clip sound start finish)
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
  (match-define (rsound data old-start old-stop sample-rate) sound)
  (rsound data (+ old-start start) (+ old-start finish) sample-rate)
  ;; NOT COPYING ANYMORE....
  #;(let* ([cblock (make-s16vector (* rc:channels (- finish start)))])
      (memcpy (s16vector->cpointer cblock) 0
              (s16vector->cpointer (rsound-data sound)) (* start rc:channels)
              (* rc:channels (- finish start)) _sint16)
      (rsound cblock (rsound-sample-rate sound))))

(define frames-out-of-range-msg
  (string-append "must have 0 < start < end < frames.  "
                 "You provided start ~s and end ~s for a sound with ~s frames."))

;; rsound-append : rsound rsound -> rsound
(define (rs-append sound-a sound-b)
  (rs-append* (list sound-a sound-b)))

;; rsound-append* : (listof rsound) -> rsound
(define (rs-append* los)
  (unless (and (list? los) (andmap rsound? los))
    (raise-type-error 'rsound-append* "list of rsounds" 0 los))
  (same-sample-rate-check los)
  (define total-frames (apply + (map rsound-frames los)))
  (define cblock (make-s16vector (* rc:channels total-frames)))
  (for/fold ([offset-samples 0])
    ([sound (in-list los)])
    (let ([sound-samples (* rc:channels (rsound-frames sound))])
      (memcpy (s16vector->cpointer cblock) offset-samples
              (s16vector->cpointer (rsound-data sound)) 
              (* rc:channels (rsound-start sound))
              sound-samples _sint16)
      (+ offset-samples sound-samples)))
  (rsound cblock 0 total-frames (rsound-sample-rate (car los))))

;; rsound-overlay* : (listof (list/c rsound nat)) -> rsound
;; overlay all of the sounds at the specified offsets to form one
;; new sound.  
;; ** early implementations were too slow, so now I call out to C.
;;
;; N.B.: currently, summing to larger amplitudes will just wrap.
(define (assemble sound&times)
  (same-sample-rate-check (map car sound&times))
  (let* ([total-frames (inexact->exact (ceiling (sound-list-total-frames sound&times)))]
         [cblock (make-s16vector (* total-frames rc:channels))])
    (memset (s16vector->cpointer cblock) 0 #x00 (* total-frames rc:channels) _sint16)
    (for ([s&t (in-list sound&times)])
      (match-define (list sound offset) s&t)
      (match-define (rsound s16vec start stop sample-rate) sound)
      (define frames (rsound-frames sound))
      (when (< (inexact->exact (floor offset)) 0)
        (error 'assemble "given target offset less than zero: ~s" 
               offset))
      (define dst-offset (* rc:channels (inexact->exact (floor offset))))
      (define src-offset (* rc:channels start))
      (define num-samples (* rc:channels frames))
      (define p1 (ptr-add (s16vector->cpointer cblock)
                          (* s16-size dst-offset)))
      (define p2 (ptr-add (s16vector->cpointer s16vec)
                          (* s16-size src-offset)))
      (s16buffer-add!/c p1 p2 num-samples))
    (rsound cblock 0 total-frames (rsound-sample-rate (caar sound&times)))))



;; sound-list-total-frames : (listof (list/c rsound nat)) -> nat
;; how long a sound is needed to hold all of the given sounds & offsets?
(define (sound-list-total-frames sound&times)
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
    (rsound cblock 0 int-frames sample-rate)))


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
    (rsound cblock 0 frames sample-rate)))


;; UNTESTED!
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
    (rsound cblock 0 frames sample-rate)))


;; special-case silence (it's easy to generate):
(define (silence frames)
  (define sample-rate (default-sample-rate))
  (define int-frames (inexact->exact (round frames)))
  (unless (nonnegative-integer? frames)
    (raise-type-error 'make-silence "non-negative integer" 0 frames sample-rate))
  (let* ([cblock (make-s16vector (* rc:channels int-frames))])
    (memset (s16vector->cpointer cblock) #x0 (* rc:channels int-frames) _sint16)
    (rsound cblock 0 int-frames sample-rate)))


;; CONVERSIONS

(define (s16->real x)
  (/ (exact->inexact x) s16max/i))

(define (real->s16 x)
  (min s16max (max -s16max (inexact->exact (round (* s16max/i x))))))


;; UTILITY FUNCTION (should be defined in a util file?)

;; this stuff is a bit of a mess... the frames don't need to be passed around, the 
;; whole thing is just a bit wordy.

(define (rs-largest-sample sound)
  (buffer-largest-sample/range (rsound-data sound) (rsound-start sound) (rsound-stop sound)
                               (rsound-frames sound)))

(define (rs-largest-frame/range/left sound min-frame max-frame)
  (buffer-largest-sample/range/left (rsound-data sound) (rsound-frames sound) min-frame max-frame))

(define (rs-largest-frame/range/right sound min-frame max-frame)
  (buffer-largest-sample/range/right (rsound-data sound) (rsound-frames sound) min-frame max-frame))

(define (buffer-largest-sample/range buffer start stop frames)
  (buffer-largest-sample/range/helper buffer (* rc:channels frames) (* rc:channels start) 
                                      (* rc:channels stop) 1))

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


