#lang racket

(require ffi/unsafe
         ffi/vector
         "read-wav.rkt"
         "write-wav.rkt"
         ;; sndfile not available on windows...
         #;ffi/examples/sndfile
         (prefix-in link: "private/drracket-link.rkt"))


(provide (struct-out rsound)
         rsound-play
         rsound-loop
         stop-playing
         change-loop
         rsound-nth-sample
         rsound-nth-sample/left
         rsound-nth-sample/right
         rsound-ith/left
         rsound-ith/right
         rsound-clip
         rsound-append
         rsound-append*
         rsound-overlay*
         fun->mono-rsound
         funs->stereo-rsound
         fun->filtered-mono-rsound
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
(define s16max/i (exact->inexact #x7fff))

;; an rsound (racket sound) provides a representation for sounds 
;; that leaves them packed as C data. For the moment, it's 
;; 2-channel float only. Also, it discards all meta-information
;; except length and sample-rate.

;; a rsound is (rsound s16vector nat)
(provide (struct-out rsound))
(define-struct rsound (data frames sample-rate))

;; this is a fixed global for rsounds:
(define channels 2)

;; ** FILE I/O **

;; just a wrapper around read-sound/floatblock
(define (rsound-read path)
  (unless (path-string? path)
    (raise-type-error 'rsound-read "path-string" 0 path))
  (match (read-sound/s16vector path 0 #f)
    [(list data frames sample-rate) (make-rsound data frames sample-rate)]))

;; read a portion of a sound
(define (rsound-read/clip path start-frame end-frame)
  (unless (path-string? path)
    (raise-type-error 'rsound-read "path-string" 0 path start-frame end-frame))
  (unless (and (integer? start-frame) (>= start-frame 0))
    (raise-type-error 'rsound-read "non-negative integer" 1 path start-frame end-frame))
    (unless (and (integer? end-frame) (>= end-frame 0))
    (raise-type-error 'rsound-read "non-negative integer" 2 path start-frame end-frame))
  (match (read-sound/s16vector path start-frame end-frame)
    [(list data frames sample-rate) (make-rsound data frames sample-rate)]))

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
;; commenting out until we have write-wav written...
(define (rsound-write sound path)
  (unless (rsound? sound)
    (raise-type-error 'rsound-write "rsound" 0 sound path))
  (unless (string? path)
    (raise-type-error 'rsound-write "path" 1 sound path))
  (match sound
    [(struct rsound (data frames sample-rate))
     ;; don't blow out anyone's eardrums. takes about 
     ;; 1 sec per minute on my 2006 machine.
     ;; IRRELEVANT IN THE S16 WORLD
     #;(check-below-threshold data frames 4.0)
     (write-sound/s16vector data sample-rate path)]))

;; ** SP3AKeR I/O **

;; play a sound using portaudio:
(define ((rsound-play/helper loop?) sound)
  (match sound
    [(struct rsound (data frames sample-rate))
     ;; don't destroy anyone's eardrums:
     ;; IRRELEVANT IN THE S16 WORLD
     #;(check-below-threshold data frames 4.0)
     (when (custodian? (unbox link:link))
       (error 'rsound-play "rsound play thread is uninitialized. Perhaps you just installed the planet package and need to restart?"))
     (if loop?
         (send (unbox link:link) loop-sound data frames sample-rate)
         (send (unbox link:link) play-sound data frames sample-rate))]
    [other
     (error 'rsound-play/helper "expected an rsound, got: ~e" sound)]))

;; play an rsound
(define rsound-play 
  (rsound-play/helper #f))

;; loop an rsound endlessly
(define rsound-loop 
  (rsound-play/helper #t))
 
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

;; stop the currently-playing sound
(define (stop-playing)
  (when (custodian? (unbox link:link))
    (error 'rsound-play "rsound play thread is uninitialized. Perhaps you just installed the planet package and need to restart?"))
  (send (unbox link:link) stop-playing))

;; change the loop that's currently playing. Has no effect if a 
;; loop isn't currently playing.
;; ** ASSUMES SAMPLE-RATE IS UNCHANGED **
(define (change-loop sound)
  (unless (rsound? sound)
    (raise-type-error 'change-loop "rsound" 0 sound))
  (match sound 
    [(struct rsound (data frames sample-rate))
     (send (unbox link:link) change-loop data frames)]
    [other 
     (error 'change-loop "expected an rsound, got: ~e" sound)]))

;; return the nth sample of an rsound's left channel.
(define (rsound-nth-sample/left sound frame)
  (rsound-extractor sound frame #true (lambda (x) x)))

;; return the nth sample of an rsound's right channel
(define (rsound-nth-sample/right sound frame)
  (rsound-extractor sound frame #false (lambda (x) x)))

(define (rsound-ith/left sound frame)
  (rsound-extractor sound frame #true (lambda (x) (/ (exact->inexact x) s16max/i))))

(define (rsound-ith/right sound frame)
  (rsound-extractor sound frame #false (lambda (x) (/ (exact->inexact x) s16max/i))))

;; the abstraction behind the last four functions...
(define (rsound-extractor rsound frame left? scale-fun)
  (unless (rsound? rsound)
    (raise-type-error 'rsound-extractor "rsound" 0 rsound frame))
  (unless (frame? frame)
    (raise-type-error 'rsound-extractor "nonnegative integer" 1 rsound frame))
  (unless (< frame (rsound-frames rsound))
    (raise-type-error 'rsound-extractor (format "frame index less than available # of frames ~s" (rsound-frames rsound)) 1 rsound frame))
  (scale-fun (s16vector-ref (rsound-data rsound) (+ (* frame channels) (if left? 0 1)))))

;; return the nth *sample* (not frame) of an rsound.
(define (rsound-nth-sample sound sample)
  (unless (rsound? sound)
    (raise-type-error 'rsound-nth-sample/right "rsound" 0 sound sample))
  (unless (frame? sample)
    (raise-type-error 'rsound-nth-sample/right "positive integer" 1 sound sample))
  (match-let* ([(struct rsound (data frames sample-rate)) sound])
    (when (>= sample (* channels frames))
      (error 'rsound-nth-sample "requested sample # ~s greater than available # of samples ~s." sample (* channels frames)))
    (s16vector-ref data sample)))

;; rsound->list : rsound -> (listof (list/c sample sample))
;; translate an rsound into a list of frames, where each
;; frame is represented as a list of two samples
#;(define (rsound->list sound)
  (let* ([(struct )])))

;; RSOUND OPERATIONS: subsound, append, overlay, etc...



;; rsound-clip : rsound nat nat -> rsound
;; extract a chunk of an rsound, beginning at frame 'start'
;; and ending before frame 'end'. *Does* copy the memory.
;; (an alternate representation could avoid this, if important)
(define (rsound-clip sound start finish)
  (unless (rsound? sound)
    (raise-type-error 'rsound-clip "rsound" 0 sound start finish))
  (unless (frame? start)
    (raise-type-error 'rsound-clip "non-negative integer" 1 sound start finish))
  (unless (frame? finish)
    (raise-type-error 'rsound-clip "non-negative integer" 2 sound start finish))
  (unless (and (<= 0 start finish (rsound-frames sound)))
    (error 'rsound-clip "must have 0 < start < end < frames.  You provided start ~s and end ~s for a sound with ~s frames."
           start finish (rsound-frames sound)))
  (let* ([cblock (make-s16vector (* channels (- finish start)))])
    (memcpy (s16vector->cpointer cblock) 0
            (s16vector->cpointer (rsound-data sound)) (* start channels)
            (* channels (- finish start)) _sint16)
    (rsound cblock (- finish start) (rsound-sample-rate sound))))

;; rsound-append : rsound rsound -> rsound
(define (rsound-append sound-a sound-b)
  (rsound-append* (list sound-a sound-b)))

;; rsound-append* : (listof rsound) -> rsound
(define (rsound-append* los)
  (same-sample-rate-check los)
  (let* ([total-frames (apply + (map rsound-frames los))]
         [cblock (make-s16vector (* channels total-frames))])
    (for/fold ([offset-samples 0])
      ([sound (in-list los)])
      (let ([sound-samples (* channels (rsound-frames sound))])
        (memcpy (s16vector->cpointer cblock) offset-samples
                (s16vector->cpointer (rsound-data sound)) 0
                sound-samples _sint16)
        (+ offset-samples sound-samples)))
    (rsound cblock total-frames (rsound-sample-rate (car los)))))

;; rsound-overlay* : (listof (list/c rsound nat)) -> rsound
;; overlay all of the sounds at the specified offsets to form one
;; new sound.  
;; ** there are a bunch of different implementation schemes here;
;; I'm picking the straightforward one (go through each input, add
;; it to the accumulator one frame at a time), and we'll see how it
;; performs.
(define (rsound-overlay* sound&times)
  (unless (and (list? sound&times) 
               (andmap list? sound&times)
               (andmap rsound? (map first sound&times))
               (andmap nonnegative-integer? (map second sound&times)))
    (raise-type-error 'rsound-overlay* "list of lists containing rsounds and times" 0 sound&times))
  (same-sample-rate-check (map car sound&times))
  (let* ([total-frames (inexact->exact (round (sound-list-total-frames sound&times)))]
         [cblock (make-s16vector (* total-frames channels))])
    (memset (s16vector->cpointer cblock) 0 #x00 (* total-frames channels) _sint16)
    (for ([s&t (in-list sound&times)])
      (match-let* ([(list sound offset) s&t]
                   [(struct rsound (src-buffer frames sample-rate)) sound])
        (for ([i (in-range (* channels frames))]
              [j (in-range (* channels (inexact->exact (round offset))) (* channels (+ (inexact->exact (round offset)) frames)))])
          (s16vector-set! cblock j
                          (+ (s16vector-ref cblock j)
                             (s16vector-ref src-buffer i))))))
    (rsound cblock total-frames (rsound-sample-rate (caar sound&times)))))

;; add-on-as-computed
(define (mono-fun->buffer-overlay sound offset fun overlay-frames)
  (match-let ([(struct rsound (data frames sample-rate)) sound]) 
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
(define (fun->mono-rsound frames sample-rate f)
  (unless (frame? frames)
    (raise-type-error 'fun->mono-rsound "non-negative integer" 0 frames sample-rate f))
  (unless (sample-rate? sample-rate)
    (raise-type-error 'fun->mono-rsound "positive integer" 1 frames sample-rate f))
  (unless (and (procedure? f) (procedure-arity-includes? f 1))
    (raise-type-error 'fun->mono-rsound "function of one argument" 2 frames sample-rate f)) 
  (let* ([int-frames (inexact->exact (round frames))]
         [int-sample-rate (inexact->exact (round sample-rate))]
         [cblock (make-s16vector (* channels int-frames))])
    (for ([i (in-range int-frames)])
      (let* ([offset (* 2 i)]
             [sample (inexact->s16 (f i))])
        (s16vector-set! cblock offset       sample)
        (s16vector-set! cblock (+ offset 1) sample)))
    (rsound cblock int-frames int-sample-rate)))


;; make a monaural sound of the given number of frames at the specified sample-rate
;; using the function 'f' applied to the frame number to generate each sample. It 
;; assumes that the result is a floating-point number between -1 and 1.
(define (funs->stereo-rsound frames sample-rate fleft fright)
  (unless (frame? frames)
    (raise-type-error 'funs->stereo-rsound "non-negative integer" 0 frames sample-rate fleft fright))
  (unless (sample-rate? sample-rate)
    (raise-type-error 'funs->stereo-rsound "positive integer" 1 frames sample-rate fleft fright))
  (unless (and (procedure? fleft) (procedure-arity-includes? fleft 1))
    (raise-type-error 'funs->stereo-rsound "function of one argument" 2 frames sample-rate fleft fright))  
  (unless (and (procedure? fright) (procedure-arity-includes? fright 1))
    (raise-type-error 'funs->stereo-rsound "function of one argument" 3 frames sample-rate fleft fright)) 
  (let* ([int-frames (inexact->exact (round frames))]
         [int-sample-rate (inexact->exact (round sample-rate))]
         [cblock (make-s16vector (* channels int-frames))])
    (for ([i (in-range int-frames)])
      (let* ([offset (* 2 i)])
        (s16vector-set! cblock offset       (inexact->s16 (fleft i)))
        (s16vector-set! cblock (+ offset 1) (inexact->s16 (fright i)))))
    (rsound cblock int-frames int-sample-rate)))

(define (fun->filtered-mono-rsound frames sample-rate filter f)
  (unless (frame? frames)
    (raise-type-error 'fun->filtered-mono-rsound "non-negative integer" 0 frames sample-rate f))
  (unless (sample-rate? sample-rate)
    (raise-type-error 'fun->filtered-mono-rsound "positive integer" 1 frames sample-rate f))
  (unless (and (procedure? f) (procedure-arity-includes? f 1))
    (raise-type-error 'fun->filtered-mono-rsound "function of one argument" 2 frames sample-rate f)) 
  (let* ([int-frames (inexact->exact (round frames))]
         [int-sample-rate (inexact->exact (round sample-rate))]
         [cblock (make-s16vector (* channels int-frames))])
    (for ([i (in-range int-frames)])
      (let* ([offset (* 2 i)]
             [sample (inexact->s16 (filter (f i)))])
        (s16vector-set! cblock offset       sample)
        (s16vector-set! cblock (+ offset 1) sample)))
    (rsound cblock int-frames int-sample-rate)))


;; special-case silence (it's easy to generate):
(define (make-silence frames sample-rate)
  (unless (frame? frames)
    (raise-type-error 'make-silence "non-negative integer" 0 frames sample-rate))
  (unless (sample-rate? sample-rate)
    (raise-type-error 'make-silence "positive integer" 1 frames sample-rate))
  (let* ([int-frames (inexact->exact (round frames))]
         [int-sample-rate (inexact->exact (round sample-rate))]
         [cblock (make-s16vector (* channels int-frames))])
    (memset (s16vector->cpointer cblock) #x0 (* channels int-frames) _sint16)
    (rsound cblock int-frames int-sample-rate)))


;; TYPE-LIKE CHECKS:
(define (sample-rate? s)
  (and (exact-integer? s) (< 0 s)))

(define (frame? f)
  (and (exact-integer? f) (<= 0 f)))

(define (inexact->s16 x)
  (inexact->exact (round (* s16max/i (max -1.0 (min 1.0 x))))))

(define (nonnegative-integer? i)
  (and (integer? i) (<= 0 i)))


;; UTILITY FUNCTION (should be defined in a util file?)

(define (rsound-largest-sample sound)
  (buffer-largest-sample (rsound-data sound) (rsound-frames sound)))

(define (rsound-largest-frame/range/left sound min-frame max-frame)
  (buffer-largest-sample/range/left (rsound-data sound) (rsound-frames sound) min-frame max-frame))

(define (rsound-largest-frame/range/right sound min-frame max-frame)
  (buffer-largest-sample/range/right (rsound-data sound) (rsound-frames sound) min-frame max-frame))

(define (buffer-largest-sample buffer frames)
  (buffer-largest-sample/range/helper buffer (* channels frames) 0 (* channels frames) 1))

;; what's the largest sample from min to max-1 ?


;; left-channel only
(define (buffer-largest-sample/range/left buffer frames min-frame max-frame)
  (frame-range-checks frames min-frame max-frame)
  (buffer-largest-sample/range/helper buffer (* channels frames) (* channels min-frame) (* channels max-frame) 2))

;; right channel only
(define (buffer-largest-sample/range/right buffer frames min-frame max-frame)
  (frame-range-checks frames min-frame max-frame)
  (buffer-largest-sample/range/helper buffer (* channels frames) (add1 (* channels min-frame)) (add1 (* channels max-frame)) 2))

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


