#lang racket/base

;; to-do:
;; check okay for inputs to be inexact integers?
;; tighter sanity checks on sample rate?

(require (only-in ffi/unsafe memcpy _sint16 memset ptr-add ctype-sizeof)
         ffi/vector
         "read-wav.rkt"
         "write-wav.rkt"
         "network.rkt"
         "define-argcheck.rkt"
         "common.rkt"
         (prefix-in rc: "rsound-commander.rkt")
         "private/s16vector-add.rkt"
         racket/contract
         racket/match
         racket/list)

(provide (except-out (all-defined-out)
                     s16vector-hash-1
                     s16vector-hash-2
                     rsound-hash-1
                     rsound-hash-2
                     HASH-CONSTANT-1
                     HASH-CONSTANT-2
                     HASH-CONSTANT-3
                     HASH-CONSTANT-4))

(define s16max #x7fff)
(define -s16max (- s16max))
(define s16max/i (exact->inexact #x7fff))
(define s16-size (ctype-sizeof _sint16))

(define CHANNELS rc:channels)
(define stop rc:stop-playing)
;; constants that control the speed testing
(define SPEED-TEST-MAX-FRAMES 1000000)
(define SPEED-TEST-TIME-THRESHOLD-MSEC 400)
(define SPEED-TEST-BUFSIZE (floor (* (default-sample-rate) 0.05))) ;; about 2K frames


;; an rsound (racket sound) provides a representation for sounds 
;; that leaves them packed as C data. For the moment, it's 
;; 2-channel int16 only. Also, it discards all meta-information
;; except length and sample-rate.

;; NB: hashing functions must occur first, so that the hash property
;; can refer to them.

;; are two rsounds equal?
(define/argcheck (rs-equal? [r1 rsound? "rsound"]
                            [r2 rsound? "rsound"])
  (and (= (rs-frames r1)
          (rs-frames r2))
       (= (rsound-sample-rate r1)
          (rsound-sample-rate r2))
       ;; possible shortcut for 'eq?' s16vectors
       (or (eq? (rsound-data r1) (rsound-data r2))
           (for/and ([i (in-range (rs-frames r1))])
             (and (= (rs-ith/left/s16 r1 i) (rs-ith/left/s16 r2 i))
                  (= (rs-ith/right/s16 r1 i) (rs-ith/right/s16 r2 i)))))))

;; a pair of hashing functions for rsounds
(define HASH-CONSTANT-3 669668284)
(define (rsound-hash-1 rs recursive-equal-hash)
  (+ HASH-CONSTANT-3 
     (s16vector-hash-1 (rsound-data rs) recursive-equal-hash)
     (* #x1 (rsound-start rs))
     (* #x100 (rsound-stop rs))
     (* #x10000 (inexact->exact (round (rsound-sample-rate rs))))))

(define HASH-CONSTANT-4 2143890123)
(define (rsound-hash-2 rs recursive-equal-hash)
  (+ HASH-CONSTANT-4 
     (s16vector-hash-2 (rsound-data rs) recursive-equal-hash)
     (* #x10000 (rsound-start rs))
     (* #x100 (rsound-stop rs))
     (* #x1 (inexact->exact (round (rsound-sample-rate rs))))))

;; a pair of hashing functions for s16vectors that represent rsounds
(define HASH-CONSTANT-1 624327903)
(define (s16vector-hash-1 v1 recursive-equal-hash)
  (+ HASH-CONSTANT-1
     (cond [(= (s16vector-length v1) 0) 0]
           [else
            (define quarter-len (/ (s16vector-length v1) 4))
            (+ (* #x80 (s16vector-length v1))
               (s16vector-ref v1 0)
               (* #x10000 (s16vector-ref v1 (floor quarter-len)))
               (s16vector-ref v1 (floor (* 2 quarter-len)))
               (* #x10000 (s16vector-ref v1 (floor (* 3 quarter-len)))))])))

(define HASH-CONSTANT-2 79438529)
(define (s16vector-hash-2 v1 recursive-equal-hash)
  (+ HASH-CONSTANT-2
     (cond [(= (s16vector-length v1) 0) 0]
           [else
            (define quarter-len (/ (s16vector-length v1) 4))
            (+ (* #x100 (s16vector-length v1))
               (s16vector-ref v1 0)
               (s16vector-ref v1 (floor quarter-len))
               (* #x10000 (s16vector-ref v1 (floor (* 2 quarter-len))))
               (* #x10000 (s16vector-ref v1 (floor (* 3 quarter-len)))))])))

;; a rsound is (rsound s16vector positive-integer positive-integer positive)
(struct rsound (data start stop sample-rate) 
  #:transparent
  ;#:property prop:equal+hash
  ;(list rsound=? rsound-hash-1 rsound-hash-2)
  #:methods gen:equal+hash
  [(define equal-proc (lambda (rs1 rs2 _) (rs-equal? rs1 rs2)))
   (define hash-proc rsound-hash-1)
   (define hash2-proc rsound-hash-2)]
  )

;; how many frames long is the sound?
(define/argcheck (rs-frames [rsound rsound? "rsound"])
  (- (rsound-stop rsound) (rsound-start rsound)))

;; given an s16vec and a frame rate, create a new rsound
(define (vec->rsound s16vec sample-rate)
  (when (= (s16vector-length s16vec) 0)
    (raise-argument-error 'rsound/all "s16vector of length > 0" 0 s16vec sample-rate))
  (rsound s16vec 0 (/ (s16vector-length s16vec) CHANNELS) sample-rate))

;; record a sound
(define/argcheck (record-sound [frames nonnegative-integer? "non-negative integer"])
  (vec->rsound (rc:s16vec-record frames (default-sample-rate)) (default-sample-rate)))

;; SIGNAL/BLOCKS

;; can this procedure be used as a signal/block?
(define (signal/block? f)
  (and (procedure? f) (procedure-arity-includes? f 2)))

;; can this procedure be used as a signal/block/unsafe?
(define (signal/block/unsafe? f)
  (and (procedure? f) (procedure-arity-includes? f 2)))

;; ** FILE I/O **

;; just a wrapper around read-sound/floatblock
(define/argcheck (rs-read [path path-string? "path-string"])
  (unless (file-exists? path)
    (raise-argument-error 'rs-read "name of existing file" 0 path))
  (unless (< 0 (file-size path))
    (raise-argument-error 'rs-read "file of length >= 0" 0 path))
  (match (read-sound/s16vector path 0 #f)
    [(list data sample-rate) (vec->rsound data sample-rate)]))

;; read a portion of a sound
(define/argcheck (rs-read/clip [path path-string? "path-string"]
                               [start-frame nonnegative-integer? "non-negative integer"]
                               [end-frame nonnegative-integer? "non-negative integer"])
  (match (read-sound/s16vector path (inexact->exact start-frame) (inexact->exact end-frame))
    [(list data sample-rate) (vec->rsound data sample-rate)]))

;; what is the sample-rate of a file?
(define/argcheck (rs-read-sample-rate [path path-string? "path-string"])
  (second (read-sound/formatting path)))

;; how many frames are in the file?
(define/argcheck (rs-read-frames [path path-string? "path-string"])
  (first (read-sound/formatting path)))

;; just a wrapper around write-sound/floatblock
(define/argcheck (rs-write [sound rsound? "rsound"]
                           [path path-string? "path-string"])
  (match sound
    [(struct rsound (data start stop sample-rate))
     (define exact-integer-frame-rate
       (cond [(integer? sample-rate) (inexact->exact sample-rate)]
             [else
              (raise-argument-error 'rs-write
                                    "rsound with integer sample rate"
                                    0 sound path)]))
     (write-sound/s16vector data start stop exact-integer-frame-rate path)]))

;; play a signal using portaudio:
(define/argcheck (signal-play [signal signal? "signal"])
  (define sample-maker (network-init signal))
  (rc:signal/block-play/unsafe (rc:signal->signal/block/unsafe sample-maker) (default-sample-rate) #f))

;; play an s16-producing signal using portaudio
(define/argcheck (signal-play/16 [signal signal? "signal"])
  (define sample-maker (network-init signal))
  (rc:signal/block-play/unsafe (rc:signal/16->signal/block/unsafe sample-maker) (default-sample-rate) #f))

;; play a signal/block using portaudio:
(define (signal/block-play signal/block sample-rate #:buffer-time [buffer-time #f])
  (unless (signal/block? signal/block)
    (raise-argument-error 'signal/block-play "signal/block" 0 signal/block sample-rate buffer-time))
  (unless (positive-integer? sample-rate)
    (raise-argument-error 'signal/block-play "positive integer" 1 signal/block sample-rate buffer-time))
  (rc:signal/block-play signal/block sample-rate buffer-time))

;; play a signal/block/unsafe using portaudio:
(define (signal/block-play/unsafe signal/block sample-rate #:buffer-time [buffer-time #f])
  (unless (signal/block/unsafe? signal/block)
    (raise-argument-error 'signal/block/unsafe-play "signal/block/unsafe" 0 signal/block sample-rate buffer-time))
  (unless (positive-integer? sample-rate)
    (raise-argument-error 'signal/block/unsafe-play "positive integer" 1 signal/block sample-rate buffer-time))
  (rc:signal/block-play/unsafe signal/block sample-rate buffer-time))

;; play a sound using portaudio:
(define/argcheck (play [sound rsound? "rsound"])
  (match sound
    [(struct rsound (data start finish sample-rate))
     (rc:buffer-play data start finish sample-rate)]
    [other
     ;; should be impossible...
     (error 'rs-play/helper "expected an rsound, got: ~e" sound)])
  "played sound")

;; backup solution: play from a file:
#;(define (play sound)
  (let ([filename (make-temporary-file "tmpsound~a.wav")])
    ;; don't blow out anyone's eardrums. takes about 
    ;; 1 sec per minute of on my 2006 machine.
    (check-below-threshold sound 2.0)
    (rs-write sound filename)
    (thread 
     (lambda ()
       (play-sound filename #f)
       (delete-file filename)))))

;; the following functions are lacking contract-like checks,
;; in order to make them run faster at 44.1K.

;; return the nth sample of an rsound's left channel as a
;; signed 16-bit int
(define (rs-ith/left/s16 sound frame)
  (rs-extractor sound frame #t (lambda (x) x)))

;; return the nth sample of an rsound's right channel
;; as a signed 16-bit int
(define (rs-ith/right/s16 sound frame)
  (rs-extractor sound frame #f (lambda (x) x)))

;; return the nth sample of an rsound's left channel as a real number
(define (rs-ith/left sound frame)
  (rs-extractor sound frame #t s16->real))

;; return the nth sample of an rsound's right channel
;; as a real number in the range -1 <= x <= 1
(define (rs-ith/right sound frame)
  (rs-extractor sound frame #f s16->real))

;; the abstraction behind the last four functions...
(define (rs-extractor rsound frame left? scale-fun)
  (scale-fun (s16vector-ref (rsound-data rsound) 
                            (frame->sample (+ (rsound-start rsound) frame)
                                           left?))))

;; set the ith frame of the left channel to be new-val
(define (set-rs-ith/left! sound frame new-val)
  (rs-mutator sound frame #t new-val real->s16))

;; set the ith frame of the right channel to be new-val
(define (set-rs-ith/right! sound frame new-val)
  (rs-mutator sound frame #f new-val real->s16))

;; set the ith frame of the left channel to be new-val
(define (set-rs-ith/left/s16! sound frame new-val)
  (rs-mutator sound frame #t new-val  (lambda (x) x)))

;; set the ith frame of the right channel to be new-val
(define (set-rs-ith/right/s16! sound frame new-val)
  (rs-mutator sound frame #f new-val (lambda (x) x)))

;; a mutation abstraction:
(define (rs-mutator rsound frame left? new-val scale-fun)
  (s16vector-set! (rsound-data rsound)
                  (frame->sample (+ (rsound-start rsound) frame) left?)
                  (scale-fun new-val)))

;; RSOUND OPERATIONS: subsound, append, overlay, etc...


;; rs-append : rsound rsound -> rsound
(define/argcheck (rs-append [sound-a rsound? "rsound"]
                            [sound-b rsound? "rsound"])
  (rs-append* (list sound-a sound-b)))

;; rs-append* : (listof rsound) -> rsound
(define (rs-append* los)
  (unless (and (list? los) (andmap rsound? los))
    (raise-argument-error 'rs-append* "list of rsounds" 0 los))
  (same-sample-rate-check los)
  (define total-frames (apply + (map rs-frames los)))
  (define cblock (make-s16vector (* rc:channels total-frames)))
  (for/fold ([offset-samples 0])
    ([sound (in-list los)])
    (let ([sound-samples (* rc:channels (rs-frames sound))])
      (memcpy (s16vector->cpointer cblock) offset-samples
              (s16vector->cpointer (rsound-data sound)) 
              (* rc:channels (rsound-start sound))
              sound-samples _sint16)
      (+ offset-samples sound-samples)))
  (vec->rsound cblock (rsound-sample-rate (car los))))

;; rs-overlay* : (listof (list/c rsound nat)) -> rsound
;; overlay all of the sounds at the specified offsets to form one
;; new sound.  
;; ** early implementations were too slow, so now I call out to C.
;;
;; N.B.: currently, summing to larger amplitudes will just wrap.
(define (assemble sound&times)
  (unless (and (list? sound&times)
               (andmap (lambda (x) (and (list? x) (= (length x) 2)
                                        (rsound? (first x)) 
                                        (nonnegative-integer? (second x))))
                       sound&times))
    (raise-argument-error 'assemble "list of (list <rsound> <frame>)"
                      0 sound&times))
  (same-sample-rate-check (map car sound&times))
  (let* ([total-frames (inexact->exact (sound-list-total-frames sound&times))]
         [cblock (make-s16vector (* total-frames rc:channels))])
    (memset (s16vector->cpointer cblock) 0 #x00 (* total-frames rc:channels) _sint16)
    (for ([s&t (in-list sound&times)])
      (match-define (list sound offset/i) s&t)
      (define offset (inexact->exact offset/i))
      (match-define (rsound s16vec start stop sample-rate) sound)
      (define frames (rs-frames sound))
      (define dst-offset (* rc:channels offset))
      (define src-offset (* rc:channels start))
      (define num-samples (* rc:channels frames))
      (define p1 (ptr-add (s16vector->cpointer cblock)
                          (* s16-size dst-offset)))
      (define p2 (ptr-add (s16vector->cpointer s16vec)
                          (* s16-size src-offset)))
      (s16buffer-add!/c p1 p2 num-samples))
    (vec->rsound cblock (rsound-sample-rate (caar sound&times)))))



;; sound-list-total-frames : (listof (list/c rsound nat)) -> nat
;; how long a sound is needed to hold all of the given sounds & offsets?
(define (sound-list-total-frames sound&times)
  (apply max (for/list ([s&t (in-list sound&times)])
               (+ (rs-frames (car s&t)) (cadr s&t)))))


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
;; using the signal 'f'. It 
;; assumes that the result is a floating-point number between -1 and 1.
(define/argcheck (signal->rsound [frames nonnegative-integer? "non-negative integer"]
                                 [f signal? "signal"])
  (define sample-rate (default-sample-rate))
  (define sample-maker (network-init f))
  (define int-frames (inexact->exact (floor frames)))
  (define cblock (make-s16vector (* rc:channels int-frames)))
  (for ([i (in-range int-frames)])
    (let* ([offset (* rc:channels i)]
           [sample (real->s16 (sample-maker))])
      ;; I believe these could safely be unsafe.
      (s16vector-set! cblock offset       sample)
      (s16vector-set! cblock (+ offset 1) sample)))
  (vec->rsound cblock sample-rate))


;; make a monaural sound of the given number of frames at the specified sample-rate
;; using the function 'f' applied to the frame number to generate each sample. It 
;; assumes that the result is a floating-point number between -1 and 1.
(define/argcheck (signals->rsound [frames nonnegative-integer? "non-negative integer"]
                         [fleft signal? "signal"] 
                         [fright signal? "signal"])
  (define sample-rate (default-sample-rate)) 
  (define int-frames (inexact->exact frames))
  (define sample-maker/left (network-init fleft))
  (define sample-maker/right (network-init fright))
  (let* ([cblock (make-s16vector (* rc:channels int-frames))])
    (for ([i (in-range int-frames)])
      (let* ([offset (* rc:channels i)])
        ;; I believe these could safely be unsafe
        (s16vector-set! cblock offset       (real->s16 (sample-maker/left)))
        (s16vector-set! cblock (+ offset 1) (real->s16 (sample-maker/right)))))
    (vec->rsound cblock sample-rate)))

;; special-case silence (it's fast to generate):
(define/argcheck (silence [frames positive-integer? "positive integer"])
  (define sample-rate (default-sample-rate))
  (define int-frames (inexact->exact frames))
  (let* ([cblock (make-s16vector (* rc:channels int-frames))])
    (memset (s16vector->cpointer cblock) #x0 (* rc:channels int-frames) _sint16)
    (vec->rsound cblock sample-rate)))


;; apply a filter to a sound (left and right are filtered
;; individually
;; rsound filter -> rsound
(define/argcheck (rs-filter [sound rsound? "rsound"]
                            [filter network/s? "network"])
  (define left-filter (network-init filter))
  (define right-filter (network-init filter))
  (define output-s16vec (make-s16vector (* CHANNELS (rs-frames sound))))
  (for ([i (rs-frames sound)])
    ;; could be faster with unsafe write and read...
    (s16vector-set! output-s16vec       (* i CHANNELS)  (real->s16 (left-filter (rs-ith/left sound i))))
    (s16vector-set! output-s16vec (add1 (* i CHANNELS)) (real->s16 (right-filter (rs-ith/right sound i))))
    )
  (vec->rsound output-s16vec (rsound-sample-rate sound)))

;; SPEED TESTING
;; test the time taken per frame for a given signal.
;; generates an rsound containing a bunch of frames.
(define (signal-speed-test signal)
  (define cblock (make-s16vector (inexact->exact (* rc:channels SPEED-TEST-BUFSIZE))))
  (define sample-maker (network-init signal))
  (define (generate-samples n)
    (for ([dc (in-range (floor (/ n SPEED-TEST-BUFSIZE)))])
      (for ([i (in-range SPEED-TEST-BUFSIZE)])
        (let* ([offset (* rc:channels i)]
               [sample (real->s16 (sample-maker))])
          ;; I believe these could safely be unsafe.
          (s16vector-set! cblock offset       sample)
          (s16vector-set! cblock (+ offset 1) sample))))
    ;; leftovers:
    (for ([i (in-range (remainder n SPEED-TEST-BUFSIZE))])
      (let* ([offset (* rc:channels i)]
             [sample (real->s16 (sample-maker))])
        ;; I believe these could safely be unsafe.
        (s16vector-set! cblock offset       sample)
        (s16vector-set! cblock (+ offset 1) sample))))
  ;; try with larger and larger #s of frames until 
  ;; it gets way too big or the time is too high
  (let loop ([num-frames 1])
    (printf "testing with ~s frame(s)\n" num-frames)
    (define-values (_ cpu-msec wall-clock-msec gc-msec)
      (time-apply generate-samples (list num-frames)))
    (cond [(or (<= SPEED-TEST-MAX-FRAMES num-frames)
               (<= SPEED-TEST-TIME-THRESHOLD-MSEC wall-clock-msec))
           (speed-report num-frames cpu-msec wall-clock-msec gc-msec)]
          [else
           (speed-report num-frames cpu-msec wall-clock-msec gc-msec)
           (loop (* 10 num-frames))])))

(define (speed-report num-frames cpu-msec wall-clock-msec gc-msec)
  (printf "test took ~s msec of cpu time, ~s msec of wall clock time, and ~s msec of gc time.\n"
          cpu-msec wall-clock-msec gc-msec)
  (printf "cpu msec / frame: ~s\n" (exact->inexact (/ cpu-msec num-frames)))
  (printf "load (wall-clock-msec-per-frame / msec-allowed-per-frame): ~s %\n"
          (round (* 100 (/ (/ wall-clock-msec num-frames) (/ 1000 (default-sample-rate)))))))

;; CONVERSIONS

(define (s16->real x)
  (/ (exact->inexact x) s16max/i))

(define (real->s16 x)
  (min s16max (max -s16max (inexact->exact (round (* s16max/i x))))))


;; check-below-threshold : rsound threshhold -> (void)
;; signals an error if any sample is above the threshold
;; IRRELEVANT IN THE S16 WORLD
#;(define (check-below-threshold buffer frames threshold)
  (when (> (buffer-largest-sample buffer frames) threshold)
    (error 'check-below-threshold "sound contains samples above threshold ~s." threshold)))


