#lang racket

;; untested as of 2011-09:
#|make-squaretone
make-zugtone
make-sawtooth-tone
signal-*
signal-+s
split-in-4
times
adsr
rsound-fft/right
rsound-max-volume
|#

(require "rsound.rkt"
         "fft.rkt"
         "integral-cycles.rkt"
         racket/flonum
         ffi/vector
         (for-syntax syntax/parse))

(define twopi (* 2 pi))


(provide rs-map
         rs-map/idx
         scale
         resample
         clip
         rs-mult
         twopi
         sine-wave
         sawtooth-wave
         approx-sawtooth-wave
         square-wave
         harm3-wave
         noise
         rearrange
         ;; functions on numbers
         thresh
         ;; envelope-funs
         fader
         frisellinator
         ;; special signals
         dc-signal
         ;; signal combiners
         signal-*s
         signal-+s
         thresh/signal
         
         signal-scale
         clip&volume
         rsound->signal/left
         rsound->signal/right
         rs-largest-sample
         ;; rsound makers
         make-tone
         make-squaretone
         make-sawtooth-tone
         make-zugtone
         make-harm3tone
         make-pulse-tone
         make-square-fade-tone
         make-sawtooth-fade-tone
         wavefun->tone-maker
         ding
         make-ding
         split-in-4
         times
         overlay*
         overlay
         mono
         vectors->rsound
         tile-to-len
         fader-snd
         
         rsound-fft/left
         rsound-fft/right
         rsound-max-volume
         signal
         midi-note-num->pitch
         ;; for testing:
         raw-sine-wave
         raw-square-wave
         raw-sawtooth-wave
         binary-logn
         )


;; given a function from numbers to numbers and an rsound, 
;; produce a new rsound where every sample is modified 
;; by applying the given function.
(define (rs-map fun sound)
  (rs-map/idx (lambda (s i) (fun s)) sound))

;; given a function from sample and index to sample and an rsound,
;; produce a new rsound where every sample is modified 
;; by applying the given function
(define (rs-map/idx fun sound)
  (define (left i) (fun (rs-ith/left sound i) i))
  (define (right i) (fun (rs-ith/right sound i) i))
  (parameterize ([default-sample-rate (rsound-sample-rate sound)])
  (signals->rsound (rs-frames sound)
                   left
                   right)))

;; rsound-scale : number rsound -> rsound
(define (scale scalar rsound)
  (rs-map (lambda (x) (* x scalar)) rsound))


;; clip : rsound nat nat -> rsound
;; extract a chunk of an rsound, beginning at frame 'start'
;; and ending before frame 'end'. 
(define (clip sound start finish)
  (unless (rsound? sound)
    (raise-type-error 'rsound-clip "rsound" 0 sound start finish))
  (unless (nonnegative-integer? start)
    (raise-type-error 'rsound-clip "non-negative integer" 1 sound start finish))
  (unless (nonnegative-integer? finish)
    (raise-type-error 'rsound-clip "non-negative integer" 2 sound start finish))
  (unless (and (<= 0 start finish (rs-frames sound)))
    (error 'clip 
           frames-out-of-range-msg
           start finish (rs-frames sound)))
  (match-define (rsound data old-start old-stop sample-rate) sound)
  (rsound data
          (+ old-start (i2e start)) 
          (+ old-start (i2e finish)) sample-rate))

(define frames-out-of-range-msg
  (string-append "must have 0 < start < end < frames.  "
                 "You provided start ~s and end ~s for a sound with ~s frames."))

;; given a factor and a sound, resample the sound (using simple rounding)
;; to obtain a new one. Using e.g. factor of 2 will make the sound one
;; octave higher and half as long.
(define (resample factor sound)
  (define (left i) (rs-ith/left sound 
                                (inexact->exact (floor (* factor i)))))
  (define (right i) (rs-ith/right sound 
                                  (inexact->exact (floor (* factor i)))))
  (parameterize ([default-sample-rate 
                   (rsound-sample-rate sound)])
    (signals->rsound (inexact->exact
                      (floor (/ (rs-frames sound) factor)))
                     left
                     right)))


;; produce a new rsound by multiplying each sample in the
;; first by each sample in the second. The length and sample
;; rate are determined by the first, and nonexistent samples
;; in the second are taken to be zeros.
(define (rs-mult a b)
  (define len1 (rs-frames a))
  (define len2 (rs-frames b))
  (define new-snd
    (parameterize ([default-sample-rate 
                     (rsound-sample-rate a)])
    (silence len1)))
  (for ([i (in-range (min len1 len2))])
    (set-rs-ith/left/s16! new-snd
                          i
                          (inexact->exact
                           (floor
                            (* (rs-ith/left/s16 a i)
                               (rs-ith/left b i)))))
    (set-rs-ith/right/s16! new-snd
                           i
                           (inexact->exact
                            (floor
                             (* (rs-ith/right/s16 a i)
                                (rs-ith/right b i))))))
  new-snd)


;; given a length and a function, build the corresponding flvector
(define (build-flvector len fun)
  (let ([newvec (make-flvector len)])
    (for ([i (in-range len)])
      (flvector-set! newvec i (exact->inexact (fun i))))
    newvec))

;; build a wavetable for a periodic function
(define (build-wavetable fun)
  (build-flvector wavetable-build-sample-rate
                  (fun 1 wavetable-build-sample-rate)))

;; this is independent, but it should be nice and high to get 
;; good wavetables
(define wavetable-build-sample-rate 44100)

;; given a wavetable, make a wavetable lookup function
;; how much slower would it be with interpolation?
(define ((make-table-based-wavefun vec) pitch sample-rate)
  (define relative-pitch (* pitch (/ wavetable-build-sample-rate sample-rate)))
  (define skip-rate (inexact->exact (round relative-pitch)))
  (lambda (i)
    (flvector-ref vec (modulo (* i skip-rate) wavetable-build-sample-rate))))


;; given a raw function, produce a table-based version of it
;; (nat nat -> (nat -> fl)) -> (nat nat -> (nat -> fl))
(define (make-checked-wave-fun raw-wave-fun)
  (let* ([table (build-wavetable raw-wave-fun)]
         [table-based-fun (make-table-based-wavefun table)]
         [table-sr (default-sample-rate)])
    (lambda (pitch)
      (when (= 0 pitch)
        (raise-type-error 'wave-fun "nonzero number" 0 pitch))
      (cond [(and (integer? pitch)
                  (= (default-sample-rate) table-sr))
             (table-based-fun (inexact->exact pitch) (default-sample-rate))]
            [else
             (raw-wave-fun pitch (default-sample-rate))]))))

;; SYNTHESIS OF SINE WAVES

;; raw-sine-wave : number number -> signal
;; given a pitch and a sample rate, produce a sine wave signal
(define (raw-sine-wave pitch sample-rate)
  (define tpisrp (* 2 pi (/ 1 sample-rate) pitch))
  (lambda (i) (sin (* tpisrp i))))

(define sine-wave (make-checked-wave-fun raw-sine-wave))

;; SYNTHESIS OF THREE-PARTIAL SINE

(define (raw-harm3-wave pitch sample-rate)
  (let ([scalar1 (* twopi pitch)]
        [scalar2 (* twopi 2 pitch)]
        [scalar3 (* twopi 3 pitch)])
    (lambda (i)
      (let ([t (/ i sample-rate)])
        (+ (sin (* scalar1 t))
           (* 0.5 (sin (* scalar2 t)))
           (* 0.25 (sin (* scalar3 t))))))))

(define harm3-wave (make-checked-wave-fun raw-harm3-wave))

;; SYNTHESIS OF TRIANGULAR WAVES:


(define (raw-sawtooth-wave pitch sample-rate)
  (let ([scalar (exact->inexact (* 2 (* pitch (/ 1 sample-rate))))])
    (lambda (i)
      (let* ([unwrapped (+ 1.0 (* (exact->inexact i) scalar))]
             [scaled (/ unwrapped 2.0)])
        (- (* 2.0 (- scaled (floor scaled))) 1.0)))))

(define sawtooth-wave (make-checked-wave-fun raw-sawtooth-wave))


;; a memoized 20-term sawtooth
(define sawtooth-terms 20)
(define (raw-sawtooth-approx-wave pitch sample-rate)
  (let ([scalar (exact->inexact (* twopi (* pitch (/ 1 sample-rate))))])
    (lambda (i) 
      (for/fold ([sum 0.0])
        ([t (in-range 1 sawtooth-terms)])
        (+ sum (* (expt -1 t) (/ 1 (* twopi t)) (sin (* i scalar t))))))))

(define approx-sawtooth-wave (make-checked-wave-fun raw-sawtooth-approx-wave))



;; square waves

(define (raw-square-wave pitch sample-rate)
  (let* ([period (* sample-rate (/ 1 pitch))])
    (lambda (i)
      (let* ([scaled (/ i period)]
             [frac (- scaled (floor scaled))])
        (cond [(< frac 0.5) 1.0]
              [else -1.0])))))

(define square-wave (make-checked-wave-fun raw-square-wave))

;;fader : frames -> signal
(define (fader fade-frames)
  (let ([p (expt 0.001 (/ 1 fade-frames))])
  (lambda (i)
    (expt p i))))

;; frisellinator : frames -> signal
(define (frisellinator intro-frames)
  (lambda (i)
    (cond [(< intro-frames i) 1.0]
          [else (* 0.5 (- 1.0 (cos (* pi (/ i intro-frames)))))])))

;; dc-signal : number -> signal
(define (dc-signal volume)
  (lambda (i) volume))

(define (signal-*s lof)
  (lambda (i) (apply * (map (lambda (x) (x i)) lof))))

(define (signal-* a b) (signal-*s (list a b)))

(define (signal-+s lof)
  (lambda (i) (apply + (map (lambda (x) (x i)) lof))))

;; given a number and a signal, scale the number by the signal
;; this can be done using signal-* and dc-signal, but this
;; turns out to be a lot faster
(define (sig-scale volume signal)
  (lambda (i) (* volume (signal i))))

;; convert a wavefun into a tone-maker; basically just keep a hash table
;; of previously generated sounds.
(define (wavefun->tone-maker wavefun)
  (let ([tone-table (make-hash)])
    (lambda (pitch volume frames)
      (define sample-rate (default-sample-rate))
      (define key (list pitch volume sample-rate))
      (define (compute-and-store)
        (define snd (mono-signal->rsound frames 
                             (wavefun pitch volume sample-rate)))
        (hash-set! tone-table key snd)
        snd)
      (match (hash-ref tone-table key #f)
        ;; 
        [#f (compute-and-store)]
        [(and s (struct rsound (data start stop sample-rate)))
         (let ()
           (define stored-frames (rs-frames s))
           (cond [(= frames stored-frames) s]
                 [(< frames stored-frames) (clip s 0 frames)]
                 [else (compute-and-store)]))]))))

;; OPTIMIZATION: don't generate everything, stop when the wave comes out "even"
;; on a frame. WARNING! Assumes that the sound is periodic. Don't use
;; this for sounds that aren't periodic on the given frequency.

;; NB: cache depends on volume, too; for some applications, caching a single
;; full-volume copy of the waveform could be a big win. It depends on how 
;; many different volumes you use.
(define (wavefun->tone-maker/periodic wavefun)
  (let ([tone-table (make-hash)])
    (lambda (pitch volume frames)
      (define sample-rate (default-sample-rate))
      (define key (list pitch volume sample-rate))
      (define (compute-and-store)
        (define num-cycles (cycles-to-use pitch sample-rate))
        (define generated-frames (round (* num-cycles (/ sample-rate pitch))))
        (log-debug (format "generated ~s frames" generated-frames))
        (define core (mono-signal->rsound generated-frames
                                          (wavefun pitch volume sample-rate)))
        (define snd (tile-to-len core frames))
        (when (< generated-frames too-long-to-cache)
          (hash-set! tone-table key snd))
        snd)
      (match (hash-ref tone-table key #f)
        ;; 
        [#f (compute-and-store)]
        [(and s (struct rsound (data start stop sample-rate)))
         (let ()
           (define stored-frames (rs-frames s))
           (cond [(= frames stored-frames) s]
                 [(< frames stored-frames) (clip s 0 frames)]
                 [else (compute-and-store)]))]))))

(define too-long-to-cache (* 44100 10))

;; we want to re-use the wavefun->tone-maker for the fader. It doesn't
;; have the right parameters, so we just re-purpose them.

(define (fader-as-wavefun fade-frames dc1 dc2)
  (let ([p (expt 0.001 (/ 1 fade-frames))])
    (lambda (i)
      (expt p i))))

(define fader-proxy (wavefun->tone-maker fader-as-wavefun))

(define (fader-snd fade-frames frames)
  (fader-proxy fade-frames #f frames))




;; generate a sound containing repeated copies of the sound out to the
;; given number of frames
(define (tile-to-len snd frames)
  (define copies (/ frames (rs-frames snd)))
  (define integral-copies (floor copies))
  (define leftover-frames (- frames (* (rs-frames snd) integral-copies)))
  (rs-append*
   (append (for/list ([i (in-range integral-copies)]) snd)
           (list (clip snd 0 leftover-frames)))))

(define make-harm3tone/unfaded
  (wavefun->tone-maker/periodic
   (lambda (pitch volume sample-rate)
     (sig-scale volume
                (harm3-wave pitch)))))

;; a memoized harm3 tone
(define (make-harm3tone pitch volume frames)
  (rs-mult (fader-snd 88200 frames)
           (make-harm3tone/unfaded pitch volume frames)))

;; make a monaural pitch with the given number of frames
(define make-tone
  (wavefun->tone-maker/periodic 
   (lambda (pitch volume sample-rate)
     (sig-scale volume (sine-wave pitch)))))

(define make-squaretone
  (wavefun->tone-maker/periodic
   (lambda (pitch volume sample-rate)
     (sig-scale volume (square-wave pitch)))))

(define (make-square-fade-tone pitch volume frames)
  (rs-mult (fader-snd 88200 frames)
           (make-squaretone pitch volume frames)))

(define make-zugtone
  (wavefun->tone-maker
   (lambda (pitch volume sample-rate)
     (sig-scale volume
                (signal-*s (list (frisellinator 8820) #;(fader 88200) (approx-sawtooth-wave pitch)))))))

(define make-sawtooth-tone
  (wavefun->tone-maker/periodic
   (lambda (pitch volume sample-rate)
     (sig-scale volume (sawtooth-wave pitch)))))

(define (make-sawtooth-fade-tone pitch volume frames)
  (rs-mult (fader-snd 88200 frames)
           (make-sawtooth-tone pitch volume frames)))

(define (make-pulse-tone duty-cycle)
  (when (not (< 0.0 duty-cycle 1.0))
    (raise-type-error 'make-pulse-tone
                      "number between 0 and 1"
                      0
                      duty-cycle))
  (wavefun->tone-maker/periodic
   (lambda (pitch volume sample-rate)
     (define wavelength (/ sample-rate pitch))
     (define on-samples (round (* duty-cycle wavelength)))
     (define total-samples (round wavelength))
     (define up volume)
     (define down (- up))
     (lambda (i)
       (cond [(< (modulo i total-samples) on-samples) up]
             [else down])))))

(define (make-ding pitch)
  (define sample-rate (default-sample-rate))
  (mono-signal->rsound sample-rate
                       (signal-*s (list (sine-wave pitch)
                                        (dc-signal 0.35)
                                        (fader sample-rate)))))

;; sounds like a ding...
(define ding (make-ding 600))

(define (split-in-4 s)
  (let ([len (floor (/ (rs-frames s) 4))])
    (apply values (for/list ([i (in-range 4)])
                    (clip s (* i len) (* (+ 1 i) len))))))

;; play a sound 'n' times
(define (times n s)
  (rs-append* (build-list n (lambda (x) s))))




;; put vectors together into an rsound at the default sample-rate. Ignores
;; the complex component entirely.
(define (vectors->rsound leftvec rightvec)
  (define sample-rate (default-sample-rate))
  (unless (equal? (vector-length leftvec) (vector-length rightvec))
    (error 'vectors->rsound "expected vectors of equal length, given vectors of lengths ~v and ~v." 
           (vector-length leftvec) (vector-length rightvec)))
  (let* ([len (vector-length leftvec)]
         [datamax (for/fold ((max-abs 0.0))
                    ((x (in-vector leftvec))
                     [y (in-vector rightvec)])
                    (max (abs (real-part x)) 
                         (abs (real-part y))
                         max-abs))]
         [newvec (make-s16vector (* 2 len))]
         [scaling (/ s16max datamax)])
    (for ([i (in-range len)])
      (s16vector-set! newvec (* 2 i)        (inexact->exact (round (* scaling (real-part (vector-ref leftvec i))))))
      (s16vector-set! newvec (add1 (* 2 i)) (inexact->exact (round (* scaling (real-part (vector-ref rightvec i)))))))
    (rsound newvec 0 len sample-rate)))


;; ADSR envelope (actually more of an ADS envelope)
(define (adsr frames attack-len attack-height decay-len decay-height)
  (let* ([t1 attack-len]
         [t2 (+ t1 decay-len)]
         [t3 frames])
  (lambda (i)
    (cond [(< i t1) (weighted (/ i attack-len) 0 attack-height)]
          [(< i t2) (weighted (/ (- i t1) decay-len) attack-height decay-height)]
          [(< i t3) decay-height]
          [else 0]))))

;; (1-s)*a + s*b
(define (weighted s a b)
  (+ (* a (- 1 s)) (* b s)))

;; turn a function of multiple arguments into a signal. Basically,
;; just curry w.r.t. the first argument.
(define (signal f . args)
  (lambda (t) (apply f (cons t args))))

;; FFTs

;; binary-logn; a safe version of the one that appears in the fft code
(define (binary-logn n)
  (let ((binary-logn
         (let loop ((k 1)
                    (l 0))
           (if (>= k n)
               l
               (loop (* k 2) (+ l 1))))))
    (if (= n (arithmetic-shift 1 binary-logn))
        binary-logn
        #f)))

;; return the (complex) fft of the left channel
(define (rsound-fft/left rsound)
  (channel-fft (lambda (i) (rs-ith/left/s16 rsound i)) (rs-frames rsound)))

;; return the (complex) fft of the right channel
(define (rsound-fft/right rsound)
  (channel-fft (lambda (i) (rs-ith/right/s16 rsound i)) (rs-frames rsound)))

;; the common left-right abstraction
(define (channel-fft accessor len)
  (let* ([v (build-vector len 
                          (lambda (i) 
                            (/ (exact->inexact (accessor i)) s16max)))])
    (if (binary-logn len)
        (fft-complex-radix2-forward v)
        (fft-complex-forward v)) 
    v))

;; make the sound as lound as possible without distortion
(define (rsound-max-volume rsound)
  (let* ([scalar (fl/ 1.0 (exact->inexact (rs-largest-sample rsound)))])
    (signals->rsound (rs-frames rsound)
                     (rsound-sample-rate rsound)
                     (lambda (i) (fl* scalar (exact->inexact (rs-ith/left/s16 rsound i))))
                     (lambda (i) (fl* scalar (exact->inexact (rs-ith/right/s16 rsound i)))))))


;; midi-note-num->pitch : number -> number
;; produces the pitch that corresponds to a midi note number
(define (midi-note-num->pitch note-num)
  (unless (real? note-num)
    (raise-type-error 'midi-note-num->pitch "real" 0 note-num))
  (* 440 (expt 2 (/ (- note-num 69) 12))))


;; rsound->signal/either : (rsound number -> number) -> rsound -> signal
;; an abstraction over left/right channels for the following two functions.
;; it has to appear before them, unfortunately.
(define ((rsound->signal/either ith-fun) rsound)
  (unless (rsound? rsound)
    (raise-type-error 'rsound->signal "rsound" 0 rsound))
  (let ([len (rs-frames rsound)])
    (lambda (t)
      (cond [(< t len) (ith-fun rsound t)]
            [else 0.0]))))

;; rsound->signal/left : rsound -> signal
;; produce the signal that corresponds to the rsound's left channel, followed by silence.
(define rsound->signal/left (rsound->signal/either rs-ith/left))

;; rsound->signal/right : rsound -> signal
;; produce the signal that corresponds to the rsound's right channel, followed by silence.
(define rsound->signal/right (rsound->signal/either rs-ith/right))

;; thresh : number number -> number
;; clip to a threshold
(define (thresh threshold n)
  (let ([abs-thresh (abs threshold)])
    (max (- abs-thresh) (min abs-thresh n))))

;; thresh/signal : number signal -> signal
;; clip to a threshold (lifted to signals)
(define (thresh/signal threshold signal)
  (let* ([abs-thresh (abs threshold)]
         [neg-abs-thresh (- abs-thresh)])
    (lambda (t)
      (max neg-abs-thresh (min abs-thresh (signal t))))))

;; scale : number signal -> signal
;; scale the signal by the given number
(define (signal-scale volume signal)
  (lambda (t)
    (* volume (signal t))))

;; clip&volume : number signal -> signal
;; clip the given signal to 1.0, then multiply by the volume.
(define (clip&volume volume signal)
  (signal-scale volume (thresh/signal 1.0 signal)))


;; overlay a list of sounds on top of each other
(define (overlay* los)
  (assemble (map (lambda (s) (list s 0)) los)))

;; overlay two sounds on top of each other
(define (overlay sound1 sound2)
  (assemble (list (list sound1 0)
                  (list sound2 0))))

;; a wrapper to simplify specifying sounds.
;; e.g.: (mono 30 t (sin (* t 300 twopi 1/44100)))
(define-syntax (mono stx)
  (syntax-parse stx
    [(_ frames:expr timevar:id body:expr ...)
     #'(mono-signal->rsound frames 
                            (lambda (timevar) body ...))]))


;; noise
(define (noise duration)
  (define samples (* duration channels))
  (define vec (make-s16vector samples))
  (for ([i (in-range samples)])
    (s16vector-set! vec i (- (random (* 2 s16max)) s16max)))
  (rsound vec 0 duration (default-sample-rate)))

;; rearrange
(define (rearrange frames fun orig)
  (define samples (* frames channels))
  (define vec (make-s16vector samples))
  (for ([i (in-range frames)])
    (define source (fun i))
    (s16vector-set! vec (* channels i) 
                    (rs-ith/left/s16 orig source))
    (s16vector-set! vec (add1 (* channels i)) 
                    (rs-ith/right/s16 orig source)))
  (rsound vec 0 frames (default-sample-rate)))

(define i2e inexact->exact)



;; this stuff is a bit of a mess... the frames don't need to be passed around, the 
;; whole thing is just a bit wordy.

(define (rs-largest-sample sound)
  (buffer-largest-sample/range (rsound-data sound) (rsound-start sound) (rsound-stop sound)
                               (rs-frames sound)))

(define (rs-largest-frame/range/left sound min-frame max-frame)
  (buffer-largest-sample/range/left (rsound-data sound) (rs-frames sound) min-frame max-frame))

(define (rs-largest-frame/range/right sound min-frame max-frame)
  (buffer-largest-sample/range/right (rsound-data sound) (rs-frames sound) min-frame max-frame))

(define (buffer-largest-sample/range buffer start stop frames)
  (buffer-largest-sample/range/helper buffer (* channels start) 
                                      (* channels stop) 1))

;; what's the largest sample from min to max-1 ?


;; left-channel only
(define (buffer-largest-sample/range/left buffer frames min-frame max-frame)
  (frame-range-checks frames min-frame max-frame)
  (buffer-largest-sample/range/helper buffer
                                      (* channels min-frame)
                                      (* channels max-frame)
                                      2))

;; right channel only
(define (buffer-largest-sample/range/right buffer frames min-frame max-frame)
  (frame-range-checks frames min-frame max-frame)
  (buffer-largest-sample/range/helper buffer
                                      (add1 (* channels min-frame))
                                      (add1 (* channels max-frame))
                                      2))

;; sample-based, for internal use only:
(define (buffer-largest-sample/range/helper buffer min-sample max-sample increment)
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