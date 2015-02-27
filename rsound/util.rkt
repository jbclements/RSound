#lang racket/base

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
         "common.rkt"
         math/base
         math/array
         "integral-cycles.rkt"
         "wavetable.rkt"
         "network.rkt"
         "paste-util.rkt"
         racket/flonum
         ffi/vector
         (only-in racket/math pi)
         (only-in racket/match match-define match)
         (for-syntax racket/base syntax/parse)
         racket/unsafe/ops
         "define-argcheck.rkt"
         lang/prim)

(provide-higher-order-primitive rs-map (mapping-fn _))
(provide-higher-order-primitive rs-map/idx (mapping-fn _))
(provide-higher-order-primitive rearrange (_ mapping-fn _))
(provide rs-scale
         resample
         resample/interp
         resample-to-rate
         clip
         rs-mult
         twopi
         sine-wave
         sawtooth-wave
         approx-sawtooth-wave
         square-wave
         harm3-wave
         pulse-wave
         wavetable-osc/l
         noise
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
         signal-*
         signal-+
         thresh/signal
         mono
         indexed-signal
         
         signal-scale
         clip&volume
         rsound->signal/left
         rsound->signal/right
         rs-largest-sample
         ;; rsound makers
         make-tone
         make-pulse-tone
         wavefun->tone-maker
         ding
         make-ding
         split-in-4
         times
         rs-overlay*
         rs-overlay
         vectors->rsound
         tile-to-len
         fader-snd
         
         rsound-fft/left
         rsound-fft/right
         rsound-maximize-volume
         midi-note-num->pitch
         pitch->midi-note-num
         
         andplay
         )


;; don't allow resampling to higher than this sample-rate:
(define MAX-FRAME-RATE 100000)
(define FRAME-RATE-DESCRIPTION (format "number in the range 0<n<=~a"
                                       MAX-FRAME-RATE))

(define twopi (* 2 pi))

;; given a function from numbers to numbers and an rsound, 
;; produce a new rsound where every sample is modified 
;; by applying the given function.
(define (rs-map fun sound)
  (rs-generate (rs-frames sound)
               (lambda (i) 
                 (fun (rs-ith/left sound i)))
               (lambda (i) 
                 (fun (rs-ith/right sound i)))
               (rsound-sample-rate sound)))

;; given (a function from sample and index to sample) and an rsound,
;; produce a new rsound where every sample is modified 
;; by applying the given function
(define (rs-map/idx fun sound)
  (define samp (silence (rs-frames sound)))
  (for ([i (rs-frames sound)])
    (set-rs-ith/left! samp i (fun (rs-ith/left sound i) i))
    (set-rs-ith/right! samp i (fun (rs-ith/right sound i) i)))
  samp)

;; given a number of frames and a left and right generator and a sample
;; rate, produce a sound.
(define/argcheck (rs-generate [frames nonnegative-integer?
                                      "non-negative integer"]
                              [left-fun procedure? "procedure"]
                              [right-fun procedure? "procedure"]
                              [sample-rate frame-rate? FRAME-RATE-DESCRIPTION])
  (define vec (make-s16vector (* CHANNELS frames)))
  (for ([i frames])
    (unsafe-s16vector-set! vec (* CHANNELS i) (real->s16 (left-fun i)))
    (unsafe-s16vector-set! vec (add1 (* CHANNELS i)) (real->s16 (right-fun i))))
  (vec->rsound vec sample-rate))

;; rsound-scale : number rsound -> rsound
;; this uses C subroutines. It will behave badly when
;; clipping occurs (not crashy, but noisy)
(define/argcheck (rs-scale [scalar real? "real number"]
                           [rsound rsound? "rsound"])
  (define inexact-scalar (exact->inexact scalar))
  (define samp (silence (rs-frames rsound)))
  (rs-copy-mult-add! (s16vector->cpointer (rsound-data samp)) 0 
                     rsound 0
                     (rs-frames rsound) (rs-frames samp)
                     inexact-scalar)
  samp)


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

;; given a factor and a sound, resample the sound (using simple rounding,
;; no interpolation)
;; to obtain a new one. Using e.g. factor of 2 will make the sound one
;; octave higher and half as long.
(define/argcheck (resample [factor positive-real? "positive real number"]
                           [sound rsound? "rsound"])
  (define new-sound-len 
    (inexact->exact (floor (/ (rs-frames sound) factor))))
  (unless (< 0 new-sound-len)
    (raise-argument-error 'resample "sound long enough to be shortened by the given factor"
                          1 factor sound))
  (define left 
    (indexed-signal
     (lambda (i)
       (rs-ith/left sound 
                    (inexact->exact (floor (* factor i)))))))
  (define right
    (indexed-signal
     (lambda (i) (rs-ith/right sound 
                               (inexact->exact (floor (* factor i)))))))
  (parameterize ([default-sample-rate (rsound-sample-rate sound)])
    (signals->rsound new-sound-len left right)))

;; given a factor and a sound, resample the sound (using linear interpolation)
;; to obtain a new one. Using e.g. factor of 2 will make the sound one
;; octave higher and half as long.
(define/argcheck (resample/interp [factor positive-real? "positive real number"]
                                  [sound rsound? "rsound"])
  (define new-sound-len
    (inexact->exact (floor (/ (rs-frames sound) factor))))
  (unless (< 0 new-sound-len)
    (raise-argument-error 'resample "sound long enough to be shortened by the given factor"
                          1 factor sound))
  (define (the-sig extractor)
    (lambda (i)
      (define virtual-source-sample (min (sub1 (rs-frames sound))
                                         (* factor i)))
      (define lower-index (inexact->exact (floor virtual-source-sample)))
      (define fractional-part (- virtual-source-sample lower-index))
      (cond [(= fractional-part 0) (extractor sound lower-index)]
            [else (+ (* (- 1.0 fractional-part) (extractor sound lower-index))
                     (* fractional-part (extractor sound (add1 lower-index))))])))
  (define left 
    (indexed-signal (the-sig rs-ith/left)))
  (define right
    (indexed-signal (the-sig rs-ith/right)))
  (parameterize ([default-sample-rate 
                   (rsound-sample-rate sound)])
    (signals->rsound new-sound-len
                     left
                     right)))


;; resample-to-rate : like resample/interp, but produces a sound with a 
;; given sample rate.
;; That is, it resamples but also resets the sample rate, so the result should
;; sound the same, just be at a different sample rate.
(define/argcheck (resample-to-rate [frame-rate frame-rate?
                                               FRAME-RATE-DESCRIPTION]
                                   [sound rsound? "rsound"])
  (define old-rate (rsound-sample-rate sound))
  (define factor (/ old-rate frame-rate))
  (define resampled
    (resample/interp factor sound))
  (vec->rsound (rsound-data resampled) frame-rate))

;; produce a new rsound by multiplying each sample in the
;; first by each sample in the second. The length and sample
;; rate are determined by the first, and nonexistent samples
;; in the second are taken to be zeros.
(define/argcheck (rs-mult [a rsound? "rsound"]
                          [b rsound? "rsound"])
  (define len1 (rs-frames a))
  (define len2 (rs-frames b))
  (define new-snd
    (parameterize ([default-sample-rate 
                     (rsound-sample-rate a)])
    (silence len1)))
  (for ([i (in-range (min len1 len2))])
    (set-rs-ith/left/s16! new-snd
                          i
                          (real->s16
                           (* (rs-ith/left a i)
                             (rs-ith/left b i))))
    (set-rs-ith/right/s16! new-snd
                           i
                           (real->s16
                            (* (rs-ith/right a i)
                               (rs-ith/right b i)))))
  new-snd)



;; given a raw function, produce a table-based version of it
;; (nat nat -> signal) -> (nat nat -> signal)
(define (make-checked-wave-fun raw-wave-fun)
  ;; assume that the given raw-wave-fun operates at the current sr:
  (define (raw-wave-fun/sr pitch sample-rate)
    (raw-wave-fun pitch))
  (let* ([table (build-wavetable raw-wave-fun/sr)]
         [table-based-fun (make-table-based-wavefun table)])
    (lambda (pitch)
      (when (= 0 pitch)
        (raise-type-error 'wave-fun "nonzero number" 0 pitch))
      (cond [(integer? pitch)
             (table-based-fun (inexact->exact pitch) (default-sample-rate))]
            [else
             (raw-wave-fun pitch)]))))


;; SIGNAL FUNCTIONS

;; these will assume a fixed sample rate, to speed computation:

(define SR (exact->inexact (default-sample-rate)))
(define SRINV (/ 1.0 SR))
(define TPSRINV (* 2.0 pi SRINV))

;; dc-signal : number -> signal
(define (dc-signal volume)
  (lambda () volume))

;; indexed-signal : (idx -> sample) -> signal
(define (indexed-signal fun)
  (network ()
           [idx <= (simple-ctr 0 1)]
           [out = (fun idx)]))

;; SYNTHESIS OF SINE WAVES

(define sine-wave
  (network (pitch)
    [_ = (unless (number? pitch)
           (raise-argument-error 'sine-wave "number" 0 pitch))]
    [angle = (prev added 0.0)]
    [added = (angle-add angle (* pitch TPSRINV))]
    [output = (sin angle)]))

;; SYNTHESIS OF THREE-PARTIAL SINE

(define harm3-wave
  (network (pitch)
    [_ = (unless (number? pitch)
           (raise-argument-error 'harm3-wave "number" 0 pitch))]
    (ctr <= (simple-ctr 0 SRINV))
    (out = (+ (sin (* twopi pitch ctr))
              (* 0.5 (sin (* twopi 2.0 pitch ctr)))
              (* 0.25 (sin (* twopi 3.0 pitch ctr)))))))


;; SYNTHESIS OF SAWTOOTH WAVES:

(define sawtooth-wave
  (let ()
    (define (increment p pitch)
      (define next (+ p (* 2.0 (* pitch SRINV))))
      (cond [(< next 1.0) next]
            [else (- next 2.0)]))
    (network (pitch)
      [_ = (unless (number? pitch)
             (raise-argument-error 'sawtooth-wave "number" 0 pitch))]
      [b   = (prev a 0.0)]
      [a   = (increment b pitch)]
      [out = b])))

;; a memoized 20-term sawtooth; it'll be slow if you don't hit the 
;; wavetable.
(define sawtooth-terms 20)
(define (raw-sawtooth-approx-wave pitch)
  (let ([scalar (exact->inexact (* twopi (* pitch SRINV)))])
    (indexed-signal
     (lambda (i) 
      (for/fold ([sum 0.0])
        ([t (in-range 1 sawtooth-terms)])
        (+ sum (* (expt -1 t) (/ 1 (* twopi t)) (sin (* i scalar t)))))))))

(define approx-sawtooth-wave (make-checked-wave-fun 
                              raw-sawtooth-approx-wave))

;; pulse waves. Pulse waves switch between 1.0 and 0.0, to 
;; make them more useful as gates.
;; NB: right now, this starts past zero...
;; also, the angle goes 
(define pulse-wave
  (network (duty-cycle pitch)
    [_ = (unless (number? pitch)
           (raise-argument-error 'pulse-wave "number" 0 pitch))]
    [angle = (prev added 0.0)]
    [added = (angle-add/unit angle (* pitch SRINV))]
    [out <= pulse-wave-thresh angle duty-cycle]))

;; add args, subtract 2pi if greater than 2pi. assumes all values
;; are positive, and that the sum can't be greater than 4pi
(define (angle-add a b)
  (define sum (+ a b))
  (cond [(<= twopi sum) (- sum twopi)]
        [else sum]))

;; add args, subtract 2pi if greater than 2pi. assumes all values
;; are positive, and that the sum can't be greater than 4pi
(define (angle-add/unit a b)
  (define sum (+ a b))
  (cond [(<= 1.0 sum) (- sum 1.0)]
        [else sum]))

;; return 1 if angle is less than duty-cycle, 0 otherwise.
(define (pulse-wave-thresh angle duty-cycle)
  (cond [(< angle duty-cycle) 1.0] [else 0.0]))

;; square waves

(define square-wave 
  (network (pitch)
    [_ = (unless (number? pitch)
           (raise-argument-error 'square-wave "number" 0 pitch))]
    [out <= pulse-wave 0.5 pitch]))

;; create a looping signal from the left channel of an rsound
;; no interpolation, just flooring.
(define/argcheck (wavetable-osc/l [wt (lambda (wt)
                                        (and (rsound? wt)
                                             (= (rs-frames wt) SR)))
                                      (format "sound with ~s frames" SR)])
  (define (fetch s) (rs-ith/left wt (inexact->exact (floor s))))
  (network (vol pitch)
           [idx <= (loop-ctr/variable SR) pitch]
           [out = (* vol (fetch idx))]))

;;fader : frames -> signal
(define (fader fade-frames)
  (let ([p (expt 0.001 (/ 1 fade-frames))])
    (network ()
             (out = (* p (prev out 1.0))))))

;; frisellinator : frames -> signal
(define (frisellinator intro-frames)
  (indexed-signal
  (lambda (i)
    (cond [(< intro-frames i) 1.0]
          [else (* 0.5 (- 1.0 (cos (* pi (/ i intro-frames)))))]))))

;; multiply two signals together
(define (signal-* a b)
  (network ()
           [a-out <= a]
           [b-out <= b]
           (out = (* a-out b-out))))

;; add two signals together
(define (signal-+ a b)
  (network ()
           (a-out <= a)
           (b-out <= b)
           (out = (+ a-out b-out))))

;; given a number and a signal, scale the number by the signal
;; this can be done using signal-* and dc-signal, but this
;; turns out to be a lot faster
(define (sig-scale volume signal)
  (network ()
           (s <= signal)
           (out = (* volume s))))

;; convert a wavefun into a tone-maker; basically just keep a hash table
;; of previously generated sounds.

(define (wavefun->tone-maker wavefun)
  (let ([tone-table (make-hash)])
    (lambda (pitch volume frames)
      (define sample-rate (default-sample-rate))
      (define key (list pitch volume sample-rate))
      (define (compute-and-store)
        (define snd (signal->rsound frames 
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
      (unless (= SR (default-sample-rate))
        (error 'wavefun->tone-maker
               "this function only works when (default-sample-rate) = ~s"
               SR))
      (define sample-rate (default-sample-rate))
      (define key (list pitch volume sample-rate))
      (define (compute-and-store)
        (define num-cycles (cycles-to-use pitch sample-rate))
        (define generated-frames (round (* num-cycles (/ sample-rate pitch))))
        (log-debug (format "generated ~s frames" generated-frames))
        (define core 
          (parameterize ([default-sample-rate SR])
            (signal->rsound generated-frames
                            (wavefun pitch volume))))
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

(define too-long-to-cache (* (default-sample-rate) 10))

;; we want to re-use the wavefun->tone-maker for the fader. It doesn't
;; have the right parameters, so we just re-purpose them.

(define (fader-as-wavefun fade-frames dc1 dc2)
  (fader fade-frames))

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

(define make-tone
  (wavefun->tone-maker/periodic
   (lambda (pitch volume)
     (sig-scale volume (fixed-inputs sine-wave pitch)))))

(define make-harm3tone/unfaded
  (wavefun->tone-maker/periodic
   (lambda (pitch volume)
     (sig-scale volume
                (harm3-wave pitch)))))

;; a memoized harm3 tone
(define (make-harm3tone pitch volume frames)
  (rs-mult (fader-snd 88200 frames)
           (make-harm3tone/unfaded pitch volume frames)))

(define/argcheck (make-pulse-tone [duty-cycle (lambda (cycle)
                                                (< 0.0 cycle 1.0))
                                              "number between 0 and 1"])
  (wavefun->tone-maker/periodic
   (lambda (pitch volume)
     (define wavelength (/ (default-sample-rate) pitch))
     (define on-samples (inexact->exact (round (* duty-cycle wavelength))))
     (define total-samples (inexact->exact (round wavelength)))
     (define up volume)
     (define down (- up))
     (define (hi-lo idx)
       (cond [(< idx on-samples) up]
             [else down]))
     (network ()
              (idx <= (loop-ctr total-samples 1))
              (out = (hi-lo idx))))))

(define (make-ding pitch)
  (parameterize ([default-sample-rate SR])
    (signal->rsound SR
                    (signal-*s (list (fixed-inputs sine-wave pitch)
                                     (dc-signal 0.35)
                                     (fader SR))))))

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
    (error 'vectors->rsound 
           "expected vectors of equal length, given vectors of lengths ~v and ~v." 
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
      (s16vector-set! newvec (* 2 i)        
                      (inexact->exact 
                       (round (* scaling 
                                 (real-part (vector-ref leftvec i))))))
      (s16vector-set! newvec (add1 (* 2 i))
                      (inexact->exact 
                       (round (* scaling 
                                 (real-part (vector-ref rightvec i)))))))
    (rsound newvec 0 len sample-rate)))



;; FFTs

;; return the (complex) fft of the left channel
(define (rsound-fft/left rsound)
  (channel-fft (lambda (i) (rs-ith/left/s16 rsound i)) (rs-frames rsound)))

;; return the (complex) fft of the right channel
(define (rsound-fft/right rsound)
  (channel-fft (lambda (i) (rs-ith/right/s16 rsound i)) (rs-frames rsound)))

;; the common left-right abstraction
(define/argcheck (channel-fft [accessor procedure? "procedure"]
                              [len (lambda (len) 
                                     (and (integer? len) (power-of-two? len)))
                                   "integer power of two"])
  (let* ([v (build-array (vector len) 
                         (lambda (i) 
                           (/ (exact->inexact (accessor (vector-ref i 0))) s16max)))])
    (array-fft v)))

;; make the sound as lound as possible without distortion
(define (rsound-maximize-volume rsound)
  (let* ([scalar (fl/ 1.0 (exact->inexact (rs-largest-sample rsound)))])
    (signals->rsound 
     (rs-frames rsound)
     (rsound-sample-rate rsound)
     (lambda (i) (fl* scalar (exact->inexact (rs-ith/left/s16 rsound i))))
     (lambda (i) (fl* scalar (exact->inexact (rs-ith/right/s16 rsound i)))))))


;; midi-note-num->pitch : number -> number
;; produces the pitch that corresponds to a midi note number
(define (midi-note-num->pitch note-num)
  (unless (real? note-num)
    (raise-type-error 'midi-note-num->pitch "real" 0 note-num))
  (* 440 (expt 2 (/ (- note-num 69) 12))))

;; pitch->midi-note-num : number -> number
;; produces the midi note number that corresponds to a pitch
(define (pitch->midi-note-num pitch)
  (unless (real? pitch)
    (raise-type-error 'pitch->midi-note-num "real" 0 pitch))
  (+ 69 (* 12 (/ (log (/ pitch 440)) (log 2)))))

;; rsound->signal/either : (rsound number -> number) -> rsound -> signal
;; an abstraction over left/right channels for the following two functions.
;; it has to appear before them, unfortunately.
(define ((rsound->signal/either ith-fun) rsound)
  (unless (rsound? rsound)
    (raise-type-error 'rsound->signal "rsound" 0 rsound))
  (let ([len (rs-frames rsound)])
    (indexed-signal
     (lambda (t)
       (cond [(< t len) (ith-fun rsound t)]
             [else 0.0])))))

;; rsound->signal/left : rsound -> signal
;; produce the signal that corresponds to the rsound's 
;; left channel, followed by silence.
(define rsound->signal/left (rsound->signal/either rs-ith/left))

;; rsound->signal/right : rsound -> signal
;; produce the signal that corresponds to the rsound's 
;; right channel, followed by silence.
(define rsound->signal/right (rsound->signal/either rs-ith/right))

;; thresh : number number -> number
;; clip to a threshold
(define (thresh threshold n)
  (let ([abs-thresh (abs threshold)])
    (max (- abs-thresh) (min abs-thresh n))))

;; thresh/signal : number signal -> signal
;; clip to a threshold (lifted to signals)
(define (thresh/signal threshold signal)
  (define abs-threshold (abs threshold))
  (define neg-threshold (- abs-threshold))
  (define (limit-fun s)
    (cond [(< s neg-threshold) neg-threshold]
          [(< s abs-threshold) s]
          [else abs-threshold]))
  ;; this pattern is looking very abstract-able...
  (network ()
           (base <= signal)
           (out = (limit-fun base))))

;; scale : number signal -> signal
;; scale the signal by the given number
(define (signal-scale volume signal)
  (define mult (lambda (s) (* volume s)))
  (network ()
           (s <= signal)
           (out = (mult s))))

;; clip&volume : number signal -> signal
;; clip the given signal to 1.0, then multiply by the volume.
(define (clip&volume volume signal)
  (signal-scale volume (thresh/signal 1.0 signal)))


;; overlay a list of sounds on top of each other
(define (rs-overlay* los)
  (assemble (map (lambda (s) (list s 0)) los)))

;; overlay two sounds on top of each other
(define (rs-overlay sound1 sound2)
  (assemble (list (list sound1 0)
                  (list sound2 0))))

;; a wrapper to simplify specifying sounds.
;; e.g.: (mono 30 t (sin (* t 300 twopi 1/44100)))
(define-syntax (mono stx)
  (syntax-parse stx
    [(_ frames:expr timevar:id body:expr ...)
     #'(signal->rsound frames 
                       (indexed-signal
                        (lambda (timevar) body ...)))]))


;; noise
(define (noise duration)
  (define samples (* duration CHANNELS))
  (define vec (make-s16vector samples))
  (for ([i (in-range samples)])
    (s16vector-set! vec i (- (random (* 2 s16max)) s16max)))
  (rsound vec 0 duration (default-sample-rate)))

;; rearrange
(define (rearrange frames fun orig)
  (define samples (* frames CHANNELS))
  (define vec (make-s16vector samples))
  (for ([i (in-range frames)])
    (define source (fun i))
    (s16vector-set! vec (* CHANNELS i) 
                    (rs-ith/left/s16 orig source))
    (s16vector-set! vec (add1 (* CHANNELS i)) 
                    (rs-ith/right/s16 orig source)))
  (rsound vec 0 frames (default-sample-rate)))

(define i2e inexact->exact)

;; a silly little helper to hide effectfulness
(define/argcheck (andplay [snd rsound? "rsound"] 
                          [val any? "value"])
  (play snd)
  val)

(define (any? v) #t)

;; this stuff is a bit of a mess... the frames don't need to be passed around, the 
;; whole thing is just a bit wordy.

(define (rs-largest-sample sound)
  (buffer-largest-sample/range (rsound-data sound) 
                               (rsound-start sound) (rsound-stop sound)
                               (rs-frames sound)))

;; these two aren't actually 'provide'd at all?

(define (rs-largest-frame/range/left sound min-frame max-frame)
  (buffer-largest-sample/range/left (rsound-data sound) 
                                    (rs-frames sound) min-frame max-frame))

(define (rs-largest-frame/range/right sound min-frame max-frame)
  (buffer-largest-sample/range/right (rsound-data sound) 
                                     (rs-frames sound) min-frame max-frame))

(define (buffer-largest-sample/range buffer start stop frames)
  (buffer-largest-sample/range/helper buffer (* CHANNELS start) 
                                      (* CHANNELS stop) 1))

;; what's the largest sample from min to max-1 ?


;; left-channel only
(define (buffer-largest-sample/range/left buffer frames min-frame max-frame)
  (frame-range-checks frames min-frame max-frame)
  (buffer-largest-sample/range/helper buffer
                                      (* CHANNELS min-frame)
                                      (* CHANNELS max-frame)
                                      2))

;; right channel only
(define (buffer-largest-sample/range/right buffer frames min-frame max-frame)
  (frame-range-checks frames min-frame max-frame)
  (buffer-largest-sample/range/helper buffer
                                      (add1 (* CHANNELS min-frame))
                                      (add1 (* CHANNELS max-frame))
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
    (error 'frame-range-checks 
           "range limits ~v and ~v not in range 0 - ~v" 
           min-frame max-frame frames))
  (when (not (< min-frame max-frame))
    (error 'frame-range-checks 
           "range limits ~v and ~v not in order and separated by at least 1" 
           min-frame max-frame)))

(define (positive-real? x)
  (and (real? x) (< 0 x)))


(define (frame-rate? fr)
  (and (real? fr)
       (< 0 fr)
       (<= fr MAX-FRAME-RATE)))