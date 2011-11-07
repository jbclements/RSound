#lang racket

;; untested:
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
         racket/flonum
         ffi/vector
         (for-syntax syntax/parse))

(define twopi (* 2 pi))


(provide rs-map
         rs-map/idx
         scale
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
         fir-filter
         iir-filter
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
         up-to-power-of-two
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
  (signals->rsound (rsound-frames sound)
                   left
                   right)))

;; rsound-scale : number rsound -> rsound
(define (scale scalar rsound)
  (rs-map (lambda (x) (* x scalar)) rsound))


;; produce a new rsound by multiplying each sample in the
;; first by each sample in the second. The length and sample
;; rate are determined by the first, and nonexistent samples
;; in the second are taken to be zeros.
(define (rs-mult a b)
  (define len1 (rsound-frames a))
  (define len2 (rsound-frames b))
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
         [table-based-fun (make-table-based-wavefun table)])
    (lambda (pitch sample-rate)
      (when (= 0 pitch)
        (raise-type-error 'wave-fun "nonzero number" 0 pitch sample-rate))
      (when (= 0 sample-rate)
        (raise-type-error 'wave-fun "nonzero number" 1 pitch sample-rate))
      (cond [(and (= sample-rate (default-sample-rate))
                  (integer? pitch))         
             (table-based-fun (inexact->exact pitch) sample-rate)]
            [else
             (raw-wave-fun pitch sample-rate)]))))

;; SYNTHESIS OF SINE WAVES

;; raw-sine-wave : number number -> signal
;; given a pitch and a sample rate, produce a sine wave signal
(define (raw-sine-wave pitch sample-rate)
  (let ([scalar (* twopi pitch)])
    (lambda (i)
      (let ([t (/ i sample-rate)])
        (sin (* scalar t))))))

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

;; convert a wavefun into a tone-maker; basically just keep a hash table
;; of previously generated sounds.
(define (wavefun->tone-maker wavefun)
  (let ([tone-table (make-hash)])
    (lambda (pitch volume frames)
      (define sample-rate (default-sample-rate))
      (define key (list pitch volume sample-rate))
      (define (compute-and-store)
        (let ([s (mono-signal->rsound frames 
                                      (wavefun pitch volume sample-rate))])
          (hash-set! tone-table key s)
          s))
      (match (hash-ref tone-table key #f)
        ;; 
        [#f (compute-and-store)]
        [(and s (struct rsound (data start stop sample-rate)))
         (let ()
           (define stored-frames (rsound-frames s))
           (cond [(= frames stored-frames) s]
                 [(< frames stored-frames) (clip s 0 frames)]
                 [else (compute-and-store)]))]))))

;; a memoized harm3 tone
(define make-harm3tone
  (wavefun->tone-maker
   (lambda (pitch volume sample-rate)
     (signal-*s (list (fader 88200)
                      (dc-signal volume)
                      (harm3-wave pitch sample-rate))))))

;; make a monaural pitch with the given number of frames
(define make-tone
  (wavefun->tone-maker 
   (lambda (pitch volume sample-rate)
     (signal-*s (list (dc-signal volume) (sine-wave pitch sample-rate))))))

(define make-squaretone
  (wavefun->tone-maker
   (lambda (pitch volume sample-rate)
     (signal-*s (list (dc-signal volume) (square-wave pitch sample-rate))))))

(define make-square-fade-tone
  (wavefun->tone-maker
   (lambda (pitch volume sample-rate)
     (signal-*s (list (dc-signal volume) (fader 88200) (square-wave pitch sample-rate))))))

(define make-zugtone
  (wavefun->tone-maker
   (lambda (pitch volume sample-rate)
     (signal-*s (list (frisellinator 8820) #;(fader 88200) (dc-signal volume) (approx-sawtooth-wave pitch sample-rate))))))




(define make-sawtooth-tone
  (wavefun->tone-maker 
   (lambda (pitch volume sample-rate)
     (signal-*s (list (dc-signal volume)
                      (sawtooth-wave pitch sample-rate))))))

(define make-sawtooth-fade-tone
  (wavefun->tone-maker 
   (lambda (pitch volume sample-rate)
     (signal-*s (list (dc-signal volume)
                      (fader 88200)
                      (sawtooth-wave pitch sample-rate))))))

(define (make-pulse-tone duty-cycle)
  (when (not (< 0.0 duty-cycle 1.0))
    (raise-type-error 'make-pulse-tone
                      "number between 0 and 1"
                      0
                      duty-cycle))
  (wavefun->tone-maker
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
                       (signal-*s (list (sine-wave pitch sample-rate)
                                        (dc-signal 0.35)
                                        (fader sample-rate)))))

;; sounds like a ding...
(define ding (make-ding 600))

(define (split-in-4 s)
  (let ([len (floor (/ (rsound-frames s) 4))])
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
  (channel-fft (lambda (i) (rs-ith/left/s16 rsound i)) (rsound-frames rsound)))

;; return the (complex) fft of the right channel
(define (rsound-fft/right rsound)
  (channel-fft (lambda (i) (rs-ith/right/s16 rsound i)) (rsound-frames rsound)))

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
    (signals->rsound (rsound-frames rsound)
                     (rsound-sample-rate rsound)
                     (lambda (i) (fl* scalar (exact->inexact (rs-ith/left/s16 rsound i))))
                     (lambda (i) (fl* scalar (exact->inexact (rs-ith/right/s16 rsound i)))))))


;; midi-note-num->pitch : number -> number
;; produces the pitch that corresponds to a midi note number
(define (midi-note-num->pitch note-num)
  (unless (exact-integer? note-num)
    (raise-type-error 'midi-note-num->pitch "exact integer" 0 note-num))
  (* 440 (expt 2 (/ (- note-num 69) 12))))


;; rsound->signal/either : (rsound number -> number) -> rsound -> signal
;; an abstraction over left/right channels for the following two functions.
;; it has to appear before them, unfortunately.
(define ((rsound->signal/either ith-fun) rsound)
  (unless (rsound? rsound)
    (raise-type-error 'rsound->signal "rsound" 0 rsound))
  (let ([len (rsound-frames rsound)])
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

;; FIR filters

;; fir-filter : (listof (list/c delay amplitude)) -> signal -> signal
;; filter the input signal using the delay values and amplitudes given for an FIR filter
(define (fir-filter params)
  (match params
    [`((,delays ,amplitudes) ...)
     (unless (andmap (lambda (d) (and (exact-integer? d) (<= 0 d))) delays)
       (raise-type-error 'fir-filter "exact integer delays greater than zero" 0 params))
     (unless (andmap real? amplitudes)
       (raise-type-error 'fir-filter "real number amplitudes" 0 params))
     (lambda (signal)
       ;; enough to hold delayed and current, rounded up to next power of 2:
       (let* ([max-delay (up-to-power-of-two (+ 1 (apply max delays)))]
              ;; set up buffer to delay the signal
              [delay-buf (make-vector max-delay 0.0)]
              [next-idx 0]
              ;; ugh... we must be called sequentially:
              [last-t -1])
         (lambda (t)
           (unless (= t (add1 last-t))
             (error 'fir-filter "called with t=~s, expecting t=~s. Sorry about that limitation." 
                    t
                    (add1 last-t)))
           (let ([this-val (signal t)])
             (begin
               (vector-set! delay-buf next-idx this-val)
               (define result
                 (for/fold ([sum 0])
                   ([d (in-list delays)]
                    [a (in-list amplitudes)])
                   (+ sum (* a (vector-ref delay-buf (modulo (- next-idx d) max-delay))))))
               (set! last-t (add1 last-t))
               (set! next-idx (modulo (add1 next-idx) max-delay))
               result)))))]
    [other (raise-type-error 'fir-filter "(listof (list number number))" 0 params)]))

(define (up-to-power-of-two n)
  (inexact->exact (expt 2 (ceiling (/ (log (max n 1)) (log 2))))))

;; IIR filters

;; iir-filter : (listof (list/c delay amplitude)) -> signal -> signal
;; filter the input signal using the delay values and amplitudes given for an IIR filter
;; the only difference here is that we put the final result in the delay line, rather than
;; the input signal.
(define (iir-filter params)
  (match params
    [`((,delays ,amplitudes) ...)
     (unless (andmap (lambda (d) (and (exact-integer? d) (< 0 d))) delays)
       (raise-type-error 'iir-filter "exact integer delays greater than zero" 0 params))
     (unless (andmap real? amplitudes)
       (raise-type-error 'iir-filter "real number amplitudes" 0 params))
     (lambda (signal)
       ;; use a minimum vector length of 1:
       (let* ([max-delay (apply max (cons 1 delays))]
              ;; set up buffer to delay the signal
              [delay-buf (make-vector max-delay 0.0)]
              [next-idx 0]
              ;; ugh... we must be called sequentially:
              [last-t -1])
         (lambda (t)
           (unless (= t (add1 last-t))
             (error 'fir-filter "called with t=~s, expecting t=~s. Sorry about that limitation." 
                    t
                    (add1 last-t)))
           (let* ([this-val (signal t)]
                  [new-val (for/fold ([sum this-val])
                                     ([d (in-list delays)]
                                      [a (in-list amplitudes)])
                                     (+ sum (* a (vector-ref delay-buf (modulo (- next-idx d) max-delay)))))])
             (begin0
               new-val
               (vector-set! delay-buf next-idx new-val)
               (set! last-t (add1 last-t))
               (set! next-idx (modulo (add1 next-idx) max-delay)))))))]
    [other (raise-type-error 'iir-filter "(listof (list number number))" 0 params)]))

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