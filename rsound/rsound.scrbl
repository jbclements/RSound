#lang scribble/doc

@(require scribble/manual)

@title{@bold{RSound}: An Adequate Sound Engine for Racket}

@author[(author+email "John Clements" "clements@racket-lang.org")]

@(require (for-label racket
                     rsound
                     rsound/frequency-response))

@defmodule[rsound]{This collection provides a means to represent, read,
write, play, and manipulate sounds. It depends on the @racket[clements/portaudio] 
package to provide bindings to the cross-platform `PortAudio' library which appears
to run on Linux, Mac, and Windows.

It represents all sounds internally as stereo 16-bit PCM, with all the attendant
advantages (speed, mostly) and disadvantages (clipping).

Does it work on your machine? Try this example:
@racketblock[
 (require (planet clements/rsound))
  
 (play ding)
 ]

}

A note about volume: be careful not to damage your hearing, please. To take a simple example,
the @racket[sine-wave] function generates a sine wave with amplitude 1.0.  That translates into
the @emph{loudest possible sine wave} that can be represented. So please set your volume low, 
and be careful with the headphones. Maybe there should be a parameter that controls the clipping 
volume. Hmm.

@section{A NOTE ABOUT WINDOWS}

Windows is a bit of a pain for developers. If you're having trouble hearing sounds under 
windows (high latency, or "Invalid Device" errors), try running @racket[diagnose-sound-playing].

@defproc[(diagnose-sound-playing) void?]{
 Tries playing a short tone using all of the available APIs and several plausible sample rates.
 It tries to offer a helpful message, along with the test.}

@section{Sound Control}

These procedures start and stop playing sounds and loops.

@defproc[(play (rsound rsound?)) void?]{
 Plays an rsound. Plays concurrently with an already-playing sound, if there is one.}

@;{@defproc[(rsound-loop (rsound rsound?)) void?]{
 Plays an rsound repeatedly.  Continues looping until interrupted by 
 another sound command.}

@defproc[(change-loop (rsound rsound?)) void?]{
 When the current sound or loop finishes, starts looping this one instead.}
}
@defproc[(stop) void]{
 Stop all of the the currently playing sounds.}

@defthing[ding rsound?]{
 A one-second "ding" sound. Nice for testing whether sound playing is working.}

@section{Sound I/O}

These procedures read and write rsounds from/to disk.

The RSound library reads and writes WAV files only; this means fewer FFI dependencies
(the reading & writing is done in Racket), and works on all platforms. 

@defproc[(rs-read (path path-string?)) rsound?]{
 Reads a WAV file from the given path, returns it as an rsound.
 
 It currently
has lots of restrictions (it insists on 16-bit PCM encoding, for instance), but deals 
with a number of common bizarre conventions that certain WAV files have (PAD chunks,
extra blank bytes at the end of the fmt chunk, etc.), and tries to fail
relatively gracefully on files it can't handle.

Reading in a large sound can result in a very large value (~10 Megabytes per minute);
for larger sounds, consider reading in only a part of the file, using @racket[rs-read/clip].}

@defproc[(rs-read/clip (path path-string?) (start nonnegative-integer?) (finish nonnegative-integer?)) rsound?]{
 Reads a portion of a WAV file from a given path, starting at frame @racket[start]  and ending at frame @racket[finish].
                                                                    
 It currently
has lots of restrictions (it insists on 16-bit PCM encoding, for instance), but deals 
with a number of common bizarre conventions that certain WAV files have (PAD chunks,
extra blank bytes at the end of the fmt chunk, etc.), and tries to fail
relatively gracefully on files it can't handle.}

@defproc[(rs-read-frames (path path-string?)) nonnegative-integer?]{
 Returns the number of frames in the sound indicated by the path. It parses
 the header only, and is therefore much faster than reading in the whole sound.
 
 The file must be encoded as a WAV file readable with @racket[rsound-read].}

@defproc[(rs-read-sample-rate (path path-string?)) number?]{
 Returns the sample-rate of the sound indicated by the path. It parses
 the header only, and is therefore much faster than reading in the whole sound.
 
  The file must be encoded as a WAV file readable with @racket[rs-read].}


@defproc[(rs-write (rsound rsound?) (path path-string?)) void?]{
 Writes an rsound to a WAV file, using stereo 16-bit PCM encoding. It
 overwrites an existing file at the given path, if one exists.}

@section{Rsound Manipulation}

These procedures allow the creation, analysis, and manipulation of rsounds.

@defstruct[rsound ([data s16vector?] [start nonnegative-number?] [end nonnegative-number?] [sample-rate nonnegative-number?])]{
 Represents a sound; specifically, frames @racket[start] through @racket[end] of the given 16-bit stereo @racket[s16vector].}

@defproc[(rs-frames [sound rsound?]) nonnegative-integer?]{
 Returns the length of a sound, in frames.}

@defproc[(rs-equal? [sound1 rsound?] [sound2 rsound?]) boolean?]{
 Returns @racket[#true] when the two sounds are (extensionally) equal.
         
 This procedure is necessary because s16vectors don't natively support @racket[equal?].}

@defproc[(silence [frames nonnegative-integer?]) rsound?]{
 Returns an rsound of length @racket[frames] containing silence.  This procedure is relatively fast.}

@defproc[(rs-ith/left (rsound rsound?) (frame nonnegative-integer?)) nonnegative-integer?]{
 Returns the @racket[n]th sample from the left channel of the rsound, represented as a number in the range @racket[-1.0]
 to @racket[1.0].}

@defproc[(rs-ith/right (rsound rsound?) (frame nonnegative-integer?)) nonnegative-integer?]{
 Returns the @racket[n]th sample from the right channel of the rsound, represented as a number in the range @racket[-1.0]
 to @racket[1.0].}

@defproc[(clip (rsound rsound?) (start nonnegative-integer?) (finish nonnegative-integer?)) rsound?]{
 Returns a new rsound containing the frames in @racket[rsound] from the @racket[start]th to the @racket[finish]th - 1.
 This procedure copies the required portion of the sound.}

@defproc[(rs-append* (rsounds (listof rsound?))) rsound?]{
 Returns a new rsound containing the given @racket[rsounds], appended sequentially. This procedure is relatively
 fast. All of the given rsounds must have the same sample-rate.}

@defproc[(rs-overlay (rsound-1 rsound?) (rsound-2 rsound?)) rsound?]{
 Returns a new rsound containing the two sounds played simultaneously.  
 Note that unless both sounds have amplitudes less that 0.5, clipping
 or wrapping is likely.}

@defproc[(rs-overlay* (rsounds (listof rsound?))) rsound?]{
 Returns a new rsound containing all of the sounds played simultaneously.  
 Note that unless all of the sounds have low amplitudes, clipping
 or wrapping is likely.}

@defproc[(assemble (assembly-list (listof (list/c rsound? nonnegative-integer?)))) rsound?]{
 Returns a new rsound containing all of the given rsounds. Each sound begins at the frame number 
 indicated by its associated offset. The rsound will be exactly the length required to contain all of
 the given sounds.
 
 So, suppose we have two rsounds: one called 'a', of length 20000, and one called 'b', of length 10000.
 Evaluating
 
 @racketblock[
  (rs-overlay* (list (list a 5000)
                         (list b 0)
                         (list b 11000)))]
 
 ... would produce a sound of 21000 frames, where each instance of 'b' overlaps with the central
 instance of 'a'.
 
 }

@defproc[(rs-scale (scalar nonnegative-number?) (rsound rsound?)) rsound?]{
 Scale the given sound by multiplying all of its samples by the given scalar.}

@section{Signals and Networks}

For signal processing, RSound adopts a dataflow-like paradigm. Networks represent
interconnected signal-processing nodes, and produce streams of values. They can be
connected together using a number of primitives, including the @racket[network]
syntactic form. Networks that have no inputs are called @deftech{signals}.

Here's a trivial signal:

@racketblock[
(network ()
         [out 3])]

This is the signal that always produces 3.

Here's another one, that counts upward:

@racketblock[
(define counter/sig
  (network ()
           [counter (+ 1 (prev counter 0))]))]

The @racket[prev] form is special, and is used to refer to the prior value of the
signal component.

Note that since we're adding one immediately, this counter starts at 1.

Here's another example, that adds together two sine waves, at 34 Hz and 46 Hz, assuming 
a sample rate of 44.1KHz:

@racketblock[
(define sum-of-sines
  (network ()
           [a (sine-wave 34)]
           [b (sine-wave 46)]
           [out (+ a b)]))]

Several things to note:
@itemlist[@item{a network can have many clauses; each clause contains a name and a right-hand-side.}
           @item{a right-hand-side must be a constant, or an application, either of a primitive function or of a network.}
           @item{the last clause is used as the output, regardless of its name.}
           @item{clauses can produce multiple values; in this case, the name is replaced by a parenthesized list.}
          ]

In order to use a signal with @racket[signal-play], it should produce a real number in the range @racket[-1.0] to @racket[1.0].

Here's an example that uses one sine-wave (often called an "LFO") to control the pitch of another one:

@racketblock[
(define vibrato-tone
  (network ()
           [lfo (sine-wave 2)]
           [sin (sine-wave (+ 400 (* 50 lfo)))]
           [out (* 0.1 sin)]))
(signal-play vibrato-tone)
(sleep 5)
(stop)
]

There are many built-in signals. Note that these are documented as 
though they were procedures, but they're not; they can be used in
a procedure-like way in network clauses. Otherwise, they will behave
as opaque values; you can pass them to various signal functions, etc.

Also note that all of these assume a fixed sample rate of 44.1 KHz.

@defproc[#:kind "signal"
                (sine-wave [frequency nonnegative-number?]) real?]{
 Produces a signal representing a sine wave of the given
 frequency, at the default sample rate, of amplitude 1.0.}

@defproc[#:kind "signal"
                (sawtooth-wave [frequency nonnegative-number?]) real?]{
 Produces a signal representing a naive sawtooth wave of the given
 frequency, of amplitude 1.0. Note that since this is a simple -1.0 up to 1.0 sawtooth wave, it's got horrible 
 aliasing all over the spectrum.}

@defproc[#:kind "signal"
                (square-wave [frequency nonnegative-number?]) real?]{
 Produces a signal representing a naive square wave of the given
 frequency, of amplitude 1.0, at the default sample rate. It alternates
 between 1.0 and 0.0, which makes it more useful in, e.g., gating 
 applications.
 
 Also note that since this is a simple 1/-1 square wave, it's got horrible 
 aliasing all over the spectrum.}

@defproc[#:kind "signal"
                (pulse-wave [duty-cycle real?] [frequency nonnegative-number?]) real?]{
 Produces a signal representing a "pulse wave", with part of the signal at 1.0 and
 the rest of the signal at 0.0. The @racket[duty-cycle] determines the fraction of the
 cycle that is 1.0. So, for instance, when @racket[duty-cycle] is 0.5, the result is
 a square wave.
}

@defproc[#:kind "signal"
                (dc-signal [amplitude real?]) real?]{
 Produces a constant signal at @racket[amplitude]. Inaudible unless used to multiply by
 another signal.}

In order to listen to them, you can transform them into rsounds, or play them directly:

@defproc[(signal->rsound (frames nonnegative-integer?) (signal signal?)) rsound?]{
 Builds a sound of length @racket[frames] at the default sample-rate by calling 
 @racket[signal] with integers from 0 up to @racket[frames]-1. The result should be an inexact 
 number in the range @racket[-1.0] to @racket[1.0]. Values outside this range are clipped.
 Both channels are identical.
 
 Here's an example of using it:

 @racketblock[
(define sig1
  (network ()
           [a (sine-wave 560)]
           [out (* 0.1 a)]))

(define r (signal->rsound 44100 sig1))

(play r)]
}
@defproc[(signals->rsound (frames nonnegative-integer?) 
                             (left-sig signal?) (right-sig signal?)) rsound?]{
 Builds a stereo sound of length @racket[frames] by using
 @racket[left-sig] and @racket[right-sig] to generate the
 samples for the left and right channels.
 
 with integers from 0 up to @racket[frames]-1. The result should be an inexact 
 number in the range @racket[-1.0] to @racket[1.0]. Values outside this range are clipped.}
                                                                             
@defproc[(signal-play (signal signal?)) void?]{
 Plays a (single-channel) signal. Halt playback using @racket[(stop)].}
 

There are several functions that produce signals.

@defproc[(indexed-signal [time->amplitude procedure?]) signal?]{
  Given a mapping from frame to amplitude, return a signal. In prior versions of RSound, such
  a mapping was called a signal. This function converts those functions into new-style
  signals.
}

@defproc[(fader [fade-samples number?]) signal?]{
 Produces a signal that decays exponentially. After @racket[fade-samples], its value is @racket[0.001].
 Inaudible unless used to multiply by another signal.}



There are also a number of functions that combine existing signals, called "signal combinators":

@defproc[(signal-+s [signals (listof signal?)]) signal?]{
 Produces the signal that is the sum of the input signals.}

@defproc[(signal-*s [signals (listof signal?)]) signal?]{
 Produces the signal that is the product of the input signals.}

We can turn an rsound back into a signal, using rsound->signal:

@defproc[(rsound->signal/left [rsound rsound?]) signal?]{
 Produces the signal that corresponds to the rsound's left channel, followed by endless silence. Ah, endless silence.}


@defproc[(rsound->signal/right [rsound rsound?]) signal?]{
 Produces the signal that corresponds to the rsound's right channel, followed by endless silence. (The silence joke
 wouldn't be funny if I made it again.)}

@defproc[(thresh/signal [threshold real-number?] [signal signal?]) signal?]{
 Applies a threshold (see @racket[thresh], below) to a signal.}

@defproc[(clip&volume [volume real-number?] [signal signal?]) signal?]{
 Clips the @racket[signal] to a threshold of 1, then multiplies by the given @racket[volume].}

Where should these go?

@defproc[(thresh [threshold real-number?] [input real-number?]) real-number?]{
 Produces the number in the range @racket[(- threshold)] to @racket[threshold] that is 
 closest to @racket[input]. Put differently, it ``clips'' the input at the threshold.}

Finally, here's a predicate.  This could be a full-on contract, but I'm afraid of the 
overhead.

@defproc[(signal? [maybe-signal any/c]) boolean?]{
 Is the given value a signal? More precisely, is the given value a procedure whose
 arity includes 1?}

@section{Visualizing Rsounds}

@defmodule[rsound/draw]

@defproc[(rs-draw [rsound rsound?]
                      [#:title title string?]
                      [#:width width nonnegative-integer? 800]
                      [#:height height nonnegative-integer? 200])
         void?]{
 Displays a new window containing a visual representation of the sound as a waveform.}
               
@defproc[(rsound-fft-draw [rsound rsound?]
                          [#:zoom-freq zoom-freq nonnegative-real?]
                          [#:title title string?]
                          [#:width width nonnegative-integer? 800]
                          [#:height height nonnegative-integer? 200]) void?]{
 Draws an fft of the sound by breaking it into windows of 2048 samples and performing an
 FFT on each. Each fft is represented as a column of gray rectangles, where darker grays
 indicate more of the given frequency band.}
                                                                      
@defproc[(vector-pair-draw/magnitude [left (vectorof complex?)] [right (vectorof complex?)]
                                     [#:title title string?]
                                     [#:width width nonnegative-integer? 800]
                                     [#:height height nonnegative-integer? 200]) 
         void?]{
 Displays a new window containing a visual representation of the two vectors' magnitudes
 as a waveform. The lines connecting the dots are really somewhat inappropriate in the 
 frequency domain, but they aid visibility....}
               
@defproc[(vector-draw/real/imag [vec (vectorof complex?)]
                                [#:title title string?]
                                [#:width width nonnegative-integer? 800]
                                [#:height height nonnegative-integer? 200]) 
         void?]{
 Displays a new window containing a visual representation of the vector's real and imaginary
 parts as a waveform.}
               


@section{RSound Utilities}

@defproc[(make-harm3tone [frequency nonnegative-number?] [volume? nonnegative-number?] [frames nonnegative-integer?]
                         [sample-rate nonnegative-number?]) rsound?]{
 Produces an rsound containing a semi-percussive tone of the given frequency, frames, and volume.  The tone contains the first
 three harmonics of the specified frequency.  This function is memoized, so that subsequent calls with the same parameters 
 will return existing values, rather than recomputing them each time.}
                                                                    
@defproc[(make-tone (pitch nonnegative-number?) (volume nonnegative-number?) (duration nonnegative-exact-integer?)) rsound?]{
 given a pitch in Hz, a volume between 0.0 and 1.0, and a duration in frames, return the
 rsound consisting of a pure sine wave tone using the specified parameters.}

@defproc[(rsound-fft/left [rsound rsound?]) (vectorof complex?)]{
 Produces the complex-valued vector that represents the fourier transform of the rsound's left channel.
 Since the FFT takes time N*log(N) in the size of the input, running this on rsounds with more than a
 few thousand frames is probably going to be slow, unless the number of frames is a power of 2.}

@defproc[(rsound-fft/right [rsound rsound?]) (vectorof complex?)]{
 Produces the complex-valued vector that represents the fourier transform of the rsound's right channel.
 Since the FFT takes time N*log(N) in the size of the input, running this on rsounds with more than a
 few thousand frames is probably going to be slow, unless the number of frames is a power of 2}

@defproc[(midi-note-num->pitch [note-num nonnegative-integer?]) number?]{
 Returns the frequency (in Hz) that corresponds to a given midi note number. Here's the top-secret formula: 
 440*2^((n-69)/12).}

@defproc[(fir-filter [delay-lines (listof (list/c nonnegative-exact-integer? real-number?))]) procedure?]{
 Given a list of delay times (in frames) and amplitudes for each, produces a function that maps signals
 to new signals where each frame is the sum of the current signal frame and the multiplied versions of 
 the delayed @emph{input} signals (that's what makes it FIR).
 
 So, for instance,
 
 @racketblock[(fir-filter (list (list 13 0.4) (list 4 0.1)))]
 
 ...would produce a filter that added the current frame to 4/10 of the input frame 13 frames ago and 1/10 of
 the input frame 4 frames ago.}

@defproc[(iir-filter [delay-lines (listof (list/c nonnegative-exact-integer? real-number?))]) procedure?]{
 Given a list of delay times (in frames) and amplitudes for each, produces a function that maps signals
 to new signals where each frame is the sum of the current signal frame and the multiplied versions of 
 the delayed @emph{output} signals (that's what makes it IIR).
 
 So, for instance,
 
 @racketblock[(iir-filter (list (list 13 0.4) (list 4 0.1)))]
 
 ...would produce a filter that added the current frame to 4/10 of the output frame 13 frames ago and 1/10 of
 the output frame 4 frames ago.}

@section{Frequency Response}

@defmodule[rsound/frequency-response]{
 This module provides functions to allow the analysis of frequency response on filters specified
 either as transfer functions or as lists of poles and zeros. It assumes a sample rate of 44.1 Khz.

@defproc[(response-plot [poly procedure?] [dbrel real?] [min-freq real?] [max-freq real]) void?]{
 Plot the frequency response of a filter, given its transfer function (a function mapping reals to reals). 
 The @racket[dbrel] number
 indicates how many decibels up the "zero" line should be shifted. The graph starts at @racket[min-freq]
 Hz and goes up to @racket[max-freq] Hz.  Note that aliasing effects may affect the apparent height or
 depth of narrow spikes.
 
 Here's an example of calling this function on a 100-pole comb filter, showing the response from 10KHz
 to 11KHz:
 
 @racketblock[
 (response-plot (lambda (z)
                  (/ 1 (- 1 (* 0.95 (expt z -100)))))
                30 10000 11000)]}

@defproc[(poles&zeros->fun [poles (listof real?)] [zeros (listof real?)]) procedure?]{
 given a list of poles and zeros in the complex plane, generate the corresponding transfer
 function.
 
 Here's an example of calling this function as part of a call to response-plot, for a filter
 with three poles and two zeros, from 0 Hz up to the nyquist frequency, 22.05 KHz:
 
 @racketblock[
 (response-plot (poles&zeros->fun '(0.5 0.5+0.5i 0.5-0.5i) '(0+i 0-i))
                40 
                0
                22050)]
 }}

@section{Filtering}


@defmodule[rsound/filter]{
 This module provides a dynamic low-pass filter, among other things.
 
@defproc[(lpf/dynamic [control signal?] [input signal?]) signal?]{
 The control signal must produce real numbers in the range 0.01 to 3.0. A small
 number produces a low cutoff frequency. The input signal is the audio signal
 to be processed. For instance, here's a time-varying low-pass filtered sawtooth:
 
@racketblock[
 (define (control f) (+ 0.5 (* 0.2 (sin (* f 7.123792865282977e-05)))))
 (define (sawtooth f) (/ (modulo f 220) 220))

 (play (signal->rsound 88200 (lpf/dynamic control sawtooth)))]
 
 }
}

@section{Single-cycle sounds}

@defmodule[rsound/single-cycle]{
 This module provides support for generating tones from single-cycle waveforms.
 
 In particular, it comes with a library of 247 such waveforms, courtesy of 
 @link["http://www.adventurekid.se"]{Adventure Kid's website}. Used with
 permission. Thanks!
 
@defproc[(synth-note [family string?] [spec number-or-path?] [midi-note-number natural?] [duration natural?])
         rsound]{
 Given a family (currently either "main", "vgame", or "path"), a spec (a number in the first two cases),
 a midi note number and a duration in frames, produces an rsound. There's a (non-configurable) 
 envelope applied, too.
 
 Example, playing sound #49 from the vgame package for a half-second at middle C:
 
@racketblock[
 (synth-note "vgame" 49 60 22010)]
 }
 
@defproc[(synth-note/raw [family string?] [spec number-or-path?] [midi-note-number natural?] [duration natural?])
         rsound]{
 Same as above, but no envelope is applied.}
 
 
 }

@section{Stream-based Playing}

@defmodule[rsound/stream-play]{
 RSound now provides functions whereby all played sounds use a single stream.
 This has the advantage of lower latency and avoids problems on Windows, where
 opening a new stream for each sound causes errors.
 
 
@defproc[(play/s [sound rsound?]) void]{
 Plays a given sound.}

@defproc[(play/s/f [sound rsound?] [frame natural?]) void]{
 Plays a given sound at a given (stream-relative) frame.}

@defproc[(current-time/s) natural?]{
 Returns the current stream-relative frame.}}

@section{Sample Code}

An example of a signal that plays two lines, each with randomly changing 
square-wave tones. This one runs in the Intermediate student language:

@#reader scribble/comment-reader
(racketblock
(require (planet clements/rsound))
(require (planet clements/rsound/filter))

;; scrobble: number number number -> signal
;; return a signal that generates square-wave tones, changing
;; at the given interval into a new randomly-chosen frequency
;; between lo-f and hi-f
(define (scrobble change-interval lo-f hi-f)
  (local
    [(define freq-range (floor (- hi-f lo-f)))
     (define (maybe-change f l)
       (cond [(= l 0) (+ lo-f (random freq-range))]
             [else f]))]
    (network ()
             [looper ((loop-ctr change-interval 1))]
             [freq (maybe-change (prev freq 400) looper)]
             [a (square-wave freq)])))

(define my-signal
  (network ()
           [a ((scrobble 4000 200 600))]
           [b ((scrobble 40000 100 200))]
           [lpf-wave (sine-wave 0.1)]
           [c (lpf/dynamic (max 0.01 (abs (* 0.5 lpf-wave))) (+ a b))]
           [b (* c 0.1)]))

;; write 20 seconds to a file, if uncommented:
; (rs-write (signal->rsound (* 20 44100) my-signal) "/tmp/foo.wav")

;; play the signal
(signal-play my-signal)
)



@section{Reporting Bugs}

For Heaven's sake, report lots of bugs!
