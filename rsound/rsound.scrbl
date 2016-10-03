#lang scribble/manual

@title{RSound: A Sound Engine for Racket}

@author[(author+email "John Clements" "clements@racket-lang.org")]

@(require (for-label racket
                     "main.rkt"
                     "frequency-response.rkt"))

@defmodule[rsound]{This collection provides a means to represent, read,
write, play, and manipulate sounds. It depends on the @racket[portaudio] 
package to provide bindings to the cross-platform `PortAudio' library which appears
to run on Linux, Mac, and Windows.

It represents all sounds internally as stereo 16-bit PCM, with all the attendant
advantages (speed, mostly) and disadvantages (clipping).

Does it work on your machine? Try this example:
@racketblock[
 (require rsound)
  
 (play ding)
 ]

}

If it doesn't work on your machine, please try running @racket[(diagnose-sound-playing)], and 
tell me about it!

A note about volume: be careful not to damage your hearing, please. To take a simple example,
the @racket[sine-wave] function generates a sine wave with amplitude 1.0.  That translates into
the @emph{loudest possible sine wave} that can be represented. So please set your volume low, 
and be careful with the headphones. Maybe there should be a parameter that controls the clipping 
volume. Hmm.

@section{Sound Control}

These procedures start and stop playing sounds.

@defproc[(play (rsound rsound?)) void?]{
 Plays an rsound. Plays concurrently with an already-playing sound, if there is one. Returns 
 immediately (before the sound is played).}

@defproc[(stop) void]{
 Stop all of the the currently playing sounds.}

@defthing[ding rsound?]{
 A one-second "ding" sound. Nice for testing whether sound playing is working.}

@section{Stream-based Playing}

 RSound provides a "pstream" abstraction which falls conceptually in between
 @racket[play] and @racket[signal-play]. In particular, a @racket[pstream] encapsulates
 an ongoing signal, with primitives available to queue sounds for playback, to check
 the signal's "current time" (in frames), and to queue a callback to occur at a 
 particular time.
 
 This mechanism has two advantages over @racket[play]; first, it allows you to queue sounds 
 for a particular frame, avoiding hiccups in playback.  Second, it only uses a single
 portaudio stream, rather than the multiple portaudio streams that would occur in 
 multiple calls to @racket[play]
 
@defproc[(make-pstream (#:buffer-time buffer-time (or/c number? #f) #f)) pstream?]{
 Create a new pstream and start playing it. Initially, of course, it will be silent. Returns
 the pstream. If a @racket[buffer-time] argument is specified (in seconds), it overrides the default
 buffer time of about 50 ms. Use a long buffer-time when continuity is more important than 
 responsiveness (background music, etc).
}

@defproc[(pstream-queue [pstream pstream?] [rsound rsound?] [frames natural?]) pstream?]{
 Queue the given sound to be played at the time specified by @racket[frames]. If that frame
 is in the past, it will still play the appropriate remainder of the sound. Returns
 the pstream.
}

@defproc[(pstream-current-frame [pstream pstream?]) natural?]{
 Returns the current value of the stream's frame counter.
 }

@defproc[(pstream-play [pstream pstream?] [rsound rsound?]) pstream?]{
 Play the given sound on the given stream. Returns the pstream.
}

@defproc[(pstream-queue-callback [pstream pstream?] [callback procedure?] [frames natural?]) pstream?]{
 Queue the callback (a procedure of no arguments) to be called when the pstream's frame
 counter reaches @racket[frames]. If the counter is already larger than @racket[frames], calls
 it immediately.
 
 It's perhaps worth noting that the callbacks are triggered by semaphore posts, to avoid the possibility
 of a callback stalling playback.  This can mean that the callback is delayed by a few milliseconds.
 }

@defproc[(pstream-set-volume! [pstream pstream?] [volume real?]) pstream?]{
 Given a nonnegative real number, sets the pstream's volume to that number. A value of 0 indicates
 silence, a value of 1.0 indicates full volume. Returns the pstream.}

@defproc[(pstream-clear! [pstream pstream?]) void?]{
 Clear all rsounds from @racket[pstream]'s queue.  This will not stop any of @racket[pstream]'s rsounds which have already 
 begun playing.}

@section{Recording}

RSound now includes basic support for recording sounds.

@defproc[(record-sound (frames nat?)) rsound?]{
 Using the default input default device, record a (stereo) sound of length @racket[frames],
 using the default sample rate. Blocks until the sound is finished.
}

@section{File I/O}

These procedures read and write rsounds from/to disk.

The RSound library reads and writes WAV files only; this means fewer FFI dependencies
(the reading & writing is done in Racket), and works on all platforms. 

@defproc[(rs-read (path path-string?)) rsound?]{
 Reads a WAV file from the given path, returns it as an rsound.
 
 It currently
has lots of restrictions (it insists on 16-bit PCM encoding, for instance), but deals 
with a number of common bizarre conventions th-at certain WAV files have (PAD chunks,
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

@defproc[(rs-read-sample-rate (path path-string?)) positive-number?]{
 Returns the frame rate of the sound indicated by the path. It parses
 the header only, and is therefore much faster than reading in the whole sound.
 
  The file must be encoded as a WAV file readable with @racket[rs-read].}


@defproc[(rs-write (rsound rsound?) (path path-string?)) void?]{
 Writes an rsound to a WAV file, using stereo 16-bit PCM encoding. It
 overwrites an existing file at the given path, if one exists.}

@section{Rsound Manipulation}

These procedures allow the creation, analysis, and manipulation of rsounds.

@defstruct[rsound ([data s16vector?] [start nonnegative-number?] [end nonnegative-number?] [frame-rate nonnegative-number?])]{
 Represents a sound; specifically, frames @racket[start] through @racket[end] of the given 16-bit stereo @racket[s16vector].}

@defthing[FRAME-RATE nonnegative-integer?]{
 the basic default frame rate.
 
 Note for people not using the beginning student language: this constant is provided because
 the @racket[default-sample-rate] parameter isn't usable in beginning student language.
}

@defparam[default-sample-rate frame-rate positive-real? #:value 48000]{
 A parameter that defines the default frame rate for construction of new sounds.
 
 Note that the terms sample rate and frame rate are used interchangeably. The
 term "frame rate" is arguably more correct, because one second of stereo
 sound at a frame rate of 44100 actually has 88200 samples---44100 for the left
 channel, and 44100 for the right channel.  Despite this, the term "sample rate"
 is generally used to refer to the frame rate in audio applications.
}

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

@defproc[(rs-append (rsound-1 rsound?) (rsound-2 rsound?)) rsound?]{
 Returns a new rsound containing the given two rsounds appended sequentially.
 Both of the given rsounds must have the same frame rate.
 }

@defproc[(rs-append* (rsounds (listof rsound?))) rsound?]{
 Returns a new rsound containing the given @racket[rsounds], appended sequentially. This procedure is relatively
 fast. All of the given rsounds must have the same frame rate.}

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
 (assemble (list (list a 5000)
                 (list b 0)
                 (list b 11000)))]
 
 ... would produce a sound of 21000 frames, where each instance of 'b' overlaps with the central
 instance of 'a'.
 
 }

@defproc[(rs-scale (scalar real?) (rsound rsound?)) rsound?]{
 Scale the given sound by multiplying all of its samples by the given scalar.}

@defproc[(rs-mult (a rsound?) (b rsound?)) rsound?]{
 Produce a new sound by pointwise multiplication of sounds @racket[a] and @racket[b].}

@defproc[(rearrange (length frames?) (mapping-fun procedure?) (rsound rsound?)) rsound?]{
 Returns a new sound with samples drawn from the original according to the @racket[mapping-fun].
 Specifically, a sound of length @racket[length] is constructed by calling @racket[mapping-fun]
 once for each sample with the frame number, and using the resulting number to select a frame
 from the input sound @racket[rsound].}

@defproc[(resample [factor positive-real?] [sound rsound?]) rsound]{
 Returns a new sound that is resampled by the given factor. So, for 
 instance, calling @racket[(resample 2 ding)] will produce a sound that 
 is half as long and one octave higher. The sample rate of the new sound
 is the same as the old one.
 
 Samples are chosen using rounding; there is no interpolation done.}

@defproc[(resample/interp [factor positive-real?] [sound rsound?]) rsound]{
 Similar to @racket[resample], except that it performs linear interpolation.
 The resulting sound should sound better, but the function takes slightly
 longer.
 
 My tests of 2014-09-22 suggest that interpolating takes about twice as long.
 In command-line racket, this amounts to a jump from 1.7% CPU usage to 3.0%
 CPU usage.}

@defproc[(resample-to-rate [frame-rate frame-rate?] [sound rsound?]) rsound]{
 Similar to @racket[resample/interp], except that it accepts a new desired
 frame rate rather than a factor, and produces a sound whose frame rate is
 the given one.
 
 Put differently, the sounds that result from @racket[(resample/interp 2.0 ding)]
 and @racket[(resample-to-rate 24000 ding)] should contain exactly the same set
 of samples, but the first will have a frame rate of 48000, and the second a frame 
 rate of 24000.
}

@defproc[(build-sound [frames frames?] [generator procedure?]) rsound?]{
 Given a number of frames and a procedure, produce a sound.

 More specifically, the samples in the sound are generated by calling the procedure
 with each frame number in the range @racket[[0 .. frames-1]]. The procedure
 must return real numbers in the range @racket[(-1 .. 1)]]. The left and right
 channels will be identical.

 Here's an example that generates a simple sine-wave (you could also use @racket[make-tone]
 for this).

 @racketblock[
 (define VOLUME 0.1)
 (define FREQUENCY 430)

 (define (sine-tone f)
   (* VOLUME (sin (* 2 pi FREQUENCY (/ f FRAME-RATE)))))

 (build-sound (* 2 FRAME-RATE) sine-tone)
 ]}

@defproc[(vec->rsound [s16vec s16vector?] [frame-rate frame-rate?]) rsound?]{
 Construct an rsound from an @racket[s16vector] containing interleaved 16-bit
 samples, using the given frame rate.}

@include-section["signals.scrbl"]

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

@defproc[(rsound/left-1-fft-draw [rsound rsound?]
                                 [#:title title string?]
                                 [#:width width 800]
                                 [#:height height 200]) void?]{
 Draws an fft of the left channel of the sound, and displays it as a pair of
 graphs, one for magnitude and one for phase. The whole sound is processed
 as a single fft frame, so it must be of a length that is a power of 2, and
 using a sound of more than 16384 frames could be slow.}
                                                                      
@defproc[(vector-pair-draw/magnitude [left (fcarrayof complex?)] [right (vectorof complex?)]
                                     [#:title title string?]
                                     [#:width width nonnegative-integer? 800]
                                     [#:height height nonnegative-integer? 200]) 
         void?]{
 Displays a new window containing a visual representation of the two vectors' magnitudes
 as a waveform. The lines connecting the dots are really somewhat inappropriate in the 
 frequency domain, but they aid visibility....}
               
@defproc[(vector-draw/real/imag [vec (fcarrayof complex?)]
                                [#:title title string?]
                                [#:width width nonnegative-integer? 800]
                                [#:height height nonnegative-integer? 200]) 
         void?]{
 Displays a new window containing a visual representation of the vector's real and imaginary
 parts as a waveform.}
               


@section{RSound Utilities}

@defproc[(make-harm3tone [frequency nonnegative-number?] [volume? nonnegative-number?] [frames nonnegative-integer?]
                         [frame-rate nonnegative-number?]) rsound?]{
 Produces an rsound containing a semi-percussive tone of the given frequency, frames, and volume.  The tone contains the first
 three harmonics of the specified frequency.  This function is memoized, so that subsequent calls with the same parameters 
 will return existing values, rather than recomputing them each time.}
                                                                    
@defproc[(make-tone (pitch nonnegative-number?) (volume nonnegative-number?) (duration nonnegative-exact-integer?)) rsound?]{
 given a pitch in Hz, a volume between 0.0 and 1.0, and a duration in frames, return the
 rsound consisting of a pure sine wave tone using the specified parameters.}

@defproc[(rs-fft/left [rsound rsound?]) (fcarrayof complex?)]{
 Produces the complex-valued vector that represents the fourier transform of the rsound's left channel.
 The sound's length must be a power of two. 
 
 The FFT takes time N*log(N) in the size of the input, so runtimes will be super-linear, but on a modern
 machine even FFTs of 32K points take on the order of 100msec.
@(history #:changed "20151120.0" @elem{Was named @racket[rsound-fft/left].})}

@defproc[(rs-fft/right [rsound rsound?]) (fcarrayof complex?)]{
 Produces the complex-valued vector that represents the fourier transform of the rsound's right channel.
 The sound's length must be a power of two. 
 
 The FFT takes time N*log(N) in the size of the input, so runtimes will be super-linear, but on a modern
 machine even FFTs of 32K points take on the order of 100msec.
@(history #:changed "20151120.0" @elem{Was named @racket[rsound-fft/right].})}

@defproc[(midi-note-num->pitch [note-num nonnegative-integer?]) number?]{
 Returns the frequency (in Hz) that corresponds to a given midi note number. Here's the top-secret formula: 
 440*2^((n-69)/12).}

@defproc[(pitch->midi-note-num [pitch nonnegative-real?]) nonnegative-real?]{
 Returns the midi note number that corresponds to a given frequency (in Hz). Inverse of the previous function.
}

@defproc[(andplay [snd rsound?] [val any/c]) any/c]{
 plays the given sound and returns the value.}


@section{Piano Tones}

@defmodule[rsound/piano-tones]{
This module provides functions that generate resampled piano tones.  The source for these are recordings
made by the University of Iowa's Electronic Music Studios, which graciously makes their data available
for re-use. In particular, rsound uses samples of c3, c4, c5, and c6, and resamples as needed.

@defproc[(piano-tone [midi-note-num number?]) rsound?]{
  Returns an rsound containing a recording of a piano note at the given midi note number,
  resampled from a nearby one. The notes are fairly long--about three seconds--though the
  exact length naturally depends on the length of the recorded note and the resampling factor.

  This function is memoized, to speed loading.
}

}

@section{Envelopes}

@defmodule[rsound/envelope]{

@defproc[(sine-window [len frames?] [fade-in frames]) rsound?]{
 Generates an rsound of length @racket[len + fade-in] representing
 a window with sine-shaped fade-in and fade-out. The fade-in and fade-out
 periods are identical, and have half-overlap with the center section. Er...
 that could be worded better.}

@defproc[(hann-window [len frames?]) rsound?]{
 Generates an rsound of length @racket[len] representing
 a window with sine-shaped fade-in and fade-out, with no flat part in
 the middle. This is often called the "Hann" window, and is useful
 when applying the FFT. Strictly speaking, this one differs 
 (indistinguishably, I believe) from the
 one specified by Wikipedia in that it hits zero at the length, not
 at length-1.}
}

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
 RSound provides a dynamic low-pass filter, among other things.



@defproc[(fir-filter [delay-lines (listof (list/c nonnegative-exact-integer? real-number?))]) network?]{
 Given a list of delay times (in frames) and amplitudes for each, produces a function that maps signals
 to new signals where each frame is the sum of the current signal frame and the multiplied versions of 
 the delayed @emph{input} signals (that's what makes it FIR).
 
 So, for instance,
 
 @racketblock[(fir-filter (list (list 13 0.4) (list 4 0.1)))]
 
 ...would produce a filter that added the current frame to 4/10 of the input frame 13 frames ago and 1/10 of
 the input frame 4 frames ago.
 }

@defproc[(iir-filter [delay-lines (listof (list/c nonnegative-exact-integer? real-number?))]) network?]{
 Given a list of delay times (in frames) and amplitudes for each, produces a function that maps signals
 to new signals where each frame is the sum of the current signal frame and the multiplied versions of 
 the delayed @emph{output} signals (that's what makes it IIR).
 
 So, for instance,
 
 @racketblock[(iir-filter (list (list 13 0.4) (list 4 0.1)))]
 
 ...would produce a filter that added the current frame to 4/10 of the output 
 frame 13 frames ago and 1/10 of the output frame 4 frames ago.
 
 Here's an example of code that uses a simple comb 
 filter to extract a 3-second buzzing sound at 
 300 Hz from noise:
 
@#reader scribble/comment-reader
(racketblock
(define comb-level 0.99)

(play
 (signal->rsound
  (* 48000 3)
  (network ()
           [r = (random)]    ;; a random number from 0 to 1
           [r2 = (* r 0.1)]  ;; scaled to make it less noisy
                             ;; apply the comb filter:
           [o2 <= (iir-filter (list (list 147 comb-level))) r]
                             ;; compensate for the filter's gain:
           [out = (* (- 1 comb-level) o2)])))
)
}

@defproc[#:kind "signal" (lpf/dynamic [control number?] [input number?]) signal?]{
 The control signal must produce real numbers in the range 0.01 to 3.0. A small
 number produces a low cutoff frequency. The input signal is the audio signal
 to be processed. For instance, here's a time-varying low-pass filtered sawtooth:
 

@racketblock[
 (signal->rsound 
 88200 
 (network ()
          [f <= (simple-ctr 0 1)] ;; the current frame
          [sawtooth = (/ (modulo f 220) 220)]
          [control = (+ 0.5 (* 0.2 (sin (* f 7.123792865282977e-05))))]
          [out <= lpf/dynamic control sawtooth]))]
 }

@defproc[#:kind "signal" (reverb [input number?]) number?]{
 Apply a nice basic reverb to the input. Uses the algorithm and
 the constants from Moorer 1979.}


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

@defproc[(synth-waveform [family string?] [spec number-or-path?])
         rsound]{
 Given a family and a spec, produce an rsound representing the waveform; that is, a one-second-long,
 1 Hz tone.}
 
 
 }

@section{Helper Functions}

@defproc[(nonnegative-integer? [v any]) boolean?]{
 returns true for nonnegative integers.}

@defproc[(positive-integer? [v any]) boolean?]{
 returns true for strictly positive integers.}

@section{Configuration}


@defproc[(diagnose-sound-playing) void?]{
 Tries playing a short tone using all of the available APIs and several plausible sample rates.
 It tries to offer a helpful message, along with the test.}

@defproc[(all-host-apis) (listof symbol?)]{
 Returns a list of symbols representing host APIs supported by the underlying system. This is
 a re-export from the @racket[portaudio] package.
}

@defparam[host-api api symbol?]{
 A parameter that instructs portaudio to choose a particular API to use in playing sounds. If its
 value is @racket[false], portaudio chooses one.
}

@defproc[(set-host-api! (api (or/c false? string?))) void?]{
 A version of the @racket[host-api] parameter that can be used in the teaching languages
 (because it's a regular procedure).
 A parameter that instructs portaudio to choose a particular API to use in playing sounds. If its
 value is @racket[false], portaudio chooses one.
}

@defproc[(display-device-table) void?]{
 Display a table listing all of the available devices: what host API they're associated
 with, what their names are, and the maximum number of input and output channels
 associated with each one.
}

@defproc[(set-output-device! (index (or/c false? natural?))) void]{
 Choose a specific device index number for use with portaudio. Note that this choice
 supersedes the host-api choice.
}

@section{Fsounds}

@defmodule[rsound/fsound]{
As part of a different project, I want a way to manipulate sounds as vectors of doubles.
To handle this, I've copied and updated a bunch of rsound code, to make it work with
vectors of doubles. As time passes and memory gets more common, I expect at some point
simply to switch over to using these sounds everywhere.}

@defproc[(rsound->fsound [rs rsound?]) fsound?]{
 Turn an rsound into an fsound. The result may be much smaller than
 the original, because of lazy clipping.
}

@defproc[(fsound->rsound [fs fsound?]) rsound?]{
 Turn an fsound into an rsound. The result is guaranteed to be at most
 1/4 the size, but may be smaller, because of lazy clipping. Naturally,
 this is an extremely lossy conversion, because doubles hold more information.
}

@defproc[(vector->fsound [fs (vectorof real?)] [sample-rate exact-positive-integer?]) fsound?]{
Given a vector of real numbers and a sample rate, produce an fsound where the samples
in the left and right channels are identical and given by the elements of the vector @racket[fs],
using the given sample rate.
}



@section{Sample Code}

An example of a signal that plays two lines, each with randomly changing 
square-wave tones. This one runs in the Intermediate student language:

@#reader scribble/comment-reader
(racketblock
(require rsound)

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
             [looper <= (loop-ctr change-interval 1)]
             [freq = (maybe-change (prev freq 400) looper)]
             [a <= square-wave freq])))

(define my-signal
  (network ()
           [a <= (scrobble 4000 200 600)]
           [b <= (scrobble 40000 100 200)]
           [lpf-wave <= sine-wave 0.1]
           [c <= lpf/dynamic (max 0.01 (abs (* 0.5 lpf-wave))) (+ a b)]
           [b = (* c 0.1)]))

;; write 20 seconds to a file, if uncommented:
; (rs-write (signal->rsound (* 20 48000) my-signal) "/tmp/foo.wav")

;; play the signal
(signal-play my-signal)
)


An example of a signal that plays from one of the single-cycle vgame tones:

@#reader scribble/comment-reader
(racketmod
racket

(require rsound)

(define waveform (synth-waveform "vgame" 4))

;; wrap i around when it goes off the end:
(define (maybe-wrap i)
  (cond [(< i 48000) i]
        [else (- i 48000)]))

;; a signal that plays from a waveform:
(define loop-sig
  (network (pitch)
    [i = (maybe-wrap (+ (prev i 0) (round pitch)))]
    [out = (rs-ith/left waveform i)]))

(signal-play
 (network ()
   [alternator <= square-wave 2]
   [s <= loop-sig (+ (* 200 (inexact->exact alternator)) 400)]
   [out = (* s 0.1)]))
)


@section{Reporting Bugs}

For Heaven's sake, report lots of bugs!
