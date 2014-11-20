#lang scribble/manual

@title{Signals and Networks}

For signal processing, RSound adopts a dataflow-like paradigm, where elements
may be joined together to form a directed acyclic graph, which is itself an
element that can be joined together, and so forth. So, for instance,
you might have a sine wave generator connected to the amplitude input of
another sine wave generator, and the result pass through a distortion filter. 
Each node accepts a stream of inputs, and produces a stream of outputs. I will
use the term @deftech{node} to refer to both the primitive elements and the
compound elements.

The most basic form of node is simply a procedure.  It takes inputs, and 
produces outputs. In addition, the @racket[network] form provides support
for nodes that are stateful and require initialization.

A node that requires no inputs is called a @deftech{signal}.

Signals can be played directly, with @racket[signal-play]. They may also be
converted to rsounds, using @racket[signal->rsound] or @racket[signals->rsound].

A node that takes one input is called a @deftech{filter}.

@defform[(network (in ...)
                  network-clause
                  ...)
         #:grammar
         [(in identifier)
          (network-clause [node-label = expression]
                          [node-label <= network expression ...]
                          [(node-label ...) = expression]
                          [(node-label ...) <= network expression ...])
          (node-label identifier)]]{
 Produces a network. The @racket[in] names specify input arguments to the
 network.  Each network clause describes a node.  Each node must have a 
 label, which may be used later to refer to the value that is the result 
 of that node. Multiple labels are used for clauses that produce multiple
 values.
 
 There are two kinds of clause. A clause that uses @racket[=] simply gives
 the name to the result of evaluating the right-hand-side expression. A clause
 that uses @racket[<=] evaluates the input expressions, and uses them as 
 inputs to the given network.
 
 The special @(deftech #:key "prev" @racket[(prev node-label init-val)]) form may be used to refer
 to the previous value of the corresponding node. It's fine to have ``forward''
 references to clauses that haven't been evaluated yet. 
 
 The final clause's node
 is used as the output of the network.
 
 The @racket[network] form is useful because it manages the initialization
 of stateful networks, and allows reference to previous outputs.}

Here's a trivial signal:

@racketblock[
(lambda () 3)
]

Here's the same signal, written using @racket[network]:

@racketblock[
(network ()
         [out = 3])]

This is the signal that always produces 3.

Here's another one, that counts upward:

@racketblock[
(define counter/sig
  (network ()
           [counter = (+ 1 (prev counter 0))]))]

The @racket[prev] form is special, and is used to refer to the prior value of the
signal component.

Note that since we're adding one immediately, this counter starts at 1.

Here's another example, that adds together two sine waves, at 34 Hz and 46 Hz, assuming 
a sample rate of 44.1KHz:

@racketblock[
(define sum-of-sines
     (network ()
              [a <= sine-wave 34]
              [b <= sine-wave 46]
              [out = (+ a b)]))]

In order to use a signal with @racket[signal-play], it should produce a real number in the range @racket[-1.0] to @racket[1.0].

Here's an example that uses one sine-wave (often called an "LFO") to control the pitch of another one:

@racketblock[
(define vibrato-tone
  (network ()
           [lfo <= sine-wave 2]
           [sin <= sine-wave (+ 400 (* 50 lfo))]
           [out = (* 0.1 sin)]))
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
                (frame-ctr) signal?]{
 A signal that counts up from zero, representing the number of frames
 since the beginning of the signal.}


@defproc[#:kind "signal"
                (sine-wave [frequency nonnegative-number?]) real?]{
 A signal representing a sine wave of the given
 frequency, at the default sample rate, of amplitude 1.0.}

@defproc[#:kind "signal"
                (sawtooth-wave [frequency nonnegative-number?]) real?]{
 A signal representing a naive sawtooth wave of the given
 frequency, of amplitude 1.0. Note that since this is a simple -1.0 up to 1.0 sawtooth wave, it's got horrible 
 aliasing all over the spectrum.}

@defproc[#:kind "signal"
                (square-wave [frequency nonnegative-number?]) real?]{
 A signal representing a naive square wave of the given
 frequency, of amplitude 1.0, at the default sample rate. It alternates
 between 1.0 and 0.0, which makes it more useful in, e.g., gating 
 applications.
 
 Also note that since this is a simple 1/-1 square wave, it's got horrible 
 aliasing all over the spectrum.}

@defproc[#:kind "signal"
                (pulse-wave [duty-cycle real?] [frequency nonnegative-number?]) real?]{
 A signal representing a "pulse wave", with part of the signal at 1.0 and
 the rest of the signal at 0.0. The @racket[duty-cycle] determines the fraction of the
 cycle that is 1.0. So, for instance, when @racket[duty-cycle] is 0.5, the result is
 a square wave.
}

@defproc[#:kind "signal"
                (dc-signal [amplitude real?]) real?]{
 A constant signal at @racket[amplitude]. Inaudible unless used to multiply by
 another signal.}

The following are functions that @emph{return} signals.

@defproc[(simple-ctr [init real?] [skip real?]) signal?]{
 Produces a signal whose value starts at @racket[init] and increases by @racket[skip]
 at each frame.}

@defproc[(loop-ctr [len real?] [skip real?]) signal?]{
 Produces a signal whose value starts at 0.0 and increases by @racket[skip]
 at each frame, subtracting @racket[len] when the value rises above @racket[len].}

@defproc[(loop-ctr/variable [len real?]) signal?]{
 Produces a signal whose value starts at 0.0 and increases by @racket[skip]
 at each frame, subtracting @racket[len] when the value rises above @racket[len]. In
 this case, the @racket[skip] value is supplied dynamically.}

In order to listen to them, you can transform them into rsounds, or play them directly:

@defproc[(signal->rsound (frames nonnegative-integer?) (signal signal?)) rsound?]{
 Builds a sound of length @racket[frames] at the default sample-rate by using
 @racket[signal]. 
 Both channels are identical.
 
 Here's an example of using it:

 @racketblock[
(define sig1
  (network ()
           [a <= sine-wave 560]
           [out = (* 0.1 a)]))

(define r (signal->rsound 44100 sig1))

(play r)]
}
@defproc[(signals->rsound (frames nonnegative-integer?) 
                             (left-sig signal?) (right-sig signal?)) rsound?]{
 Builds a stereo sound of length @racket[frames] by using
 @racket[left-sig] and @racket[right-sig] to generate the
 samples for the left and right channels.
}

@defproc[(rs-filter (sound rsound?) (filter filter?)) rsound?]{
 Applies the given filter to the given sound to produce a new sound. The
 sound's channels are processed independently. The new sound is of
 the same length as the old sound.}
                                                                             
@defproc[(signal-play (signal signal?)) void?]{
 Plays a (single-channel) signal. Halt playback using @racket[(stop)].}
 

There are several functions that produce signals.

@defproc[(indexed-signal [time->amplitude procedure?]) signal?]{
  Given a mapping from frame (in frames) to amplitude, return a signal. In prior versions of RSound, such
  a mapping was called a signal. This function converts those functions into new-style
  signals.
}

@defproc[(fader [fade-samples number?]) signal?]{
 Produces a signal that decays exponentially. After @racket[fade-samples], its value is @racket[0.001].
 Inaudible unless used to multiply by another signal.}



There are also a number of functions that combine existing signals, 
called "signal combinators":

@defproc[(signal+ [a signal?] [b signal?]) signal?]{
 Produces the signal that is the sum of the two input signals.}

@defproc[(signal-+s [signals (listof signal?)]) signal?]{
 Produces the signal that is the sum of the list of input signals.}

@defproc[(signal* [a signal?] [b signal?]) signal?]{
 Produces the signal that is the product of the two input signals.}

@defproc[(signal-*s [signals (listof signal?)]) signal?]{
 Produces the signal that is the product of the list of input signals.}

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
 arity includes 0, or a network that takes zero inputs?}

@defproc[(filter? [maybe-filter any/c]) boolean?]{
 Is the given value a filter? More precisely, is the given value a procedure whose
 arity includes 1, or a network that takes one input?}

@section{Signal/Blocks}

The signal/block interface can speed up sound generation, by allowing a signal to generate
a block of samples at once. This is particularly valuable when it is possible for 
signals to use c-level primitives to copy blocks of samples.

UNFINISHED: 

@defproc[(signal/block-play [signal/block signal/block/unsafe?] 
                            [sample-rate positive-integer?]
                            [#:buffer-time buffer-time (or/c nonnegative-number #f)])
         any]{
Plays a signal/block/unsafe.
}
@;{;; play a signal/block using portaudio:
(define (signal/block-play signal/block sample-rate #:buffer-time [buffer-time #f])
  (unless (signal/block? signal/block)
    (raise-argument-error 'signal/block-play "signal/block" 0 signal/block sample-rate buffer-time))
  (unless (positive-integer? sample-rate)
    (raise-argument-error 'signal/block-play "positive integer" 1 signal/block sample-rate buffer-time))
  (rc:signal/block-play signal/block sample-rate buffer-time))}

