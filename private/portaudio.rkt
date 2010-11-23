#lang racket

(require ffi/unsafe
         racket/runtime-path)


;; use local copies of the libraries for Windows & Mac...
(define-runtime-path win-dll-path "..\\lib\\portaudio_x86")
(define-runtime-path mac-dll-path "../lib/libportaudio")

(define libportaudio
  (case (system-type)
    [(windows) (ffi-lib win-dll-path)]
    [(macosx)  (with-handlers ()
                 (ffi-lib mac-dll-path '("2.0.0" "")))]
    [(unix)    (with-handlers ([exn:fail? 
                                (lambda (exn)
                                  (error 'rsound "Note: on Linux, you need to install the libportaudio library yourself. Underlying error message: ~a" 
                                         (exn-message exn)))])
                 (ffi-lib "libportaudio" '("2.0.0" "")))]))

(provide (all-defined-out))

;; headers taken from 19.20071207 release of portaudio.h

#|
/** Retrieve the release number of the currently running PortAudio build,
 eg 1900.
*/
int Pa_GetVersion( void );
|#
(define pa-get-version 
  (get-ffi-obj "Pa_GetVersion" 
               libportaudio
               (_fun -> _int)))

#|
/** Retrieve a textual description of the current PortAudio build,
 eg "PortAudio V19-devel 13 October 2002".
*/
const char* Pa_GetVersionText( void );
|#
(define pa-get-version-text
    (get-ffi-obj "Pa_GetVersionText"
                 libportaudio
                 (_fun -> _string)))


#|
/** Error codes returned by PortAudio functions.
 Note that with the exception of paNoError, all PaErrorCodes are negative.
*/
|#

(define _pa-error
  (_enum
   '(paNoError = 0
               
     paNotInitialized = -10000
     paUnanticipatedHostError
     paInvalidChannelCount
     paInvalidSampleRate
     paInvalidDevice
     paInvalidFlag
     paSampleFormatNotSupported
     paBadIODeviceCombination
     paInsufficientMemory
     paBufferTooBig
     paBufferTooSmall
     paNullCallback
     paBadStreamPtr
     paTimedOut
     paInternalError
     paDeviceUnavailable
     paIncompatibleHostApiSpecificStreamInfo
     paStreamIsStopped
     paStreamIsNotStopped
     paInputOverflowed
     paOutputUnderflowed
     paHostApiNotFound
     paInvalidHostApi
     paCanNotReadFromACallbackStream     ;; /**< @todo review error code name */
     paCanNotWriteToACallbackStream      ;; /**< @todo review error code name */
     paCanNotReadFromAnOutputOnlyStream  ;; /**< @todo review error code name */
     paCanNotWriteToAnInputOnlyStream    ;; /**< @todo review error code name */
     paIncompatibleStreamHostApi
     paBadBufferPtr)))

#|
/** Translate the supplied PortAudio error code into a human readable
 message.
*/
const char *Pa_GetErrorText( PaError errorCode );
|#
(define pa-get-error-text
  (get-ffi-obj "Pa_GetErrorText"
               libportaudio
               (_fun _pa-error -> _string)))

#|
/** Library initialization function - call this before using PortAudio.
 This function initialises internal data structures and prepares underlying
 host APIs for use.  With the exception of Pa_GetVersion(), Pa_GetVersionText(),
 and Pa_GetErrorText(), this function MUST be called before using any other
 PortAudio API functions.

 If Pa_Initialize() is called multiple times, each successful 
 call must be matched with a corresponding call to Pa_Terminate(). 
 Pairs of calls to Pa_Initialize()/Pa_Terminate() may overlap, and are not 
 required to be fully nested.

 Note that if Pa_Initialize() returns an error code, Pa_Terminate() should
 NOT be called.

 @return paNoError if successful, otherwise an error code indicating the cause
 of failure.

 @see Pa_Terminate
*/
PaError Pa_Initialize( void );
|#

(define pa-initialize/unchecked
  (get-ffi-obj "Pa_Initialize" 
               libportaudio
               (_fun -> _pa-error)))

#|
/** Library termination function - call this when finished using PortAudio.
 This function deallocates all resources allocated by PortAudio since it was
 initializied by a call to Pa_Initialize(). In cases where Pa_Initialise() has
 been called multiple times, each call must be matched with a corresponding call
 to Pa_Terminate(). The final matching call to Pa_Terminate() will automatically
 close any PortAudio streams that are still open.

 Pa_Terminate() MUST be called before exiting a program which uses PortAudio.
 Failure to do so may result in serious resource leaks, such as audio devices
 not being available until the next reboot.

 @return paNoError if successful, otherwise an error code indicating the cause
 of failure.
 
 @see Pa_Initialize
*/
PaError Pa_Terminate( void );
|#

(define pa-terminate/unchecked
  (get-ffi-obj "Pa_Terminate"
               libportaudio
               (_fun -> _pa-error)))



#|
/** The type used to refer to audio devices. Values of this type usually
 range from 0 to (Pa_GetDeviceCount()-1), and may also take on the PaNoDevice
 and paUseHostApiSpecificDeviceSpecification values.

 @see Pa_GetDeviceCount, paNoDevice, paUseHostApiSpecificDeviceSpecification
*/
typedef int PaDeviceIndex;


/** A special PaDeviceIndex value indicating that no device is available,
 or should be used.

 @see PaDeviceIndex
*/
#define paNoDevice ((PaDeviceIndex)-1)


/** A special PaDeviceIndex value indicating that the device(s) to be used
 are specified in the host api specific stream info structure.

 @see PaDeviceIndex
*/
#define paUseHostApiSpecificDeviceSpecification ((PaDeviceIndex)-2)
|#

(define _pa-device-index _int)
(define _pa-no-device -1)
(define _pa-use-host-api-specific-device-specification -2)

#|
/** The type used to represent monotonic time in seconds that can be used
 for syncronisation. The type is used for the outTime argument to the
 PaStreamCallback and as the result of Pa_GetStreamTime().
     
 @see PaStreamCallback, Pa_GetStreamTime
*/
typedef double PaTime;
|#
(define _pa-time _double)


#|
/** A type used to specify one or more sample formats. Each value indicates
 a possible format for sound data passed to and from the stream callback,
 Pa_ReadStream and Pa_WriteStream.

 The standard formats paFloat32, paInt16, paInt32, paInt24, paInt8
 and aUInt8 are usually implemented by all implementations.

 The floating point representation (paFloat32) uses +1.0 and -1.0 as the
 maximum and minimum respectively.

 paUInt8 is an unsigned 8 bit format where 128 is considered "ground"

 The paNonInterleaved flag indicates that a multichannel buffer is passed
 as a set of non-interleaved pointers.

 @see Pa_OpenStream, Pa_OpenDefaultStream, PaDeviceInfo
 @see paFloat32, paInt16, paInt32, paInt24, paInt8
 @see paUInt8, paCustomFormat, paNonInterleaved
*/
typedef unsigned long PaSampleFormat;


#define paFloat32        ((PaSampleFormat) 0x00000001) /**< @see PaSampleFormat */
#define paInt32          ((PaSampleFormat) 0x00000002) /**< @see PaSampleFormat */
#define paInt24          ((PaSampleFormat) 0x00000004) /**< Packed 24 bit format. @see PaSampleFormat */
#define paInt16          ((PaSampleFormat) 0x00000008) /**< @see PaSampleFormat */
#define paInt8           ((PaSampleFormat) 0x00000010) /**< @see PaSampleFormat */
#define paUInt8          ((PaSampleFormat) 0x00000020) /**< @see PaSampleFormat */
#define paCustomFormat   ((PaSampleFormat) 0x00010000)/**< @see PaSampleFormat */

#define paNonInterleaved ((PaSampleFormat) 0x80000000)
|#

(define _pa-sample-format
  (_bitmask
   '(paFloat32        = #x00000001
     paInt32          = #x00000002
     paInt24          = #x00000004
     paInt16          = #x00000008
     paInt8           = #x00000010
     paUInt8          = #x00000020
     paCustomFormat   = #x00010000
     
     paNonInterleaved = #x80000000)))


(define _pa-stream _pointer)



#|
/** Parameters for one direction (input or output) of a stream.
*/
typedef struct PaStreamParameters
{
    /** A valid device index in the range 0 to (Pa_GetDeviceCount()-1)
     specifying the device to be used or the special constant
     paUseHostApiSpecificDeviceSpecification which indicates that the actual
     device(s) to use are specified in hostApiSpecificStreamInfo.
     This field must not be set to paNoDevice.
    */
    PaDeviceIndex device;
    
    /** The number of channels of sound to be delivered to the
     stream callback or accessed by Pa_ReadStream() or Pa_WriteStream().
     It can range from 1 to the value of maxInputChannels in the
     PaDeviceInfo record for the device specified by the device parameter.
    */
    int channelCount;

    /** The sample format of the buffer provided to the stream callback,
     a_ReadStream() or Pa_WriteStream(). It may be any of the formats described
     by the PaSampleFormat enumeration.
    */
    PaSampleFormat sampleFormat;

    /** The desired latency in seconds. Where practical, implementations should
     configure their latency based on these parameters, otherwise they may
     choose the closest viable latency instead. Unless the suggested latency
     is greater than the absolute upper limit for the device implementations
     should round the suggestedLatency up to the next practial value - ie to
     provide an equal or higher latency than suggestedLatency wherever possibe.
     Actual latency values for an open stream may be retrieved using the
     inputLatency and outputLatency fields of the PaStreamInfo structure
     returned by Pa_GetStreamInfo().
     @see default*Latency in PaDeviceInfo, *Latency in PaStreamInfo
    */
    PaTime suggestedLatency;

    /** An optional pointer to a host api specific data structure
     containing additional information for device setup and/or stream processing.
     hostApiSpecificStreamInfo is never required for correct operation,
     if not used it should be set to NULL.
    */
    void *hostApiSpecificStreamInfo;

} PaStreamParameters;

|#


;; *** UNTESTED ***:

(define-cstruct _pa-stream-parameters
  ([device                        _pa-device-index]
   [channel-count                 _int]
   [sample-format                 _pa-sample-format]
   [suggested-latency             _pa-time]
   [host-api-specific-stream-info _pointer]))

#|/** Flags used to control the behavior of a stream. They are passed as
 parameters to Pa_OpenStream or Pa_OpenDefaultStream. Multiple flags may be
 ORed together.

 @see Pa_OpenStream, Pa_OpenDefaultStream
 @see paNoFlag, paClipOff, paDitherOff, paNeverDropInput,
  paPrimeOutputBuffersUsingStreamCallback, paPlatformSpecificFlags
*/
typedef unsigned long PaStreamFlags;

/** @see PaStreamFlags */
#define   paNoFlag          ((PaStreamFlags) 0)

/** Disable default clipping of out of range samples.
 @see PaStreamFlags
*/
#define   paClipOff         ((PaStreamFlags) 0x00000001)

/** Disable default dithering.
 @see PaStreamFlags
*/
#define   paDitherOff       ((PaStreamFlags) 0x00000002)

/** Flag requests that where possible a full duplex stream will not discard
 overflowed input samples without calling the stream callback. This flag is
 only valid for full duplex callback streams and only when used in combination
 with the paFramesPerBufferUnspecified (0) framesPerBuffer parameter. Using
 this flag incorrectly results in a paInvalidFlag error being returned from
 Pa_OpenStream and Pa_OpenDefaultStream.

 @see PaStreamFlags, paFramesPerBufferUnspecified
*/
#define   paNeverDropInput  ((PaStreamFlags) 0x00000004)

/** Call the stream callback to fill initial output buffers, rather than the
 default behavior of priming the buffers with zeros (silence). This flag has
 no effect for input-only and blocking read/write streams.
 
 @see PaStreamFlags
*/
#define   paPrimeOutputBuffersUsingStreamCallback ((PaStreamFlags) 0x00000008)

/** A mask specifying the platform specific bits.
 @see PaStreamFlags
*/
#define   paPlatformSpecificFlags ((PaStreamFlags)0xFFFF0000)
|#


;; *** UNTESTED ***:

(define _pa-stream-flags
  (_bitmask
   '(pa-no-flag                 = #x00000000
     pa-clip-off                = #x00000001
     pa-dither-off              = #x00000002
     pa-never-drop-input        = #x00000004
     pa-prime-output-buffers-using-stream-callback = #x00000008
     pa-platform-specific-flags = #xFFFF0000)))


#|
/**
 Timing information for the buffers passed to the stream callback.
*/
typedef struct PaStreamCallbackTimeInfo{
    PaTime inputBufferAdcTime;
    PaTime currentTime;
    PaTime outputBufferDacTime;
} PaStreamCallbackTimeInfo;
|#


;; *** UNTESTED ***:

(define-cstruct _pa-stream-callback-time-info
  ([input-buffer-adc-time  _pa-time]
   [current-time           _pa-time]
   [output-buffer-dac-time _pa-time]))
#|
/**
 Flag bit constants for the statusFlags to PaStreamCallback.

 @see paInputUnderflow, paInputOverflow, paOutputUnderflow, paOutputOverflow,
 paPrimingOutput
*/
typedef unsigned long PaStreamCallbackFlags;

/** In a stream opened with paFramesPerBufferUnspecified, indicates that
 input data is all silence (zeros) because no real data is available. In a
 stream opened without paFramesPerBufferUnspecified, it indicates that one or
 more zero samples have been inserted into the input buffer to compensate
 for an input underflow.
 @see PaStreamCallbackFlags
*/
#define paInputUnderflow   ((PaStreamCallbackFlags) 0x00000001)

/** In a stream opened with paFramesPerBufferUnspecified, indicates that data
 prior to the first sample of the input buffer was discarded due to an
 overflow, possibly because the stream callback is using too much CPU time.
 Otherwise indicates that data prior to one or more samples in the
 input buffer was discarded.
 @see PaStreamCallbackFlags
*/
#define paInputOverflow    ((PaStreamCallbackFlags) 0x00000002)

/** Indicates that output data (or a gap) was inserted, possibly because the
 stream callback is using too much CPU time.
 @see PaStreamCallbackFlags
*/
#define paOutputUnderflow  ((PaStreamCallbackFlags) 0x00000004)

/** Indicates that output data will be discarded because no room is available.
 @see PaStreamCallbackFlags
*/
#define paOutputOverflow   ((PaStreamCallbackFlags) 0x00000008)

/** Some of all of the output data will be used to prime the stream, input
 data may be zero.
 @see PaStreamCallbackFlags
*/
#define paPrimingOutput    ((PaStreamCallbackFlags) 0x00000010)

|#


;; *** UNTESTED ***:

(define _pa-stream-callback-flags
  (_bitmask
   '(pa-input-underflow  = #x00000001
     pa-input-overflow   = #x00000002
     pa-output-underflow = #x00000004
     pa-output-overflow  = #x00000008
     pa-priming-output   = #x00000010)))

#|

/**
 Allowable return values for the PaStreamCallback.
 @see PaStreamCallback
*/
typedef enum PaStreamCallbackResult
{
    paContinue=0,
    paComplete=1,
    paAbort=2
} PaStreamCallbackResult;

|#

(define _pa-stream-callback-result
  (_enum
   '(pa-continue = 0
     pa-complete = 1
     pa-abort    = 2)))

#|

/**
 Functions of type PaStreamCallback are implemented by PortAudio clients.
 They consume, process or generate audio in response to requests from an
 active PortAudio stream.
     
 @param input and @param output are arrays of interleaved samples,
 the format, packing and number of channels used by the buffers are
 determined by parameters to Pa_OpenStream().
     
 @param frameCount The number of sample frames to be processed by
 the stream callback.

 @param timeInfo The time in seconds when the first sample of the input
 buffer was received at the audio input, the time in seconds when the first
 sample of the output buffer will begin being played at the audio output, and
 the time in seconds when the stream callback was called.
 See also Pa_GetStreamTime()

 @param statusFlags Flags indicating whether input and/or output buffers
 have been inserted or will be dropped to overcome underflow or overflow
 conditions.

 @param userData The value of a user supplied pointer passed to
 Pa_OpenStream() intended for storing synthesis data etc.

 @return
 The stream callback should return one of the values in the
 PaStreamCallbackResult enumeration. To ensure that the callback continues
 to be called, it should return paContinue (0). Either paComplete or paAbort
 can be returned to finish stream processing, after either of these values is
 returned the callback will not be called again. If paAbort is returned the
 stream will finish as soon as possible. If paComplete is returned, the stream
 will continue until all buffers generated by the callback have been played.
 This may be useful in applications such as soundfile players where a specific
 duration of output is required. However, it is not necessary to utilise this
 mechanism as Pa_StopStream(), Pa_AbortStream() or Pa_CloseStream() can also
 be used to stop the stream. The callback must always fill the entire output
 buffer irrespective of its return value.

 @see Pa_OpenStream, Pa_OpenDefaultStream

 @note With the exception of Pa_GetStreamCpuLoad() it is not permissable to call
 PortAudio API functions from within the stream callback.
*/
typedef int PaStreamCallback(
    const void *input, void *output,
    unsigned long frameCount,
    const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags,
    void *userData );
|#

(define _pa-stream-callback
  (_fun #:atomic? #t
        #:keep #t
        #:async-apply (lambda (t) (t))
        _pointer
        _pointer
        _ulong
        _pa-stream-callback-time-info-pointer
        _pa-stream-callback-flags
        _pointer
        -> _pa-stream-callback-result))
#|
/** Opens a stream for either input, output or both.
     
 @param stream The address of a PaStream pointer which will receive
 a pointer to the newly opened stream.
     
 @param inputParameters A structure that describes the input parameters used by
 the opened stream. See PaStreamParameters for a description of these parameters.
 inputParameters must be NULL for output-only streams.

 @param outputParameters A structure that describes the output parameters used by
 the opened stream. See PaStreamParameters for a description of these parameters.
 outputParameters must be NULL for input-only streams.
 
 @param sampleRate The desired sampleRate. For full-duplex streams it is the
 sample rate for both input and output
     
 @param framesPerBuffer The number of frames passed to the stream callback
 function, or the preferred block granularity for a blocking read/write stream.
 The special value paFramesPerBufferUnspecified (0) may be used to request that
 the stream callback will recieve an optimal (and possibly varying) number of
 frames based on host requirements and the requested latency settings.
 Note: With some host APIs, the use of non-zero framesPerBuffer for a callback
 stream may introduce an additional layer of buffering which could introduce
 additional latency. PortAudio guarantees that the additional latency
 will be kept to the theoretical minimum however, it is strongly recommended
 that a non-zero framesPerBuffer value only be used when your algorithm
 requires a fixed number of frames per stream callback.
 
 @param streamFlags Flags which modify the behaviour of the streaming process.
 This parameter may contain a combination of flags ORed together. Some flags may
 only be relevant to certain buffer formats.
     
 @param streamCallback A pointer to a client supplied function that is responsible
 for processing and filling input and output buffers. If this parameter is NULL
 the stream will be opened in 'blocking read/write' mode. In blocking mode,
 the client can receive sample data using Pa_ReadStream and write sample data
 using Pa_WriteStream, the number of samples that may be read or written
 without blocking is returned by Pa_GetStreamReadAvailable and
 Pa_GetStreamWriteAvailable respectively.

 @param userData A client supplied pointer which is passed to the stream callback
 function. It could for example, contain a pointer to instance data necessary
 for processing the audio buffers. This parameter is ignored if streamCallback
 is NULL.
     
 @return
 Upon success Pa_OpenStream() returns paNoError and places a pointer to a
 valid PaStream in the stream argument. The stream is inactive (stopped).
 If a call to Pa_OpenStream() fails, a non-zero error code is returned (see
 PaError for possible error codes) and the value of stream is invalid.

 @see PaStreamParameters, PaStreamCallback, Pa_ReadStream, Pa_WriteStream,
 Pa_GetStreamReadAvailable, Pa_GetStreamWriteAvailable
*/
PaError Pa_OpenStream( PaStream** stream,
                       const PaStreamParameters *inputParameters,
                       const PaStreamParameters *outputParameters,
                       double sampleRate,
                       unsigned long framesPerBuffer,
                       PaStreamFlags streamFlags,
                       PaStreamCallback *streamCallback,
                       void *userData );

|#

;; *** UNTESTED ***:

#;(define pa-open-stream
  (get-ffi-obj "Pa_OpenStream"
               libportaudio
               (_fun (result : (_ptr o _pa-stream)) ;; stream
                     _pa-stream-parameters ;; inputParameters
                     _pa-stream-parameters ;; outputParameters
                     _double ;; sampleRate
                     _ulong ;; framesPerBuffer
                     _pa-stream-flags ;; streamFlags
                     ;; note: give this a name to prevent it from getting collected:
                     (callback : _pa-stream-callback) ;; streamCallback
                     _pointer ;; userData?
                     -> (err : _pa-error)
                     -> (match err
                          ['paNoError result]
                          [other (error 'pa-open-default-stream "~a" (pa-get-error-text err))]))))


#| A simplified version of Pa_OpenStream() that opens the default input
 and/or output devices.

 @param stream The address of a PaStream pointer which will receive
 a pointer to the newly opened stream.
 
 @param numInputChannels  The number of channels of sound that will be supplied
 to the stream callback or returned by Pa_ReadStream. It can range from 1 to
 the value of maxInputChannels in the PaDeviceInfo record for the default input
 device. If 0 the stream is opened as an output-only stream.

 @param numOutputChannels The number of channels of sound to be delivered to the
 stream callback or passed to Pa_WriteStream. It can range from 1 to the value
 of maxOutputChannels in the PaDeviceInfo record for the default output dvice.
 If 0 the stream is opened as an output-only stream.

 @param sampleFormat The sample format of both the input and output buffers
 provided to the callback or passed to and from Pa_ReadStream and Pa_WriteStream.
 sampleFormat may be any of the formats described by the PaSampleFormat
 enumeration.
 
 @param sampleRate Same as Pa_OpenStream parameter of the same name.
 @param framesPerBuffer Same as Pa_OpenStream parameter of the same name.
 @param streamCallback Same as Pa_OpenStream parameter of the same name.
 @param userData Same as Pa_OpenStream parameter of the same name.

 @return As for Pa_OpenStream

 @see Pa_OpenStream, PaStreamCallback

PaError Pa_OpenDefaultStream( PaStream** stream,
                              int numInputChannels,
                              int numOutputChannels,
                              PaSampleFormat sampleFormat,
                              double sampleRate,
                              unsigned long framesPerBuffer,
                              PaStreamCallback *streamCallback,
                              void *userData );
|#
(define pa-open-default-stream
  (get-ffi-obj "Pa_OpenDefaultStream"
               libportaudio
               (_fun (result : (_ptr o _pa-stream)) ;; stream
                     _int ;; numInputChannels
                     _int ;; numOutputChannels
                     _pa-sample-format ;; sampleFormat
                     _double ;; sampleRate
                     _ulong ;; framesPerBuffer
                     ;; give this a name to prevent it from being collected:
                     (callback : _pa-stream-callback) ;; streamCallback
                     _pointer ;; userData?
                     -> (err : _pa-error)
                     -> (match err
                          ['paNoError result]
                          [other (error 'pa-open-default-stream "~a ~a" (pa-get-error-text err)
                                        callback)]))))
#|
/** Closes an audio stream. If the audio stream is active it
 discards any pending buffers as if Pa_AbortStream() had been called.
*/
PaError Pa_CloseStream( PaStream *stream );
|#
(define pa-close-stream/unchecked 
  (get-ffi-obj "Pa_CloseStream"
               libportaudio
               (_fun _pa-stream -> _pa-error)))

#|
/** Commences audio processing.
*/
PaError Pa_StartStream( PaStream *stream );
|#
(define pa-start-stream/unchecked
  (get-ffi-obj "Pa_StartStream"
               libportaudio
               (_fun _pa-stream -> _pa-error)))

#|
/** Terminates audio processing. It waits until all pending
 audio buffers have been played before it returns.
*/
PaError Pa_StopStream( PaStream *stream );
|#
(define pa-stop-stream/unchecked
  (get-ffi-obj "Pa_StopStream"
               libportaudio
               (_fun _pa-stream -> _pa-error)))

#|
/** Terminates audio processing immediately without waiting for pending
 buffers to complete.
*/
PaError Pa_AbortStream( PaStream *stream );
|#

;; *** UNTESTED ***:

(define pa-abort-stream/unchecked
  (get-ffi-obj "Pa_AbortStream"
               libportaudio
               (_fun _pa-stream -> _pa-error)))



#|/** Read samples from an input stream. The function doesn't return until
 the entire buffer has been filled - this may involve waiting for the operating
 system to supply the data.

 @param stream A pointer to an open stream previously created with Pa_OpenStream.
 
 @param buffer A pointer to a buffer of sample frames. The buffer contains
 samples in the format specified by the inputParameters->sampleFormat field
 used to open the stream, and the number of channels specified by
 inputParameters->numChannels. If non-interleaved samples were requested,
 buffer is a pointer to the first element of an array of non-interleaved
 buffer pointers, one for each channel.

 @param frames The number of frames to be read into buffer. This parameter
 is not constrained to a specific range, however high performance applications
 will want to match this parameter to the framesPerBuffer parameter used
 when opening the stream.

 @return On success PaNoError will be returned, or PaInputOverflowed if input
 data was discarded by PortAudio after the previous call and before this call.
*/
PaError Pa_ReadStream( PaStream* stream,
                       void *buffer,
                       unsigned long frames );
|#

;; *** UNTESTED ***:

(define pa-read-stream/unchecked
  (get-ffi-obj "Pa_ReadStream"
               libportaudio
               (_fun _pa-stream _pointer _ulong -> _pa-error)))

#|/** Write samples to an output stream. This function doesn't return until the
 entire buffer has been consumed - this may involve waiting for the operating
 system to consume the data.

 @param stream A pointer to an open stream previously created with Pa_OpenStream.

 @param buffer A pointer to a buffer of sample frames. The buffer contains
 samples in the format specified by the outputParameters->sampleFormat field
 used to open the stream, and the number of channels specified by
 outputParameters->numChannels. If non-interleaved samples were requested,
 buffer is a pointer to the first element of an array of non-interleaved
 buffer pointers, one for each channel.

 @param frames The number of frames to be written from buffer. This parameter
 is not constrained to a specific range, however high performance applications
 will want to match this parameter to the framesPerBuffer parameter used
 when opening the stream.

 @return On success PaNoError will be returned, or paOutputUnderflowed if
 additional output data was inserted after the previous call and before this
 call.
*/
PaError Pa_WriteStream( PaStream* stream,
                        const void *buffer,
                        unsigned long frames );
|#

(define pa-write-stream/unchecked
  (get-ffi-obj "Pa_WriteStream"
               libportaudio
               (_fun _pa-stream _pointer _ulong -> _pa-error)))

#|/** Retrieve the number of frames that can be read from the stream without
 waiting.

 @return Returns a non-negative value representing the maximum number of frames
 that can be read from the stream without blocking or busy waiting or, a
 PaErrorCode (which are always negative) if PortAudio is not initialized or an
 error is encountered.
*/
signed long Pa_GetStreamReadAvailable( PaStream* stream );
|#

;; *** UNTESTED ***:

(define pa-get-stream-read-available
  (get-ffi-obj "Pa_GetStreamReadAvailable"
               libportaudio
               (_fun _pa-stream -> 
                     [err-or-result : _pa-error]
                     -> (cond [(< err-or-result 0) 
                               (error 'pa-get-stream-read-available "~a" 
                                      (pa-get-error-text err-or-result))]
                              [else err-or-result]))))

#|/** Retrieve the number of frames that can be written to the stream without
 waiting.

 @return Returns a non-negative value representing the maximum number of frames
 that can be written to the stream without blocking or busy waiting or, a
 PaErrorCode (which are always negative) if PortAudio is not initialized or an
 error is encountered.
*/
signed long Pa_GetStreamWriteAvailable( PaStream* stream );

|#

(define pa-get-stream-write-available
  (get-ffi-obj "Pa_GetStreamWriteAvailable"
               libportaudio
               (_fun _pa-stream -> 
                     [err-or-result : _slong]
                     -> (cond [(< err-or-result 0) 
                               (error 'pa-get-stream-write-available "~a" 
                                      (pa-get-error-text err-or-result))]
                              [else err-or-result]))))


;; wrap a function to signal an error when an error code is returned.
;; (any ... -> pa-error) -> (any ... -> )
(define (pa-checked pa-fun name)
  (lambda args
    (match (apply pa-fun args)
      ['paNoError (void)]
      [(? symbol? s) (error (pa-get-error-text s))]
      [other (error name
                    "internal error: expected a symbol, got: ~s"
                    other)])))


;; defined checked forms of functions

(define pa-initialize (pa-checked pa-initialize/unchecked 'pa-initialize))
(define pa-terminate (pa-checked pa-terminate/unchecked 'pa-terminate))

;; pa-open-default-stream already checked...
(define pa-close-stream (pa-checked pa-close-stream/unchecked 'pa-close-stream))

(define pa-start-stream (pa-checked pa-start-stream/unchecked 'pa-start-stream))
(define pa-stop-stream (pa-checked pa-stop-stream/unchecked 'pa-stop-stream))
(define pa-abort-stream (pa-checked pa-abort-stream/unchecked 'pa-abort-stream))

(define pa-read-stream (pa-checked pa-read-stream/unchecked 'pa-read-stream))
(define pa-write-stream (pa-checked pa-write-stream/unchecked 'pa-write-stream))


;; spawn a new thread to put a value on a synchronous channel
(define (channel-put/async channel val)
  (thread (lambda () 
            (channel-put channel val))))

;; resurrected code from an ancient revision:

(define (make-copying-callback total-frames master-buffer response-channel)
  (let* ([channels 2]
         [sample-offset 0] ;; mutable, to track through the buffer
         [total-samples (* total-frames channels)]
         [abort-flag (box #f)])
    (values
     abort-flag
     (lambda (input output frame-count time-info status-flags user-data)
       ;; MUST NOT ALLOW AN EXCEPTION TO ESCAPE.
       (with-handlers ([(lambda (exn) #t)
                        (lambda (exn)
                          (channel-put/async response-channel exn)
                          'pa-abort)])
         (cond 
           [(unbox abort-flag) 
            (channel-put/async response-channel 'abort-flag)
            'pa-abort]
           [else 
            (let ([buffer-samples (* frame-count channels)])
              (cond
                [(> (+ sample-offset buffer-samples) total-samples)
                 (channel-put/async response-channel 'finished)
                 ;; for now, just truncate if it doesn't come out even:
                 ;; NB: all zero bits is the sint16 representation of 0
                 (begin (memset output 0 buffer-samples _sint16)
                        'pa-complete)]
                [else
                 (begin (memcpy output
                                0
                                master-buffer
                                sample-offset
                                buffer-samples
                                _sint16)
                        (set! sample-offset (+ sample-offset buffer-samples))
                        'pa-continue)]))]))))))

(define (make-generating-callback signal response-channel)
  (let* ([channels 2]
         [s16max 32767]
         [sample-offset 0] ;; mutable, to track time
         [abort-flag (box #f)])
    (values
     abort-flag
     (lambda (input output frame-count time-info status-flags user-data)
       ;; MUST NOT ALLOW AN EXCEPTION TO ESCAPE.
       (with-handlers ([(lambda (exn) #t)
                        (lambda (exn)
                          (channel-put/async response-channel exn)
                          'pa-abort)])
         (cond 
           [(unbox abort-flag) 
            (channel-put/async response-channel 'abort-flag)
            'pa-abort]
           [else 
            (for ([t (in-range sample-offset (+ sample-offset frame-count))]
                  [i (in-range 0 (* 2 frame-count) 2)])
              (let ([sample (inexact->exact (round (* s16max (min 1.0 (max -1.0 (signal t))))))])
                (ptr-set! output _sint16 i sample)
                (ptr-set! output _sint16 (+ i 1) sample)))
            (set! sample-offset (+ sample-offset frame-count))
            'pa-continue]))))))

