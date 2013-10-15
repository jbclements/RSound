#lang racket/base

(require portaudio
         ffi/vector
         (only-in racket/math pi))

(provide diagnose-sound-playing
         all-host-apis
         host-api
         set-host-api!
         display-device-table
         output-device
         set-output-device!)

;; on Windows+WASAPI, you need to manually set the playback device to 44.1KHz,
;; or else playback simply fails with an "invalid device" error. Running
;; the "diagnose-sound-playing" function simply tries playing tones 
;; to each of the available apis using the most common 
;; just try playing using different host api's and different sample rates


;; these seem to be the most common sample rates.
(define sample-rates '(44100.0 48000.0))

(define s16vec-len 24000)
(define s16vec (make-s16vector (* 2 s16vec-len)))
;; sample tone will be 440 Hz at 44.1K, lower for 48K
(for ([i s16vec-len])
  (define sample
    (inexact->exact
     (floor 
      (* 32767 (* 0.2 (sin (* i 440.0 2.0 pi (/ 1.0 44100.0))))))))
  (s16vector-set! s16vec (* i 2) sample)
  (s16vector-set! s16vec (add1 (* i 2)) sample))

;; -> 
;; try different host api's and sample rates
(define (diagnose-sound-playing)
  (pa-maybe-initialize)
  (define host-apis (all-host-apis))
  (printf "found ~s host API(s): ~s\n"
          (length host-apis)
          host-apis)
  (printf "trying each one in turn.\n")
  (for ([api host-apis])
    (printf "trying api ~s:\n" api)
    (parameterize ([host-api api])
      (for ([sr sample-rates])
        (printf "trying to play at sample rate ~s:\n" sr)
        (with-handlers ([exn:fail?
                         (lambda (exn)
                           (fprintf
                            (current-error-port)
                            "playing sound failed with message: ~s\n"
                            (exn-message exn)))])
          (s16vec-play s16vec 0 s16vec-len sr)
          (sleep (+ 1.0 (/ s16vec-len sr)))
          (printf "configuration tested. Press <return> to go to next test.\n")
          (read-line)))))
  (printf "~a" followup-message))

(define followup-message
  #<<|
If playback at 44100.0 failed and playback at another sample rate
succeeded using Windows 7, you probably need to manually set the 
sample rate of that playback device to 44100 Hz, by right-clicking 
on the volume icon and then digging through menus (properties, advanced).

|
  )

