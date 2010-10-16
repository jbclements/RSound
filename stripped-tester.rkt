#lang racket

;; trying to diagnose a core dump on linux; this doesn't
;; use any of the DrRacket stuff.

(require ffi/unsafe
         ffi/vector
         "private/portaudio.rkt")

(define channels 2)

(define twopi (* 2 pi))

(define max-s16 #x7fff)

(define (play-buffer buffer frames sample-rate)
  (define seconds (/ frames sample-rate))
  (let* ([stream (pa-open-default-stream 0             ;; input channels
                                         channels      ;; output channels
                                         'paInt16      ;; sample format
                                         (exact->inexact sample-rate)  ;; sample rate
                                         0 ;;frames-per-buffer  ;; frames per buffer
                                         #f            ;; callback (NULL means just wait for data)
                                         #f)]         ;; user data
         )
         (pa-start-stream stream)
         (let outer-loop ()
           ;; lexical bindings so that mutations don't take effect immediately:
           (let ([this-buffer buffer]
                 [this-frames frames])
             (let loop ([buf-offset 0] 
                        [sleep-time 0.005])
               ;; if we have a message to handle, handle it:
               (if (< buf-offset this-frames) ;; do we have anything left to send?
                   ;; how much space is there to write?
                   (let ([available-space (pa-get-stream-write-available stream)])
                     (printf "available-space: ~v\n" available-space)
                     (if (= available-space 0)
                         ;; no space, sleep & try again later
                         (begin 
                           (sleep sleep-time)
                           (loop buf-offset sleep-time))
                         ;; enough space, write buffer now.
                         (let ([frames-to-write (min available-space (- this-frames buf-offset))])
                           ;; hide OutputUnderflowed errors:
                           (pa-write-stream stream
                                            (ptr-add (s16vector->cpointer this-buffer) (* channels buf-offset) _sint16)
                                            frames-to-write)
                           (loop (+ buf-offset frames-to-write)
                                 (/ (/ frames-to-write sample-rate) 2)))))
                   (printf "done.\n")))))
         (pa-stop-stream stream)
         (pa-close-stream stream)))

(define (sine-wave pitch sample-rate volume)
  (let ([scalar (* twopi pitch)])
    (lambda (i)
      (let ([t (/ i sample-rate)])
        (* max-s16 (* volume (sin (* scalar t))))))))

(define (make-block frames f)
  (let* ([cblock (make-s16vector (* 4 channels frames))])
    (for ([i (in-range frames)])
      (let* ([offset (* 2 i)]
             [sample (inexact->exact (round (f i)))])
        (s16vector-set! cblock offset        sample)
        (s16vector-set! cblock (add1 offset) sample)))
    cblock))

(define the-block (make-block 44100 (sine-wave 440 44100 0.25)))

(play-buffer the-block 44100 44100)

(sleep 3)

