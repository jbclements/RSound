#lang racket

(require racket/class
         "portaudio.rkt"
         ffi/unsafe
         ffi/vector)

(provide rsound-commander%)

;; messages: play-sound, loop-sound, change-loop, stop
;; states: not playing, playing single, playing loop.

(struct player-msg ())
(struct play-sound-msg (buffer frames sample-rate) #:super struct:player-msg)
(struct loop-sound-msg (buffer frames sample-rate) #:super struct:player-msg)
(struct stop-playing-msg () #:super struct:player-msg)
(struct change-loop-msg (buffer frames) #:super struct:player-msg)

;; assuming 2 channels for the remainder of the file:
(define channels 2)

;; check-below-threshold : rsound threshhold -> (void)
;; signals an error if any sample is above the threshold
;; IRRELEVANT IN THE S16INT WORLD
#;(define (check-below-threshold buffer frames threshold)
  (for ([i (in-range (* channels frames))])
    (when (> (ptr-ref buffer _float i) threshold)
      (error 'check-below-threshold "sound contains samples above threshold ~s."
             threshold))))

;; the channel used to communicate to the player thread.
(define player-evt-channel (make-channel))

;; put a msg on a channel, signal an error if not a legal msg
(define (player-channel-put msg)
  (cond [(player-msg? msg) (channel-put player-evt-channel msg)]
        [else (error 'player-channel-put "expected a player message, got ~e\n"
                     msg)]))


;; somehow we've got to make sure pa-terminate gets called.... or not? Looks
;; like things work even without calling pa-terminate.


(define (start-player-thread)
  (thread
   (lambda ()
     (pa-initialize)
     (let loop ([message (channel-get player-evt-channel)])
       (with-handlers ([exn:fail?
                        (lambda (exn)
                          ;; recover gracefully by logging the message and continuing
                          (log-error (format "play-thread exception: ~a" (exn-message exn)))
                          (loop (channel-get player-evt-channel)))])
       (match message
         ;; ignore stop-playing message if we're already stopped:
         [(struct stop-playing-msg ()) (loop (channel-get player-evt-channel))]
         [(struct play-sound-msg (buffer frames sample-rate))
          (loop (or (play-buffer buffer frames sample-rate #f) (channel-get player-evt-channel)))]
         [(struct loop-sound-msg (buffer frames sample-rate))
          (loop (or (play-buffer buffer frames sample-rate #t) (channel-get player-evt-channel)))]
         ;; ignore change-loop message if we're not playing:
         [(struct change-loop-msg (buffer frames)) 
          (loop (channel-get player-evt-channel))]
         [other
          (error 'start-player-thread "not a player message: ~e" other)]))))))

;; play the sound. Assumes 2 channels, floats only
;; returns #f on end-of-sound termination, returns a new event message
;; if one occurs and is the cause of termination.
(define (play-buffer buffer frames sample-rate loop?)
  ;; size of the portaudio buffer in seconds; this determines how responsive
  ;; the engine is. Making it too small (< 0.01 seconds ?) will result in ... should be just pauses, now.
  ;; under linux, every gc seems to blow this away.  Let's try using half a second, instead.
  ;; whoa! on mac, half a second leads to instand underflow. Oh dear.
  #;(define buffer-time 0.5)
  #;(define frames-per-buffer (inexact->exact (floor (* sample-rate buffer-time))))
  ;; okay, we're just going to let the library make up it's own mind
  (define seconds (/ frames sample-rate))
  (let* ([stream (pa-open-default-stream 0             ;; input channels
                                         channels      ;; output channels
                                         'paInt16      ;; sample format
                                         (exact->inexact sample-rate)  ;; sample rate
                                         0 ;;frames-per-buffer  ;; frames per buffer
                                         #f            ;; callback (NULL means just wait for data)
                                         #f)]         ;; user data
         #;[wait-time (/ buffer-time 2)])
    (dynamic-wind
     void
     (lambda ()
       (pa-start-stream stream)
       (dynamic-wind
        void
        (lambda ()
          ;; use the outer loop if the user calls with loop? = #t
          (let outer-loop ()
            (cond [(cpointer? buffer) (fprintf (current-error-port) "yep, it's a cpointer.\n")]
                  [else (fprintf (current-error-port) "nope, not a cpointer.\n")])
            ;; capture lexical bindings so that mutations don't take effect immediately:
            (let ([this-buffer buffer]
                  [this-frames frames])
              (fprintf (current-error-port) "whew! made it.")
              (let loop ([buf-offset 0] 
                         [sleep-time 0.005])
                ;; if we have a message to handle, handle it:
                (match (channel-try-get player-evt-channel)
                  ;; stop playing:
                  [(struct stop-playing-msg ()) #f]
                  ;; play a different sound
                  [(struct play-sound-msg (buffer frames sample-rate))
                   (play-sound-msg buffer frames sample-rate)]
                  ;; play a different loop:
                  [(struct loop-sound-msg (buffer frames sample-rate))
                   (set! loop? #t)
                   (loop-sound-msg buffer frames sample-rate)]
                  [other ;; including keep-playing & change-loop
                   (match other 
                     ;; change the loop (for next time through)
                     [(struct change-loop-msg (new-buffer new-frames))
                      (set! buffer new-buffer)
                      (set! frames new-frames)]
                     ;; just keep playing:
                     [#f (void)])
                   (if (< buf-offset this-frames) ;; do we have anything left to send?
                       ;; how much space is there to write?
                       (let ([available-space (pa-get-stream-write-available stream)])
                         (if (= available-space 0)
                             ;; no space, sleep & try again later
                             (begin 
                               (sleep sleep-time)
                               (loop buf-offset sleep-time))
                             ;; enough space, write buffer now.
                             (let ([frames-to-write (min available-space (- this-frames buf-offset))])
                               ;; hide OutputUnderflowed errors:
                               (with-handlers ([(lambda (exn) (and (exn:fail? exn)
                                                                   (string=? (exn-message exn) 
                                                                             "Output underflowed")))
                                                (lambda (exn) (log-error "ignoring output-underflowed error"))])
                                 (pa-write-stream stream
                                                  (ptr-add this-buffer (* channels buf-offset) _sint16)
                                                  frames-to-write))
                               (loop (+ buf-offset frames-to-write)
                                     (/ (/ frames-to-write sample-rate) 2)))))
                       ;; should we loop the sound?
                       (if loop? 
                           (outer-loop)
                           #f))])))))
        (lambda () (pa-stop-stream stream))))
     (lambda () (pa-close-stream stream)))))




(define rsound-commander%
  (class object%
    
    (init master-custodian)
    
    (parameterize ([current-custodian master-custodian])
      (start-player-thread))
    
    (define/public (play-sound buffer frames sample-rate)
      (player-channel-put (play-sound-msg buffer frames sample-rate)))
    
    (define/public (loop-sound buffer frames sample-rate)
      (player-channel-put (loop-sound-msg buffer frames sample-rate)))
    
    (define/public (stop-playing)
      (player-channel-put (stop-playing-msg)))
    
    (define/public (change-loop buffer frames)
      (player-channel-put (change-loop-msg buffer frames)))
    
    (super-new)))

