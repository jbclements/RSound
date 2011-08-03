#lang racket

(require racket/class
         "portaudio.rkt"
         (only-in ffi/unsafe cpointer?))

;; implementation overview: this module starts a player thread that interacts
;; with the portaudio library.  The 'provide'd functions communicate with this
;; thread using a channel.

;; the player thread has two different "states", corresponding to the two
;; synchronization points: idle, and playing. The idle state is implemented
;; by 'idle-state', and the playing state by 'playing-state'. Rather than 
;; making tail calls to each other, the idle state calls the playing-state
;; and receives an event as a return value. This makes the logic a bit less
;; pretty, but it allows the body of the playing-state function to be
;; inside of a dynamic-wind.

;; The use of "pa-initialize" and "pa-terminate" from the portaudio library
;; is a bit idiosyncratic--the documentation suggests calling pa-initialize
;; once at the beginning of the app and calling pa-terminate at the end--but
;; calling each one before and after each playback operation seems not to 
;; consume too much memory or time, and I can't figure out a better way to
;; make it happen on shutdown. (Actually, I'm not sure that *this* does it,
;; either....)

(define (frames? n) 
  (and (exact-integer? n)
       (<= 0 n)))
(define (sample-rate? n)
  (and (exact-integer? n)
       (< 0 n)))

(provide/contract (buffer-play (-> cpointer?
                                   frames?
                                   sample-rate?
                                   void?))
                  (buffer-loop (-> cpointer?
                                   frames?
                                   sample-rate?
                                   void?))
                  (signal-play (-> any/c ;; don't want to slow down calls to the signal
                                   sample-rate?
                                   void?))
                  (signal/block-play (-> any/c
                                         sample-rate?
                                         void?))
                  (stop-playing (-> void?)))



(define (buffer-play buffer frames sample-rate)
  (player-channel-put (play-sound-msg buffer frames sample-rate)))

;; won't loop, right now.
(define (buffer-loop buffer frames sample-rate)
  (player-channel-put (loop-sound-msg buffer frames sample-rate)))

(define (signal-play signal sample-rate)
  (player-channel-put (play-signal-msg signal sample-rate)))

(define (signal/block-play signal/block sample-rate)
  (player-channel-put (signal/block-play-msg signal/block sample-rate)))

(define (stop-playing)
  (player-channel-put (stop-playing-msg)))

;; this won't work, right now.
#;(define (change-loop buffer frames)
  (player-channel-put (change-loop-msg buffer frames)))



;; messages: play-sound, loop-sound, change-loop, stop
;; states: not playing, playing single, playing loop.

(struct player-msg ())
(struct play-sound-msg (buffer frames sample-rate) #:super struct:player-msg)
(struct loop-sound-msg (buffer frames sample-rate) #:super struct:player-msg)
(struct stop-playing-msg () #:super struct:player-msg)
(struct change-loop-msg (buffer frames) #:super struct:player-msg)
(struct play-signal-msg (signal sample-rate) #:super struct:player-msg)
(struct signal/block-play-msg (signal/block sample-rate) #:super struct:player-msg)



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




(define (idle-state message)
  (define next-evt
    (or 
     (match message
       ;; ignore stop-playing message if we're already stopped:
       [(struct stop-playing-msg ()) #f]
       
       [(struct play-sound-msg (buffer frames sample-rate))
        (playing-state (make-copying-callback frames buffer)
                       sample-rate)]
       
       #;[(struct loop-sound-msg (buffer frames sample-rate))
          ;; must pass along loop? somehow... or take this message out.
        (playing-state (make-copying-callback frames buffer)
                       sample-rate)]
       
       [(struct play-signal-msg (signal sample-rate))
        (playing-state (make-generating-callback signal)
                       sample-rate)]
       
       [(struct signal/block-play-msg (signal/block sample-rate))
        (playing-state (make-block-generating-callback signal/block)
                       sample-rate)]
       
       ;; ignore change-loop message if we're not playing:
       [(struct change-loop-msg (buffer frames)) #f]
       
       [other
        (error 'start-player-thread "not a player message: ~e" other)])
     (channel-get player-evt-channel)))
  (idle-state next-evt))


;; create a stream connected to the given callback, and start it up. 
;; Wait for a response or a new message, then shut it down.
(define (playing-state callback-maker sample-rate)
  (define response-channel (make-channel))
  (define callback (callback-maker response-channel))
  (pa-initialize)
  (let* ([stream (pa-open-default-stream
                  0             ;; input channels
                  channels      ;; output channels
                  'paInt16      ;; sample format
                  (exact->inexact sample-rate)  ;; sample rate
                  1000 ;;frames-per-buffer  ;; frames per buffer
                  callback     ;; callback (NULL means just wait for data)
                  #f)])         ;; user data (unnecessary in a world with closures))
    (dynamic-wind
     void
     (lambda ()
       (pa-start-stream stream)
       ;; block until getting a message on *either* the response
       ;; channel or the command channel:
       (let loop ()
         (let ([response (sync player-evt-channel response-channel)])
           (match response 
             [(? play-sound-msg? p) (begin (pa-stop-stream stream)
                                           p)]
             [(? loop-sound-msg? p) (begin (pa-stop-stream stream)
                                           p)]
             [(? stop-playing-msg? p) (begin (pa-stop-stream stream)
                                             #f)]
             [(? change-loop-msg? p) 
              ;; to get loops working, send a message to the callback.
              (loop)]
             [(? play-signal-msg? p) (begin (pa-stop-stream stream)
                                            p)]
             [(? exn? e) (begin (pa-stop-stream stream)
                                (raise e))]
             ['finished (begin (pa-stop-stream stream) #f)]))))
     (lambda () 
       (pa-close-stream stream)
       (pa-terminate)))))



(define (init-player-thread)
  (log-debug "player event handler started")
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     ;; recover gracefully by logging the message and continuing
                     (log-error 
                      (format "play-thread exception: ~a" (exn-message exn)))
                     (fprintf (current-error-port) "~a\n" (exn-message exn))
                     (idle-state (channel-get player-evt-channel)))])
    (idle-state (channel-get player-evt-channel))))

;; start the thread:
(define player-thread (thread init-player-thread))