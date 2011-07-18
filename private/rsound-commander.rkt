#lang racket

(require racket/class
         "portaudio.rkt"
         (only-in ffi/unsafe cpointer?))

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
                  (stop-playing (-> void?)))

;; messages: play-sound, loop-sound, change-loop, stop
;; states: not playing, playing single, playing loop.

(struct player-msg ())
(struct play-sound-msg (buffer frames sample-rate) #:super struct:player-msg)
(struct loop-sound-msg (buffer frames sample-rate) #:super struct:player-msg)
(struct stop-playing-msg () #:super struct:player-msg)
(struct change-loop-msg (buffer frames) #:super struct:player-msg)
(struct play-signal-msg (signal sample-rate) #:super struct:player-msg)

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


(define event-handler-thread
  (thread
   (lambda ()
     (log-debug "player event handler started")
     (let loop ([message (channel-get player-evt-channel)])
       (with-handlers ([exn:fail?
                        (lambda (exn)
                          ;; recover gracefully by logging the message and continuing
                          (log-error (format "play-thread exception: ~a" (exn-message exn)))
                          (fprintf (current-error-port) "~a\n" (exn-message exn))
                          (loop (channel-get player-evt-channel)))])
         (match message
           ;; ignore stop-playing message if we're already stopped:
           [(struct stop-playing-msg ()) (loop (channel-get player-evt-channel))]
           [(struct play-sound-msg (buffer frames sample-rate))
            (loop (or (play-buffer buffer frames sample-rate #f) (channel-get player-evt-channel)))]
           [(struct loop-sound-msg (buffer frames sample-rate))
            (loop (or (play-buffer buffer frames sample-rate #t) (channel-get player-evt-channel)))]
           [(struct play-signal-msg (signal sample-rate))
            (loop (or (play-signal signal sample-rate) (channel-get player-evt-channel)))]
           ;; ignore change-loop message if we're not playing:
           [(struct change-loop-msg (buffer frames)) 
            (loop (channel-get player-evt-channel))]
           [other
            (error 'start-player-thread "not a player message: ~e" other)]))))))

;; play the sound. 
(define (play-buffer buffer frames sample-rate loop?)
  (define response-channel (make-channel))
  (define callback (make-copying-callback frames buffer response-channel))
  (play-using-callback response-channel callback sample-rate))


;; play the signal.
(define (play-signal signal sample-rate)
  (define response-channel (make-channel))
  (define callback (make-generating-callback signal response-channel))
  (play-using-callback response-channel callback sample-rate))

;; create a stream connected to the given callback, and start it up. Wait for a response,
;; then shut it down.
(define (play-using-callback response-channel callback sample-rate)
 (pa-initialize)
  (let* ([stream (pa-open-default-stream 0             ;; input channels
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
             [(? change-loop-msg? p) ;; to get loops working, send a message to the callback.
              (loop)]
             [(? play-signal-msg? p) (begin (pa-stop-stream stream)
                                            p)]
             [(? exn? e) (begin (pa-stop-stream stream)
                                (raise e))]
             ['finished (begin (pa-stop-stream stream) #f)]))))
     (lambda () 
       (pa-close-stream stream)
       (pa-terminate)))))

(define (buffer-play buffer frames sample-rate)
  (player-channel-put (play-sound-msg buffer frames sample-rate)))

;; won't loop, right now.
(define (buffer-loop buffer frames sample-rate)
  (player-channel-put (loop-sound-msg buffer frames sample-rate)))

(define (signal-play signal sample-rate)
  (player-channel-put (play-signal-msg signal sample-rate)))

(define (stop-playing)
  (player-channel-put (stop-playing-msg)))

;; this won't work, right now.
#;(define (change-loop buffer frames)
  (player-channel-put (change-loop-msg buffer frames)))

