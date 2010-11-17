#lang racket

(require racket/class
         "portaudio.rkt"
         ffi/unsafe
         ffi/vector
         racket/async-channel)

(provide rsound-commander%)

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
  (define-values (abort-flag callback) (make-copying-callback frames buffer response-channel))
  (play-using-callback response-channel callback abort-flag sample-rate))


;; play the signal.
(define (play-signal signal sample-rate)
  (define response-channel (make-channel))
  (define-values (abort-flag callback) (make-generating-callback signal response-channel))
  (play-using-callback response-channel callback abort-flag sample-rate))

;; create a stream connected to the given callback, and start it up. Wait for a response,
;; then shut it down.
(define (play-using-callback response-channel callback abort-flag sample-rate)
  (set! current-stop-playing-box abort-flag)
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
             [(? exn? e) (begin (pa-stop-stream stream)
                                (raise e))]
             ['finished (begin (pa-stop-stream stream) #f)]
             ;; can we get rid of this mechanism?
             ['abort-flag (begin (pa-stop-stream stream) #f)]))))
     (lambda () (pa-close-stream stream)))))

;; set this box high to send a signal to the currently playing sound to stop.
(define current-stop-playing-box (box #f))

(define rsound-commander%
  (class object%
    
    (init master-custodian)
    
    (parameterize ([current-custodian master-custodian])
      (start-player-thread))
    
    (define/public (play-sound buffer frames sample-rate)
      (player-channel-put (play-sound-msg buffer frames sample-rate)))
    
    ;; won't loop, right now.
    (define/public (loop-sound buffer frames sample-rate)
      (player-channel-put (loop-sound-msg buffer frames sample-rate)))
    
    (define/public (play-signal signal sample-rate)
      (player-channel-put (play-signal-msg signal sample-rate)))
    
    (define/public (stop-playing)
      (set-box! current-stop-playing-box #t))
    
    ;; this won't work, right now.
    (define/public (change-loop buffer frames)
      (player-channel-put (change-loop-msg buffer frames)))
    
    (super-new)))

