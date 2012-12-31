#lang racket

(require rackunit)

(provide parse-file)

(struct chunk (id len body-offset) #:transparent)
(struct header-info (format num-tracks time-division)
  #:transparent)

;; given a path, parse the file into a list containing
;; the MIDI format, the time division, and a list of 
;; tracks, where a track contains a list of time/message
;; lists
(define (parse-file path)
  (define p (open-input-file path))
  (define chunks (port->chunks p))
  (define header-info (parse-header (first chunks) p))
  (unless (= (header-info-num-tracks header-info) (length (rest chunks)))
    (error 'parsing "wrong number of tracks"))
  (define tracks (map (parse-chunk p) (cdr chunks)))
  (close-input-port p)
  (list (header-info-format header-info)
        (header-info-time-division header-info)
        tracks))

;; pick out all of the chunk locations in a file.
(define (port->chunks port)
  (let loop ([offset 0])
    (match (bytes->chunk port offset)
      [#f empty]
      [chunk 
       (cons chunk (loop (+ (chunk-body-offset chunk)
                            (chunk-len chunk))))])))


(define header-len 6)

;; given a header chunk and a port, produce the header-info
(define (parse-header h-chunk port)
  (match h-chunk
    [(chunk #"MThd" 6 header-offset)
     (define header-bytes (bytes-from-posn port header-offset 6))
     (define format 
       (match (integer-bytes->integer (subbytes header-bytes 0 2) #f #t)
         [0 'single]
         [1 'multi]
         [2 'sequential]))
     (define num-tracks
       (integer-bytes->integer (subbytes header-bytes 2 4) #f #t))
     (define division-word
       (integer-bytes->integer (subbytes header-bytes 4 6) #f #t))
     (define division
       (cond [(= 0 (bitwise-and #x8000 division-word))
              (list 'ticks-per-quarter 
                    (bitwise-and #x7ffff division-word))]
             [else
              (list 'time-code
                    (bitwise-and 
                     #x7f 
                     (arithmetic-shift division-word -8))
                    (bitwise-and #xff division-word))]))
     (header-info format num-tracks division)]))

;; given a port and a chunk, parse the messages in the chunk
(define ((parse-chunk port) a-chunk)
  (parse-messages port (chunk-body-offset a-chunk)
                  (chunk-len a-chunk)))

;; given a port, an offset, and a length in bytes, parse
;; the messages contained in the file at that location
(define (parse-messages port offset len)
  (file-position port offset)
  (define stop-offset (+ offset len))
  (let loop ([prior-event-type-byte #f] [time 0])
    (cond [(<= stop-offset (file-position port))
           empty]
          [else 
           (match-define (list new-time byte message)
             (parse-1-message port prior-event-type-byte time))
           (cons (list new-time message) (loop byte new-time))])))

;; given a port, a prior event-type-byte, and a prior time,
;; return a list containing the new time, the new event-type-byte,
;; and the new message. The second of these is necessary to support
;; the "and another of the same" style of message.
(define (parse-1-message port prior-event-type-byte prior-time)
  (define time-offset (read-variable-length port))
  (define new-time (+ prior-time time-offset))
  (match-define (list event-type-byte message)
    (match (read-byte port)
      [#xf0 (list 
             #f
             (list
              'sysex
              (len-and-bytes port)))]
      [#xff (list 
             #f
             (list 
              'meta
              (match (read-byte port)
                ;; just doing the ones I see....
                [#x01 (parse-text-meta 'text port)]
                [#x02 (parse-text-meta 'copyright-notice port)]
                [#x03 (parse-text-meta 'sequence/track-name port)]
                [#x2f (len-and-bytes port)
                      'end-of-track]
                [#x51 (define content (len-and-bytes port))
                      (list 'set-tempo 
                            (integer-bytes->integer
                             (bytes-append (bytes #x00)
                                           content)
                             #f #t))]
                [#x58 (define content (len-and-bytes port))
                      (list 'time-signature
                            (bytes->list content))]
                [#x59 (define content (len-and-bytes port))
                      (define flats/sharps-byte (subbytes content 0 1))
                      (define major/minor
                        (match (bytes-ref content 1)
                          [#x00 'major]
                          [#x01 'minor]))
                      (list 'key-signature
                            flats/sharps-byte
                            major/minor)]
                [other
                 (list 'unknown other (len-and-bytes port))]
                )))]
      [midi-evt 
       (cond [(not (= 0 (bitwise-and #x80 midi-evt)))
              ;; new message
              (define channel (bitwise-and #x7 midi-evt))
              (define message-nibble (arithmetic-shift midi-evt -4))
              (define message-kind (bits->event-type message-nibble))
              (define parameter-1 (read-byte port))
              (define parameter-2 
                (cond [(two-byte-event? message-nibble)
                       (read-byte port)]
                      [else #f]))
              (list midi-evt
                    (list message-kind
                          channel
                          parameter-1
                          parameter-2))]
             [else
              ;; running status , the midi-evt was actually parameter 1.
              (define channel (bitwise-and #x7 prior-event-type-byte))
              (define message-nibble (arithmetic-shift 
                                      prior-event-type-byte -4))
              (define message-kind (bits->event-type message-nibble))
              (define parameter-1 midi-evt)
              (define parameter-2 
                (cond [(two-byte-event? message-nibble)
                       (read-byte port)]
                      [else #f]))
              (list prior-event-type-byte
                    (list message-kind
                          channel
                          parameter-1
                          parameter-2))
              ])]))
  (list new-time event-type-byte message))

(define (two-byte-event? bits)
  (not (or (= bits #xc) (= bits #xd))))

(define (bits->event-type bits)
  (match bits
    [#x8 'note-off]
    [#x9 'note-on]
    [#xa 'aftertouch]
    [#xb 'control-change]
    [#xc 'program-change]
    [#xd 'channel-aftertouch]
    [#xe 'pitch-bend]))

(define (parse-text-meta tag port)
  (list tag (len-and-bytes port)))

;; read a length and that number of bytes from a port
(define (len-and-bytes port)
  (define len (read-variable-length port))
  (read-bytes len port))


;; read a variable-length quantity, advance the port
;; if these can be negative, I'm worried.
(define (read-variable-length port)
  (let loop ([so-far #x00])
    (define next-byte (read-byte port))
    (define new-so-far (bitwise-ior 
                        (arithmetic-shift so-far 7)
                        (bitwise-and #x7f next-byte)))
    (cond [(not (= 0 (bitwise-and #x80 next-byte)))
           (loop new-so-far)]
          [else new-so-far])))

(check-equal? (read-variable-length
               (open-input-bytes (bytes #x00))) 0)
(check-equal? (read-variable-length 
               (open-input-bytes (bytes #x81 00))) #x80)
(check-equal? (read-variable-length
               (open-input-bytes (bytes #x81 #x80 #x80 #x00))) #x200000)


;; given a port and an offset, read the chunk info starting at that offset.
(define (bytes->chunk port offset)
  (match (bytes-from-posn port offset 8)
    [(? eof-object? e) #f]
    [bytes-in
     (let* ([id (subbytes bytes-in 0 4)]
            [len (integer-bytes->integer (subbytes bytes-in 4 8) #f #t)])
       (chunk id len (+ offset 8)))]))

;; bytes-from-posn : port nat nat -> bytes
;; read a piece from a file
(define (bytes-from-posn port offset len)
  (file-position port offset)
  (read-bytes len port))