#lang racket/base


;; untested: error clauses

(require ffi/vector
         racket/contract
         racket/match
         racket/bool)

(provide
 (contract-out
  [read-sound/s16vector (-> path-string? exact-integer? (or/c exact-integer? false?) 
                            (list/c s16vector? exact-integer?))]
  [read-sound/formatting (-> path-string? 
                             (list/c exact-integer? exact-integer?))]))


;; read-sound/s16vector : file-string nat (or/c #f nat) -> (list/c s16vector nat nat)
;; given a file-string, a beginning frame, and an ending frame (or #f),
;; return the data, the number of frames, and the sample-rate
(define (read-sound/s16vector p begin-frame end-frame)
  (call-with-input-file* p
    (lambda (port)
      (parse-main-chunk port begin-frame end-frame))))

;; given a path-string, return the number of frames in the file and the sample rate
;; file-path -> (list/c nat nat)
(define (read-sound/formatting p)
  (call-with-input-file* p
    (lambda (port)
      (parse-main-chunk/info port))))

(define global-channels 2)
(define global-bitspersample 16)
(define global-bytespersample (* global-bitspersample 1/8))
(define global-samplemax (exact->inexact #x8000))

(when (not (integer? global-bytespersample))
    (error 'parse-main-chunk/info "expected multiple of 8 bits per second, got ~a" global-bitspersample))


(struct chunk (id len body-offset))
(struct formatchunk (id len body-offset channels samplerate))

;; parse the file to obtain the number of frames and the sample rate
(define (parse-main-chunk/info port)
  (match-define (list next-chunk-offset channels samplerate) (read-formatting-info port 0))
  (match-define (list data-chunk-len data-offset) (scan-for-data-chunk port next-chunk-offset))
  (define frames-in-file (/ data-chunk-len (* channels global-bytespersample)))
  (when (not (integer? frames-in-file))
    (error 'parse-main-chunk/info "expected integer number of frames, got: ~s"
           frames-in-file))
  (list frames-in-file samplerate))

;; port nat nat (or/c nat #f) -> (list/c s16vector? nat nat)
(define (parse-main-chunk port begin-frame end-frame)
  (match-let* ([(list next-chunk-offset channels samplerate) (read-formatting-info port 0)]
               [(list data-chunk-len data-offset) (scan-for-data-chunk port next-chunk-offset)])
    (match-let ([(list data frames) (parse-data-chunk port data-chunk-len data-offset channels begin-frame end-frame)])
      (list data samplerate))))

;; read-formatting-info : port nat -> nat nat nat 
;; parse the header of the file, return the offset of the next
;; chunk after the format, the number of channels, and the sample rate.
(define (read-formatting-info port offset)
  (match-let* ([(struct chunk (main-id main-len main-offset)) (bytes->chunk port offset)]
               [_1 (unless (equal? main-id #"RIFF")
                     (error 'read-wav "not a WAV file, didn't start with #\"RIFF\""))]
               [format-bytes (bytes-from-posn port main-offset 4)]
               [_2 (unless (equal? format-bytes #"WAVE")
                     (error 'read-wav "not a WAV file, didn't contain the format string #\"WAVE\""))]
               [(struct formatchunk (dc1 format-len dc3 channels samplerate)) (parse-format-chunk port (+ main-offset 4))]
               [format-chunk-len (+ 8 format-len)]
               [next-chunk-offset (+ main-offset 4 format-chunk-len)])
    (list next-chunk-offset channels samplerate)))

;; scan-for-data-chunk : keep searching forward until we find a chunk with id #"data"
;; port nat -> (list/c nat nat)
(define (scan-for-data-chunk port offset)
  (let search-for-data-loop ([search-offset offset])
    (match-let ([(struct chunk (id len chunk-offset)) (bytes->chunk port search-offset)])
      (match id
        [(or #"PAD " #"LIST") (search-for-data-loop (+ search-offset 4 4 len))] 
        [#"data" (list len chunk-offset)]
        ;; best just to warn and move on... :
        [other (fprintf (current-error-port) "ignoring unknown chunk with id ~s" other)
               (search-for-data-loop (+ search-offset 4 4 len))]))))


;; parse-format-chunk: port nat -> formatchunk
;; read the format chunk
(define (parse-format-chunk port offset)
  (match-let* ([(struct chunk (id len format-offset)) (bytes->chunk port offset)])
    (unless (equal? id #"fmt ")
      (error 'parse-format-chunk "expected #\"fmt \" chunk here, got ~v" id))
    (unless (>= len 16)
      (error 'parse-format-chunk "format chunk was only ~v bytes long. I expected at least 16 bytes" len))
    (match-let ([(list dc audioformat/b numchannels/b samplerate/b byterate/b blockalign/b bitspersample/b)
                 (regexp-match #rx#"^(..)(..)(....)(....)(..)(..)" (bytes-from-posn port format-offset 16))])
      (let ([audioformat (integer-bytes->integer audioformat/b #f #f)]
            [numchannels (integer-bytes->integer numchannels/b #f #f)]
            [samplerate (integer-bytes->integer samplerate/b #f #f)]
            [byterate (integer-bytes->integer byterate/b #f #f)]
            [blockalign (integer-bytes->integer blockalign/b #f #f)]
            [bitspersample (integer-bytes->integer bitspersample/b #f #f)])
        (unless (= audioformat 1)
          (error 'parse-format-chunk "audio format is ~v rather than 1, indicating a non-PCM file. I give up." 
                 audioformat))
        (unless (member numchannels '(1 2))
          (error 'parse-format-chunk "this file contains ~v channels, rather than 1 or 2.  I give up." 
                 numchannels))
        (unless (= bitspersample global-bitspersample)
          (error 'parse-format-chunk "this file contains ~v-bit samples, rather than ~v. I give up."
                 bitspersample global-bitspersample))
        (unless (= byterate (* samplerate numchannels bitspersample 1/8))
          (error 'parse-format-chunk "byte rate ~v doesn't match expected ~v"
                 byterate (* samplerate numchannels bitspersample 1/8)))
        (unless (= blockalign (* numchannels bitspersample 1/8))
          (error 'parse-format-chunk "blockalign ~v doesn't match expected ~v"
                 blockalign (* numchannels bitspersample 1/8)))
        (formatchunk #"fmt " len format-offset numchannels samplerate)))))

;; parse-data-chunk : port nat nat nat nat nat -> (list/c s16vector nat)
;; read the desired data from the data chunk
(define (parse-data-chunk port data-len data-offset channels begin-frame end-frame)
  (match-let* ([frames-in-file (/ data-len (* channels global-bytespersample))]
               [end-frame/real (or end-frame frames-in-file)])
    (unless (integer? frames-in-file)
      (error 'parse-data-chunk "data chunk contains a non-integer number of frames: ~s" frames-in-file))
    (unless (<= 0 begin-frame frames-in-file)
      (error 'parse-data-chunk "requested begin frame ~v is not between 0 and ~v, the number of frames in the file"
             begin-frame frames-in-file))
    (unless (<= begin-frame end-frame/real)
      (error 'parse-data-chunk "requested begin frame ~v is not before requested end frame ~v"
             begin-frame end-frame/real))
    (unless (<= end-frame/real frames-in-file)
      (error 'parse-data-chunk "requested end frame ~v is larger than ~v, the number of frames in the file"
             end-frame/real frames-in-file))
    
    (let* ([frames-to-read (- end-frame/real begin-frame)]
           [samples-to-read (* frames-to-read channels)]
           [cblock (make-s16vector (* global-channels frames-to-read))]
           [in-bytes (bytes-from-posn port 
                                      (+ data-offset (* global-bytespersample channels begin-frame))
                                      (* global-bytespersample channels frames-to-read))])
      (cond [(= channels 2)
             (for ([j (in-range (* 2 frames-to-read))])
               (define i (* global-bytespersample j))
               (s16vector-set! cblock j (integer-bytes->integer (subbytes in-bytes i (+ i 2)) #t #f)))]
            [(= channels 1)
             (for ([j (in-range 0 (* 2 frames-to-read) 2)] ;; write into stereo buffer, duplicating samples
                   [i (in-range 0 (* global-bytespersample samples-to-read) global-bytespersample)])
               (let ([sample (integer-bytes->integer (subbytes in-bytes i (+ i 2)) #t #f)])
                 (s16vector-set! cblock j sample)
                 (s16vector-set! cblock (+ j 1) sample)))]
            [else (error 'parse-data-chunk "this package only handles 1- or 2-channel .wav files")])
      (list cblock frames-to-read))))


;; given a port and an offset, read the chunk info starting at that offset.
(define (bytes->chunk port offset)
  (let* ([bytes-in (bytes-from-posn port offset 8)]
         [id (subbytes bytes-in 0 4)]
         [len (integer-bytes->integer (subbytes bytes-in 4 8) #f #f)])
    (chunk id len (+ offset 8))))



;; bytes-from-posn : port nat nat -> bytes
;; read a piece from a file
(define (bytes-from-posn port offset len)
  (file-position port offset)
  (match (read-bytes len port)
    [(? bytes? b) 
     (when (< (bytes-length b) len)
       (error 'bytes-from-posn ".wav file too short. tried to read ~s bytes, got only ~s"
              len (bytes-length b)))
     b]
    [(? eof-object? e) 
     (error 'bytes-from-posn "no data available at file offset ~s" offset)]))