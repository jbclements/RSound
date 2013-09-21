#lang racket/base
;;#lang typed/racket

(require ffi/vector
         racket/match)
#;(require/typed ffi/vector
               [s16vector? (Any -> Bool)]
               [s16vector-length (S16Vector -> Nat)]
               [s16vector-ref (S16Vector Nat -> Nat)]
               [opaque ])

;; write-wav

;; there are probably 4-byte-aligned constraints hidden all over, here...

(provide write-sound/s16vector)

#;(define-type ChunkContent 
  (U (List Bytes (Listof Chunk))
     Bytes
     S16Vector))

;; a chunk is (chunk 4-byte-string chunk-content)
(struct chunk (id content))
(struct vector-chunk (vec start stop))
#;(struct: Chunk ([id : String] [content : ChunkContent]))

;; a chunk-content is either 
;; - (list/c byte-string (listof chunk)), or 
;; - a byte-string, or
;; - an (list/c s16vector number number)

;; constants shared with read-wav:

(define global-numchannels 2)
(define global-bitspersample 16)
(define global-bytespersample (* global-bitspersample 1/8))
(define global-blockalign (* global-numchannels global-bytespersample))
(define global-samplemax (exact->inexact #x8000))

;; write-sound/s16vector
(define (write-sound/s16vector data start stop sample-rate path)
  (call-with-output-file* path
    (lambda (port)
      (write-chunk (make-chunk (vector-chunk data start stop) sample-rate) port))
    #:exists 'truncate))

;; turn an rsound into a writable chunk
(define (make-chunk vec sample-rate)
  (chunk #"RIFF"
         (list #"WAVE"
               (list (make-format-chunk sample-rate)
                     (chunk #"data" vec)))))

(define (make-format-chunk sample-rate)
  (chunk #"fmt "
         (let ([audioformat (integer->integer-bytes 1 2 #f #f)]
               [numchannels (integer->integer-bytes global-numchannels 2 #f #f)]
               [samplerate (integer->integer-bytes sample-rate 4 #f #f)]
               [byterate (integer->integer-bytes (* sample-rate global-numchannels global-bitspersample 1/8) 4 #f #f)]
               [blockalign (integer->integer-bytes (* global-numchannels global-bitspersample 1/8) 2 #f #f)]
               [bitspersample (integer->integer-bytes global-bitspersample 2 #f #f)])
           (bytes-append audioformat numchannels samplerate byterate blockalign bitspersample))))

;; write a chunk to an output port
(define (write-chunk ch port)
  (match ch
    [(struct chunk (id content))
     (display id port)
     (display (integer->integer-bytes (content-display-len content) 4 #f #f) port)
     (write-chunk-content content port)]))

;; write content to an output port
(define (write-chunk-content content port)
  (match content
    [(list fmt-bytes (? list? elts)) (display fmt-bytes port)
                                     (for-each (lambda (chunk) (write-chunk chunk port)) elts)]
    [(? bytes? bs) (display bs port)]
    ;; this one could be much faster I think....
    [(vector-chunk vec start stop) 
     (for ([i (in-range start stop)])
       (define sample (* i global-numchannels))
       (display (integer->integer-bytes (s16vector-ref vec sample) 2 #t #f) port)
       (display (integer->integer-bytes (s16vector-ref vec (add1 sample)) 2 #t #f) port))]))

;; figure out how long a chunk will be in bytes
(define (chunk-display-len chunk)
  (+ 8 (content-display-len (chunk-content chunk))))

;; figure out how long content will be in bytes
(define (content-display-len content)
  (match content
    [(list fmt-bytes (? list? elts)) (+ (bytes-length fmt-bytes) (apply + (map chunk-display-len elts)))]
    [(? bytes? bs) (bytes-length bs)]
    [(vector-chunk vec start stop) (* global-bytespersample
                                      (* global-numchannels
                                         (- stop start)))]))
