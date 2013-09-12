#lang racket

;; hastily assembled demo for Foaad Khosmood's class

(require rsound
         rsound/paste-util
         ffi/unsafe
         ffi/vector)

(provide go
         update-q!
         update-time-q!)

;; go : (listof rsound) -> (void)
(define (go sound)
  (set! stored-snd sound)
  (signal/block-play/unsafe copier 44100))

(define (update-q! num)
  (define new-val (vector-ref quantize-vec num))
  (printf "quantized to ~s levels\n" (exact->inexact (/ 65536 new-val)))
  (set! quantize new-val)
  (print-stats))

(define (update-time-q! num)
  (define new-val (vector-ref time-quantize-vec num))
  (printf "quantized to sample rate of ~s\n" (exact->inexact (/ 44100 new-val)))
  (set! time-quantize new-val)
  (print-stats))

(define (print-stats)
  (define levels (/ 65536 quantize))
  (define sr (/ 44100 time-quantize))
  (define bytes-per-sec (* sr (/ (/ (log levels) (log 2)) 8)))
  (printf "bytes per second: ~s\n" (exact->inexact bytes-per-sec)))

;; update-play-toggles : (vectorof boolean) -> void
#;(define (update-play-toggles! new-vec)
  (unless (= (vector-length new-vec)
             (length stored-snds))
    (error 'update-play-toggles "wrong length vector, must be ~s"
           (length stored-snds)))
  (set! sound-toggles new-vec))

;; toggle-loop! : number -> void
;; turn on or off a particular loop
#;(define (toggle-loop! n)
  (define entry-box (vector-ref (vector-ref snd-table n) 1))
  (set-box! entry-box (not (unbox entry-box))))

(define stored-snd #f)
(define quantize 1)
(define time-quantize 1)
(define frames-offset 0)

(define quantize-vec '#(1 4 16 64 256 1024 4096 8192 16384 32768))
(define time-quantize-vec '#(1 4 16 64 128 256 512 1024))



(define (copier buf len frame)
  (define snd-vec (rsound-data stored-snd))
  (define snd-len (rs-frames stored-snd))
  (define frame-to-start-copying (- frame frames-offset))
  (define frame-to-stop-copying (- (+ frame-to-start-copying len) frames-offset))
  (cond [(< snd-len frame-to-stop-copying)
         ;; go back to beginning: 
         (set! frames-offset frame)
         ;; ... and try again
         (copier buf len frame)]
        [else
         (for* ([i (in-range (* 2 len))])
           (define j (+ i (* 2 (- frame frames-offset))))
           (define qj (inexact->exact (* time-quantize (floor (/ j time-quantize)))))
           (define samp (inexact->exact 
                         (min (max -32768 (* quantize (round (/ (s16vector-ref snd-vec qj) quantize))))
                              32767)))
           (ptr-set! buf _sint16 i samp))]))


