#lang racket

(require "../rsound.rkt"
         "../fft.rkt")

(define sound-start (- 212930 1024))
(define sound-end 212930)
(define window-len 1024)

(define windows (floor (/ (- sound-end sound-start) window-len)))

(printf "windows: ~s\n" windows)

(define voice (rsound-read/clip "/Users/clements/class/examples/spoken-voice.wav" sound-start (+ sound-start (* windows window-len))))

(define v1 (build-vector (rsound-frames voice) (lambda (i) (rsound-ith/left/s16 voice i))))

(define vx (build-vector 1024 (lambda (i) 0)))

(fft-complex-radix2-inverse vx)

#;(time (for ([i (in-range windows)])
        (let* ([s (* i window-len)]
               [e (+ s window-len)])
          (let ([tempv (vector-copy v1 s e)])
            (fft-complex-radix2-forward tempv)
            (let-values ([(idx mag) (for/fold ([max-index 0]
                                               [max-val 0.0])
                                      ([j (in-range (/ window-len 2))])
                                      (if (< max-val (magnitude (vector-ref tempv j)))
                                          (values j (magnitude (vector-ref tempv j)))
                                          (values max-index max-val)))])
              (printf "max-index: ~s\n" idx)
              (printf "max mag: ~s\n" mag)
              (for ([j (in-range window-len)])
                (let ([ans (vector-ref tempv j)])
                  (printf "~s\n" ans)
                  ans)
                (unless (or (= j idx) (= j (- window-len idx)))
                  (vector-set! tempv j 0.0)))
              (fft-complex-radix2-inverse tempv))))))


#;(signal->rsound (vector-length v1) 44100 (lambda (i) (/ (real-part (vector-ref v1 i)) s16max)))

#;(rsound-play (signal->rsound (vector-length v1) 44100 (lambda (i) (/ (real-part (vector-ref v1 i)) s16max))))




#;(rsound-play voice)

#;(rsound-draw voice)