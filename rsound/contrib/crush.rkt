#lang racket

;; crunch by Mustafa Khafateh
;; cleanup & quantize by John Clements

(require "../main.rkt")
(require "../draw.rkt")


(define (vector->rsound v)
  (signal->rsound (vector-length v) 44100
                    (lambda (t) (vector-ref v t))))

;(rsound-draw (vector->rsound chip1))

; ratio is how much the sample rate is divided by
(define (crush-helper t rsound ratio)
  (rs-ith/left rsound (* ratio (quotient t ratio))))

(define (crunch rsound ratio)
  (signal->rsound (rs-frames rsound) 44100
                  (network ()
                    [i <= simple-ctr 0 1]
                    [out = (crush-helper i rsound ratio)])))


#;(play (time (crunch (rsound-read "/tmp/horizon2.wav") 50)))

; test on small rsound
;(rsound-draw (crunch (vector->rsound chip1) 2))

; slide from rs/r1 to rs/r2 in nseconds
(define (crush-slide rsound r1 r2 nseconds)
  (signal->rsound (rs-frames rsound) 44100
                    (let ([factor (/ (- r2 r1) (* nseconds (rsound-sample-rate rsound)))])
                      (lambda (t)
                        (let ([ratio (+ (* t factor) r1)])
                          (rs-ith/left 
                           rsound
                           (floor (* ratio (floor (/ t ratio))))))))))

(define input (rs-read "/tmp/horizon2.wav"))


(define (quantize rsound q1)
  (signal->rsound (rs-frames rsound) 44100
                    (lambda (t)
                      (* (/ 1 q1) (floor (* q1 (rs-ith/left rsound t)))))))

; decrease apparent sample rate from 44100/10 to 44100/50 over 4 seconds
(play (rs-append* (list (time (crush-slide input 10 50 4))
                                   (time (quantize input 4)))))

