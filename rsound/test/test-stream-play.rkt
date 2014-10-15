#lang racket

(require "../main.rkt"
         rackunit
         rackunit/text-ui)

(run-tests
(test-suite 
 "stream-play"
 
(let ()
 (define ps (make-pstream))
 (define t (make-tone 441 0.2 22000))
 (sleep 0.5)
 (define now_0 (pstream-current-frame ps))
 (check-true (< 0 now_0))

 (printf "one ding.\n")
 (pstream-play ps ding)
 (sleep 1)

 (define now_1 (pstream-current-frame ps))
 (check-true (< (+ 22050 now_0) now_1))
 
 (printf "tone should be uninterrupted, 2 seconds long.\n")
 (pstream-queue ps t now_1)
 (pstream-queue ps t (+ now_1 22000))
 (pstream-queue ps t (+ now_1 44000))
 (pstream-queue ps t (+ now_1 66000))

 (sleep 4.0))

(let ()
  (printf "tone should last for 2 seconds then rise from 440 to 500 Hz\n")
  (define ps (make-pstream))
  (define pitch 440)
  (define lead-time 1000)
  (define (make-callback tenths)
    (lambda ()
      (define next-start (* tenths 4410))
      (pstream-queue ps (make-tone pitch 0.1 4410) next-start)
      (pstream-queue-callback ps (make-callback (add1 tenths))
                              (- next-start lead-time))))
  (pstream-queue-callback ps (make-callback 1) (- 4410 lead-time))
  (sleep 1.5)
  (set! pitch 500)
  (sleep 2.0)
  (stop))

(let ()
  (printf "tone should last for 2 seconds then get quieter then louder again\n")
  (define ps (make-pstream))
  (define pitch 440)
  (pstream-queue ps (make-tone pitch 0.3 (* 4 44100)) 0)
  (pstream-queue-callback ps
                          (lambda ()
                            (pstream-set-volume! ps 0.1))
                          (* 2 44100))
  (pstream-queue-callback ps
                          (lambda ()
                            (pstream-set-volume! ps 1/2))
                          (* 3 44100))
  (sleep 4.0)
  (stop))



(let ()
  (define 22ksound (resample-to-rate 22050 (make-tone 440 0.2 22050)))
  (check-equal? (rsound-sample-rate 22ksound) 22050)
  (define ps (make-pstream))
  (check-exn (lambda (exn)
               (regexp-match #px"rsound matching pstream's frame rate"
                             (exn-message exn)))
             (lambda () (pstream-queue ps 22ksound 0))))

(let ()
  (define sub-cust (make-custodian))

  ;; start a stream playing under a fresh
  ;; custodian
  (parameterize ([current-custodian sub-cust])
    (thread
     (lambda ()
       (signal-play
        (indexed-signal (lambda (x) 0.0)))
       (sleep 10)
       )))

  ;; now kill the custodian
  (sleep 2)
  (printf "calling custodian-shutdown-all\n")
  (sleep 0.1)
  ;; this should not crash... 
  (custodian-shutdown-all sub-cust) 
)
))