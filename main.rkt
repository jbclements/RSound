#lang racket

(require (prefix-in link: "private/drracket-link.rkt")
         "private/portaudio.rkt"
         "rsound.rkt"
         "util.rkt")

;; if init hasn't happened, do it now.
(when (semaphore-try-wait? link:init-semaphore)
  (pa-initialize))


(provide (all-from-out "rsound.rkt")
         (all-from-out "util.rkt"))