#lang setup/infotab

(define name "RSound")

(define blurb '((p "RSound provides a framework for manipulating "
                   "and playing sounds using the portaudio "
                   "library. It runs on Windows, Mac OS X, and "
                   "linux.")))

(define scribblings '(("rsound.scrbl" () (gui-library))))
(define categories '(media))

;; don't compile the stuff in the contrib subdirectory.
(define compile-omit-paths '("contrib"))
;; things written in non-module languages can't easily be
;; eliminated from testing.
(define test-omit-paths '("examples/katy-sliders.rkt"
                          "examples/markov-model.rkt"))
;; in order to add a sample-rate pref, we need to be a tool:
(define drracket-name "RSound")
(define drracket-tools (list (list "tool.rkt")))

