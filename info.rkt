#lang setup/infotab

(define name "RSound")

(define blurb '((p "RSound provides a framework for manipulating "
                   "and playing sounds using the portaudio "
                   "library. It runs on Windows, Mac OS X, and "
                   "linux.")))

(define scribblings '(("rsound.scrbl" () (gui-library))))
(define categories '(media))
(define version "2012-10-12-16:21")
(define release-notes '((p "complete overhaul of signals")))

;; don't compile the stuff in the contrib subdirectory.
(define compile-omit-paths '("contrib"))

;; planet-specific:
(define repositories '("4.x"))
(define primary-file "main.rkt")

#;(define homepage "http://schematics.sourceforge.net/")
#;(define url "http://schematics.sourceforge.net/")

