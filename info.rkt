#lang setup/infotab

(define name "RSound")

(define blurb '((p "RSound provides a framework for manipulating "
                   "and playing sounds using the portaudio "
                   "library. It runs on Windows, Mac OS X, and "
                   "linux.")))

(define scribblings '(("rsound.scrbl" () (tool))))
(define categories '(media))
(define version "2011-09-21-09:30")
(define release-notes '((p "new back-end C interaction, should be more stable")))

;; don't compile the stuff in the contrib subdirectory.
(define compile-omit-paths '("contrib"))

;; planet-specific:
(define repositories '("4.x"))
(define primary-file "main.rkt")

#;(define homepage "http://schematics.sourceforge.net/")
#;(define url "http://schematics.sourceforge.net/")

