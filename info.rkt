#lang setup/infotab

(define name "RSound")

(define blurb '((p "RSound provides a framework for manipulating "
                   "and playing sounds using the portaudio "
                   "library. It runs on Windows, Mac OS X, and "
                   "linux.")))

(define scribblings '(("rsound.scrbl" () (tool))))
(define tools '[("tool.rkt")])
(define tool-names '["RSound Global Control Master Unit Incorporated"])
(define categories '(media))
(define version "2010-10-14-06:36")
(define release-notes '((p "Allowed WAV files containing strange RIFF chunks, bug fixes")))

;; planet-specific:
(define repositories '("4.x"))
(define primary-file "main.rkt")

#;(define homepage "http://schematics.sourceforge.net/")
#;(define url "http://schematics.sourceforge.net/")

