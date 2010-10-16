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
(define version "2010-10-15-16:22")
(define release-notes '((p "clips rather than wrapping, uses radix-2 fft when possible, bug fixes")))

;; planet-specific:
(define repositories '("4.x"))
(define primary-file "main.rkt")

#;(define homepage "http://schematics.sourceforge.net/")
#;(define url "http://schematics.sourceforge.net/")

