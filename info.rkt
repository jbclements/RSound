#lang setup/infotab

(define collection 'multi)

(define deps (list ("portaudio" '#:version "0.1")
                   ;; unfortunately, specifying core dependencies doesn't work in 5.3.6.... even
                   ;; though the catalog supplies dummy versions, the
                   ;; graphical tool pops up a Y/N message in a dialog
                   ;; that has no means of entering text!
                   ;"base"
                   ;"data-lib"
                   ;"gui-lib"
                   ;"htdp-lib"
                   ;"plot-gui-lib"
                   ;"rackunit-lib"
                   ;"scheme-lib"
                   ;"typed-racket-lib"
                   ))
;(define build-deps (list "racket-doc"
;                         "scribble-lib"))
