#lang setup/infotab

(define collection 'multi)

;; unfortunately, this doesn't work in 5.3.6.... even
;; though the catalog supplies dummy versions, the
;; graphical tool pops up a Y/N message in a dialog
;; that has no means of entering text!
(define deps (list "portaudio"
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
