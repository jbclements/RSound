#lang setup/infotab

(define collection 'multi)

(define deps (list (list "portaudio" "0.1") 
                   "base"
                   "data-lib"
                   "gui-lib"
                   "htdp-lib"
                   "math-lib"
                   "plot-gui-lib"
                   "rackunit-lib"
                   "typed-racket-lib"
                   ))
(define build-deps (list "scribble-lib"))
