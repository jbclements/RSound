#lang setup/infotab

(define collection 'multi)
(define deps (list "portaudio"
                   "base"
                   "data-lib"
                   "gui-lib"
                   "htdp-lib"
                   "plot-gui-lib"
                   "rackunit-lib"
                   "scheme-lib"
                   "typed-racket-lib"))
(define build-deps (list "racket-doc"
                         "scribble-lib"))
