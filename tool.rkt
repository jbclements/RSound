#lang racket/base

(require drscheme/tool
         racket/unit
         racket/class
         (prefix-in drlink: "private/drracket-link.rkt")
         ffi/unsafe/cvector-def
         )

;; most of this comes from Ryan Culpepper's tool.ss file for rackunit.

(provide tool@)

(define LINK-MODULE-SPEC
  '(planet "drracket-link.rkt" ("clements" "rsound.plt") "private"))

(define-namespace-anchor drracket-ns-anchor)

(define tool@
  (unit
    (import drscheme:tool^)
    (export drscheme:tool-exports^)


    
    ;; Send them off to the drscheme-ui module.
    ;; We'll still have to attach our instantiation of drscheme-link
    ;; to the user namespace.

    (define drracket-ns (namespace-anchor->namespace drracket-ns-anchor))

    (define interactions-text-mixin
      (mixin ((class->interface drscheme:rep:text%)) ()
        (inherit get-user-namespace)
        (super-new)

        (define/private (setup-helper-module)
          (namespace-attach-module drracket-ns
                                   LINK-MODULE-SPEC
                                   (get-user-namespace))
          (namespace-attach-module drracket-ns
                                   'ffi/unsafe/cvector-def
                                   (get-user-namespace)))
          

        (define/override (reset-console)
          (super reset-console)
          (setup-helper-module))))

    (drscheme:get/extend:extend-interactions-text interactions-text-mixin)
    
    (define (phase1) (void))
    (define (phase2) (void))
    
    ))
