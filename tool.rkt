#lang racket/base


#;(require scheme/class
         drscheme/tool
         mred
         mzlib/pconvert
         string-constants
         (prefix-in frame: framework) 
         mrlib/switchable-button
         (file "private/my-macros.ss")
         (prefix-in x: "private/mred-extensions.ss")
         "private/shared.ss"
         lang/stepper-language-interface
         scheme/pretty
         "xml-sig.ss"
         "drracket-button.ss")

(require drscheme/tool
         racket/gui/base
         racket/unit
         racket/class
         (prefix-in drlink: "private/drracket-link.rkt")
         #;(prefix-in drlink: (planet "drracket-link.rkt" ("clements" "rsound.plt") "private"))
         )

;; most of this comes from Ryan Culpepper's tool.ss file for rackunit.

(provide tool@)

(define LINK-MODULE-SPEC
  '(planet "drracket-link.rkt" ("clements" "rsound.plt") "private"))

(define-namespace-anchor drracket-ns-anchor)

;; close/eventspace : (a* -> b) -> (a* -> b)
;; Returns a procedure that executes the procedure in the 
;; eventspace current when close/eventspace was executed.
;; Effectively, "close" the procedure in the current eventspace.
(define (close-eventspace f)
  (let ([es (current-eventspace)])
    (lambda args
      (parameterize [(current-eventspace es)]
        (apply f args)))))

(define (close-eventspace/async f)
  (let ([es (current-eventspace)])
    (lambda args
      (parameterize ((current-eventspace es))
        (queue-callback (lambda () (apply f args)))))))


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
                                   (get-user-namespace)))

        (define/override (reset-console)
          (super reset-console)
          (setup-helper-module))))

    (drscheme:get/extend:extend-interactions-text interactions-text-mixin)
    
    (define (phase1) (void))
    (define (phase2) (void))
    
    ))
