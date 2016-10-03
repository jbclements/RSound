#lang racket/base

(require drracket/tool
         racket/unit
         racket/file
         racket/class
         racket/gui/base
         racket/match
         framework
         "prefs.rkt")
 
(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define (phase1)
      (define FR-pref (get-fr-pref))
      (define FR-pref-idx (for/first ([i (in-naturals)]
                                      [fr (in-list LEGAL-DEFAULT-FRS)]
                                      #:when (= FR-pref fr))
                            i))
      (preferences:add-panel
       "RSound"
       (λ (parent)
         (define vpanel
           (new vertical-panel% (parent parent) (alignment '(left top))))
         (new choice%
              [label "default frame rate"]
              [choices (map number->string LEGAL-DEFAULT-FRS)]
              [parent vpanel]
              [selection FR-pref-idx]
              [callback
               (λ (choicebox evt)
                 (when (equal? (send evt get-event-type) 'choice)
                   (match (send choicebox get-selection)
                     [#f (error 'choice-callbox
                                "internal error: no default frame rate selected (should be impossible?)")]
                     [n (set-fr-pref! (list-ref LEGAL-DEFAULT-FRS n))])))])
         vpanel)))
    (define (phase2) (void))))
