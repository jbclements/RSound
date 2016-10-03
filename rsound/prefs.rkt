#lang typed/racket/base

;; manage the default-frame-rate pref

(require racket/file)

(require/typed framework/preferences
               [preferences:set-default
                (Symbol Any (Any -> Boolean) -> Void)]
               [preferences:get
                (Symbol -> Any)]
               [preferences:set
                (Symbol Any -> Void)])

(provide get-fr-pref
         set-fr-pref!
         LEGAL-DEFAULT-FRS)

;; the list of permissible default frame rates.
;; This is used for generation of all built-in sounds, and is the
;; default for pstreams and in other places. You can change it with
;; the default-frame-rate parameter, but this won't affect the sounds
;; that are already generated.
;; For ease of use, we limit this setting to one of a few possibilities.
;; there's really no reason not to allow anything between 8K and 48K except
;; that it would confuse people. I don't know whether 96K would present
;; problems... it might slow down sound generation unacceptably.
(: LEGAL-DEFAULT-FRS (Listof Real))
(define LEGAL-DEFAULT-FRS '(44100 48000))

;; if the existing preference is not one of the legal ones, reset to this:
(: RESET-FR Real)
(define RESET-FR 44100)

(unless (member RESET-FR LEGAL-DEFAULT-FRS)
  (error 'rsound-prefs
         "internal error: default fr pref (~e) is not one of the legal ones: ~e"
         RESET-FR LEGAL-DEFAULT-FRS))

;; the name used in the prefs file to associated the FR with:
(define PREF-NAME 'rsound:default-frame-rate)

(preferences:set-default
 PREF-NAME
 RESET-FR
 (λ (v)
   (and (real? v)
        ;; coerce non-#f to #t:
        (not (not ((inst member Any Any)
                   v LEGAL-DEFAULT-FRS))))))


;; get the value of the pref.
(: get-fr-pref (-> Real))
(define (get-fr-pref)
  (cast (preferences:get PREF-NAME) Real))

;; set the fr pref stored in the file.
;; WARNING: calling this outside of the preferences panel will
;; mean that the value of the choice box no longer matches the
;; value stored in the preferences file.
(: set-fr-pref! (Real -> Void))
(define (set-fr-pref! fr)
  (unless (member fr LEGAL-DEFAULT-FRS)
    (raise-argument-error 'set-fr-pref!
                          (format "frame rate from legal list (~e)"
                                  LEGAL-DEFAULT-FRS)
                          0 fr))
  (preferences:set PREF-NAME
                   fr))