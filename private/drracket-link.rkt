;; Written in #%kernel to avoid adding any module-attachment
;; dependencies. Initialized by the DrRacket integration tool.

(module drracket-link '#%kernel
  (#%provide link init-semaphore)
           
  ;; this is a once-per-process init. Once the semaphore goes
  ;; down, it never goes up again.
  (define-values (init-semaphore) (make-semaphore 1))

  ;; Before initialization, the link contains drscheme's custodian.
  ;; the sound-player thread is associated with this custodian.
  ;; after initialization, it contains an rsound-commander% object that can
  ;; be used to play sounds

  (define-values (link) (box (current-custodian))))
