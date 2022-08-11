#lang racket

;; abstraction is weak here...

(define archs '(x86_64 i386 aarch64))




(define (arch-dir arch platform)
  (~a arch "-" platform))

(define (arch-flag arch platform)
  (match arch
    ['x86_64 (match platform
               ['macosx
                "x86_64"]
               ['linux
                "x86-64"])]
    ['i386 (match platform
             ['macosx "i386"]
             ['linux (error "not sure what march flag to use for i686")])]))

(define bc-cs-dirs '("3m" "cs"))
(define (flags arch platform)
  (match platform
    ['macosx
     (list "-arch" (arch-flag arch platform) "-mmacosx-version-min=10.5")]
    ['linux
     (list (string-append "-march=" (arch-flag arch platform)))]))

(define (dylib-suffix platform)
  (match platform
    ['macosx "dylib"]
    ['linux "so"]))

(define (dylib-flag platform)
  (match platform
    ['macosx  "-dynamiclib"]
    ['linux "-shared"]))

(define (make-make-line strs)
  (apply string-append (add-between strs " ")))

(define (make-libs-and-lines platform disable-these-archs)
  (for*/list ([arch (in-list archs)]
              #:when (not (member arch disable-these-archs))
              [bc-cs-dir (in-list bc-cs-dirs)])
    (define path (build-path (arch-dir arch platform) bc-cs-dir))
    (define (ip file) (build-path path file))
    (define stem "buffer-add")
    (define (rp suffix)
      (path->string (ip (string-append stem "." suffix))))
    (list
     (rp "dylib")
     (list
      (list (rp "dylib") ":" (rp "o"))
      (append  (list "\tgcc" "-o" (rp (dylib-suffix platform))
                     (dylib-flag platform))
               (flags arch platform) (list (rp "o")))
      (list)
      (list (rp "o") ":" (string-append stem ".c"))
      (append (list "\tgcc" "-c" "-o" (rp "o")) (flags arch platform)
              (list (string-append stem ".c")))
      (list)
      ))
    ))

(define (make-makefile output-file platform disable-these-archs)

  (define libs-and-lines (make-libs-and-lines platform disable-these-archs))
  (define first-lines
    (list (list "## this makefile generated automatically by makefile-generate.rkt")
          (append (list "all" ":") (map first libs-and-lines))
          (list)))



  (call-with-output-file output-file
    #:exists 'truncate
    (λ (port)
      (for-each
       (compose (λ (s) (displayln s port))
                make-make-line)
       (append
        first-lines
        (apply append (map second libs-and-lines)))))))

(make-makefile "/tmp/makefile-mac" 'macosx '())
(make-makefile "/tmp/makefile-linux" 'linux '(i386))