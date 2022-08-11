#lang racket


;; why make a makefile? Why not just do the compilation?

;; abstraction is weak here...

(define archs '(x86_64 i386 aarch64))

;; don't try this with a name containing spaces...
(define (find-exec name)
  (unless (regexp-match #px"^[a-zA-Z0-9]+$" name)
    (error 'find-exec
           "this string might not parse as a single word in the shell: ~v\n"
           name))
  (define outstr
    (string-trim (with-output-to-string
                   (λ () (system (string-append "which "name))))))
  (when (equal? outstr "")
    (error 'find-exec
           "couldn't find ~v using `which ~v`"
           name name))
  (unless (file-exists? outstr)
    (error 'find-exec
           "command `which ~v` didn't print the path of an existing file"
           name))
  outstr)

(define gcc-path (find-exec "gcc"))



;; the subdirectory used for a given arch and platform
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
             ['linux (error "not sure what march flag to use for i686")])]
    ['aarch64 (match platform
                ['macosx
                 "arm64"])]))

(define bc-cs-dirs '("3m" "cs"))

;; the flags to use for a given arch & platform
(define (flags arch platform)
  (match platform
    ['macosx
     (list "-arch" (arch-flag arch platform) "-mmacosx-version-min=10.5")]
    ['linux
     (list (string-append "-march=" (arch-flag arch platform)))]))

;; the dylib suffix on the given platform
(define (dylib-suffix platform)
  (match platform
    ['macosx "dylib"]
    ['linux "so"]))

;; the gcc flag for generating a dylib on the given platform
(define (dylib-flag platform)
  (match platform
    ['macosx  "-dynamiclib"]
    ['linux "-shared"]))

;; call gcc (but first print out the args)
(define (call-gcc args)
  (printf "~v\n" (cons "gcc" args))
  (apply system* gcc-path args))

;; compile the shared library for the given platforms, except for
;; the specified architectures
(define (make platform disable-these-archs)
  (for*/list ([arch (in-list archs)]
              #:when (not (member arch disable-these-archs))
              [bc-cs-dir (in-list bc-cs-dirs)])
    (define tgt-path (build-path (arch-dir arch platform) bc-cs-dir))
    (define stem "buffer-add")
    (define (suffix->path suffix)
      (path->string (build-path tgt-path (string-append stem "." suffix))))
    ;; compile
    (call-gcc (append (list "-c" "-o" (suffix->path "o")) (flags arch platform)
                      (list (string-append stem ".c"))))
    ;; link
    (call-gcc (append  (list "-o" (suffix->path (dylib-suffix platform))
                             (dylib-flag platform))
                       (flags arch platform) (list (suffix->path "o"))))
    ))


;(make 'macosx '(i386))
;(make 'linux '(i386))
