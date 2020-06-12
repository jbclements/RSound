#lang racket

(define output-file "/tmp/makefile-mac")

;; can these get out of date?
(define archs '(x86_64 i386))
(define (arch-dir arch)
  (match arch
    ['x86_64 "x86_64-macosx"]
    ['i386   "i386-macosx"]))
(define (arch-flag arch)
  (match arch
    ['x86_64 "x86_64"]
    ['i386 "i386"]))
(define bc-cs-dirs '("3m" "cs"))
(define (flags arch) (list "-arch" (arch-flag arch) "-mmacosx-version-min=10.5"))

(define (make-make-line strs)
  (apply string-append (add-between strs " ")))

(define libs-and-lines
  (for*/list ([arch (in-list archs)]
              [bc-cs-dir (in-list bc-cs-dirs)])
    (define path (build-path (arch-dir arch) bc-cs-dir))
    (define (ip file) (build-path path file))
    (define stem "buffer-add")
    (define (rp suffix) (path->string (ip (string-append stem "." suffix))))
    (list
     (rp "dylib")
     (list
      (list (rp "dylib") ":" (rp "o"))
      (append  (list "\tgcc" "-o" (rp "dylib") "-dynamiclib") (flags arch) (list (rp "o")))
      (list)
      (list (rp "o") ":" (string-append stem ".c"))
      (append (list "\tgcc" "-c" "-o" (rp "o")) (flags arch) (list (string-append stem ".c")))
      (list)
      ))
    ))

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
      (apply append (map second libs-and-lines))))))