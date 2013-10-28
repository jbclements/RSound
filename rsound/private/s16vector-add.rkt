#lang racket/base

(require ffi/unsafe
         racket/runtime-path
         ffi/vector
         rackunit)

;; buffer-adding in C:

;; accepts target pointer, source pointer, and copy length *in samples*
(provide s16buffer-add!/c
         s16buffer-mult-add!/c)

(define-runtime-path here ".")

(define buffer-add-lib (ffi-lib 
                        (build-path here
                                    (system-library-subpath)
                                    "buffer-add")))

(define s16buffer-add!/c
  (get-ffi-obj "bufferAdd"
               buffer-add-lib
               (_fun _pointer _pointer _int -> _void)))

(define s16buffer-mult-add!/c
  (get-ffi-obj "bufferMultAdd"
               buffer-add-lib
               (_fun _pointer _pointer _int _double -> _void)))



