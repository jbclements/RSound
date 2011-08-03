#lang racket

(require ffi/unsafe
         racket/runtime-path
         ffi/vector
         rackunit)

;; buffer-adding in C:

(provide s16buffer-add!/c)

(define-runtime-path buffer-add-libpath "buffer-add")

(define buffer-add-lib (ffi-lib buffer-add-libpath '("")))

(define s16buffer-add!/c
  (get-ffi-obj "bufferAdd"
               buffer-add-lib
               (_fun _pointer _pointer _int -> _void)))

