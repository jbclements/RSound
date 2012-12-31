#lang racket
;;; Racket Science Collection
;;; unsafe-ops-utils.ss
;;; Copyright (c) 2010 M. Douglas Williams
;;;
;;; This file is part of the Racket Science Collection.
;;;
;;; The Racket Science Collection is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the License or
;;; (at your option) any later version.
;;;
;;; The Racket Science Collection is distributed in the hope that it will be
;;; useful, but WITHOUT WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with the Racket Science Collection.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;;
;;; -----------------------------------------------------------------------------
;;;
;;; This code has utility functions that can be used to assure floats for unsafe
;;; code. The basic philosophy we are taking is to use the unsafe operations
;;; where it makes sense, but to protect the code so that the operands to unsafe
;;; operations are guaranteed to be of the correct type.
;;;
;;; Version  Date      Description
;;; 4.0.0    05/16/10  Moved the unsafe ops utility functions from math.ss and
;;;                    added with-fixed. (MDW)

(require scheme/flonum)
(require scheme/unsafe/ops)

;;; (real->float x) -> inexact-real?
;;;   x : real?
;;; Returns an inexact real (i.e., a float) given real x. Raises an error if x
;;; is not a real. This can be used to assure a real value is a float, even in
;;; unsafe code.
(define (real->float x)
  (if (real? x)
      (exact->inexact x)
      (error "expected real, given" x)))

;;; (real-vector->float-vector v) -> (vectorof inexact-real?)
;;;   v : (vectorof real?)
;;; Returns a vector of inexact reals (i.e., floats) given a vector of reals, v.
;;; Raises an error if an element of v is not a real.
(define (real-vector->float-vector v)
  (build-vector
   (vector-length v)
   (lambda (i)
     (real->float (vector-ref v i)))))

;;; (real-vector->flvector v) -> flvector?
;;;   v : (vectorof real?)
;;; Returns an flvector given a vector of reals, v. Raises an error if an element
;;; of v is not a real.
(define (real-vector->flvector v)
  (let ((fl-v (make-flvector (vector-length v))))
    (for ((i (in-range (vector-length v))))
      (unsafe-flvector-set! fl-v i
                            (real->float (unsafe-vector-ref v i))))
    fl-v))

;;; (with-fixed (x ...)
;;;  expr ...)
;;; Executes the expr's with the x's guaranteed to be fixnums. All of the x's
;;; must be identifiers. Note that this does not attempt to coerce anything to a
;;; fixnum, just assure that they are.
(define-syntax (with-fixed stx)
  (syntax-case stx ()
    ((with-fized (x ...) expr ...)
     (for ((id (in-list (syntax->list #'(x ...)))))
       (unless (identifier? id)
         (raise-syntax-error #f
                             "not an identifier"
                             stx
                             id)))
     #`(let ((x (if (fixnum? x)
                    x
                    (error "expected fixed integer, given" x)))
             ...)
         expr ...))))

;;; (with-float (x ...)
;;;   expr ...)
;;; Executes the expr's with the x's guaranteed to be floats. All of the x's
;;; must be identifiers.
(define-syntax (with-float stx)
  (syntax-case stx ()
    ((with-float (x ...) expr ...)
     (for ((id (in-list (syntax->list #'(x ...)))))
       (unless (identifier? id)
         (raise-syntax-error #f
                             "not an identifier"
                             stx
                             id)))
     #`(let ((x (if (real? x)
                    (exact->inexact x)
                    (error "expected real, given" x)))
             ...)
         expr ...))))

;;; Module Contracts

(provide
 with-fixed
 with-float)

(provide/contract
 (real->float
  (-> real? inexact-real?))
 (real-vector->float-vector
  (-> (vectorof real?) (vectorof inexact-real?)))
 (real-vector->flvector
  (-> (vectorof real?) flvector?)))
