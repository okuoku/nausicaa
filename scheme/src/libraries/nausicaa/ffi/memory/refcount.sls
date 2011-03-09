;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: reference counted memory allocation
;;;Date: Tue Oct 13, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (nausicaa ffi memory refcount)
  (export
    malloc/refcount		(rename (malloc/refcount malloc/rc))
    pointer-acquire		pointer-release
    pointer-dismiss)
  (import (rnrs)
    (nausicaa ffi pointers)
    (only (nausicaa ffi memory alloc) malloc primitive-free)
    (nausicaa ffi peekers-and-pokers)
    (only (nausicaa ffi sizeof) c-strideof))


(define malloc/refcount
  (case-lambda
   ((number-of-bytes)
    (malloc/refcount number-of-bytes malloc))
   ((number-of-bytes malloc-funk)
    (let ((p (malloc-funk (+ (c-strideof unsigned-long) number-of-bytes))))
      (pointer-c-set! unsigned-long p 0 0)
      (pointer-add p (c-strideof unsigned-long))))))

(define-syntax refcount-set!
  (syntax-rules ()
    ((_ ?pointer ?value)
     (pointer-c-set! unsigned-long ?pointer (- (c-strideof unsigned-long)) ?value))))

(define-syntax refcount-ref
  (syntax-rules ()
    ((_ ?pointer)
     (pointer-c-ref unsigned-long ?pointer (- (c-strideof unsigned-long))))))

(define (pointer-acquire pointer)
  (refcount-set! pointer (+ 1 (refcount-ref pointer))))

(define (pointer-refcount-begin pointer)
  (pointer-add pointer (- (c-strideof unsigned-long))))

(define pointer-release
  (case-lambda
   ((pointer)
    (pointer-release pointer primitive-free))
   ((pointer free-func)
    (let ((rc (refcount-ref pointer)))
      (if (= 1 rc)
	  (primitive-free (pointer-refcount-begin pointer))
	(refcount-set! pointer (- rc 1)))))))

(define pointer-dismiss
  (case-lambda
   ((pointer)
    (pointer-dismiss pointer primitive-free))
   ((pointer free-func)
    (primitive-free (pointer-refcount-begin pointer)))))


;;;; done

)

;;; end of file
