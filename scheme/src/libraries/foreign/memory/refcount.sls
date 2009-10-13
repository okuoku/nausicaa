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
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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


(library (foreign memory refcount)
  (export
    malloc/refcount		(rename (malloc/refcount malloc/rc))
    pointer-acquire		pointer-release
    pointer-dismiss)
  (import (rnrs)
    (foreign memory pointers)
    (foreign memory alloc))


(define malloc/refcount
  (case-lambda
   ((number-of-bytes)
    (malloc/refcount number-of-bytes malloc))
   ((number-of-bytes malloc-funk)
    (let ((p (malloc-funk (+ strideof-long number-of-bytes))))
      (pointer-set-c-unsigned-long! p 0 0)
      (pointer-add p strideof-long)))))

(define-syntax refcount-set!
  (syntax-rules ()
    ((_ ?pointer ?value)
     (pointer-set-c-unsigned-long! ?pointer (- strideof-long) ?value))))

(define-syntax refcount-ref
  (syntax-rules ()
    ((_ ?pointer)
     (pointer-ref-c-unsigned-long ?pointer (- strideof-long)))))

(define (pointer-acquire pointer)
  (refcount-set! pointer (+ 1 (refcount-ref pointer))))

(define (pointer-refcount-begin pointer)
  (pointer-add pointer (- strideof-long)))

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
