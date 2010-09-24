;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility pointer data type
;;;Date: Tue Oct 13, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (ffi pointers compat)
  (export
    pointer?
    integer->pointer	pointer->integer
    pointer-null	pointer-null?
    pointer-diff	pointer-add
    pointer=?		pointer<>?
    pointer<?		pointer>?
    pointer<=?		pointer>=?)
  (import (rnrs)
    (only (ikarus foreign) pointer->integer integer->pointer pointer?))


(define pointer-null
  (integer->pointer 0))

(define (pointer-null? pointer)
  (zero? (pointer->integer pointer)))

(define (pointer-diff pointer-1 pointer-2)
  (- (pointer->integer pointer-1)
     (pointer->integer pointer-2)))

(define (pointer-add pointer offset)
  (integer->pointer (+ (pointer->integer pointer)
		       offset)))

(let-syntax ((define-pointer-comparison (syntax-rules ()
					  ((_ ?name ?func)
					   (define ?name
					     (case-lambda
					      (()
					       #f)
					      ((pointer)
					       #t)
					      ((pointer-a pointer-b)
					       (?func (pointer->integer pointer-a)
						      (pointer->integer pointer-b)))
					      ((pointer-a pointer-b . pointers)
					       (apply ?func (map pointer->integer
							      (cons* pointer-a
								     pointer-b pointers))))))))))
  (define-pointer-comparison pointer=? =)
  (define-pointer-comparison pointer<? <)
  (define-pointer-comparison pointer>? >)
  (define-pointer-comparison pointer<=? <=)
  (define-pointer-comparison pointer>=? >=))

(define pointer<>?
  (case-lambda
   (()
    #f)
   ((pointer . pointers)
    (not (apply pointer=? pointer pointers)))))


;;;; done

)

;;; end of file
