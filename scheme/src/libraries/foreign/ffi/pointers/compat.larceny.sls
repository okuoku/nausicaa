;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility pointer data type
;;;Date: Tue Oct 13, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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


(library (foreign ffi pointers compat)
  (export
    pointer?
    integer->pointer			pointer->integer
    pointer-null			pointer-null?
    pointer-diff			pointer-add
    pointer=?				pointer<>?
    pointer<?				pointer>?
    pointer<=?				pointer>=?

    retval->pointer)
  (import (rnrs)
    (primitives foreign-procedure
		%peek8 %peek8u %peek16 %peek16u %peek32 %peek32u %peek-pointer
		%poke8 %poke8u %poke16 %poke16u %poke32 %poke32u %poke-pointer
		void*-double-set! void*-double-ref void*-float-set! void*-float-ref
		void*? void*-rt record-constructor void*->address))


;;Larceny handles pointers  records of type "void*", but  when a pointer
;;is  internally  detected  to be  NULL  it  is  converted to  #f.   The
;;following is a basic guide on the "void*" API:
;;
;; void*-rt	Bound to the record type descriptor.
;;
;; void*?	Return true when applied to a "void*" record.  This
;;		predicate does NOT accept #f as valid value.
;;
;; void*->address
;;		Applied to a "void*" record return an exact integer
;;		representing the memory address.
;;
;; record-constructor
;;		Applied to a record type descriptor returns the record
;;		constructor procedure.  The record constructor is raw,
;;		it does NOT validate its arguments.
;;
;;The following interface provides what is needed to export bindings for
;;"void*" aliases to  bindings for a virtual "pointer"  type.  The macro
;;RETVAL->POINTER  is to  be used  to convert  to a  "void*"  record the
;;return value of the callout procedures (see below).
;;

(define make-pointer (record-constructor void*-rt))

(define pointer? void*?)

(define (integer->pointer value)
  (if (integer? value)
      (make-pointer value)
    (assertion-violation 'integer->pointer
      "expected integer value" value)))

(define (pointer->integer pointer)
  (if (pointer? pointer)
      (void*->address pointer)
    (assertion-violation 'pointer->integer
      "expected pointer value" pointer)))

(define-syntax retval->pointer
  (syntax-rules ()
    ((_ ?value)
     (let ((value ?value))
       (if (void*? value)
	   value
	 pointer-null)))))

(define pointer-null
  (make-pointer 0))

(define (pointer-null? pointer)
  (zero? (void*->address pointer)))

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
					      (()		#f)
					      ((pointer)	#t)
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
