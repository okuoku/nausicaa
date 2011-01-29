;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: aliases for the peekers and pokers
;;;Date: Tue Jul  7, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009-2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa ffi peekers-and-pokers)
  (export
    pointer-c-ref		pointer-c-set!
    pointer-c-accessor		pointer-c-mutator
    array-c-ref			array-c-set!
    array-c-pointer-to)
  (import (rnrs)
    (nausicaa ffi pointers)
    (nausicaa ffi sizeof)
    ;;There  must  be  a  peeker  and  a poker  for  each  type  in  the
    ;;ENUM-CLANG-INTERNAL-TYPES enumeration.
    (rename (nausicaa ffi peekers-and-pokers compat)
	    (pointer-ref-c-int8		pointer-ref-c-int8_t)
	    (pointer-ref-c-uint8	pointer-ref-c-uint8_t)
	    (pointer-ref-c-int16	pointer-ref-c-int16_t)
	    (pointer-ref-c-uint16	pointer-ref-c-uint16_t)
	    (pointer-ref-c-int32	pointer-ref-c-int32_t)
	    (pointer-ref-c-uint32	pointer-ref-c-uint32_t)
	    (pointer-ref-c-int64	pointer-ref-c-int64_t)
	    (pointer-ref-c-uint64	pointer-ref-c-uint64_t)
	    (pointer-set-c-int8!	pointer-set-c-int8_t!)
	    (pointer-set-c-uint8!	pointer-set-c-uint8_t!)
	    (pointer-set-c-int16!	pointer-set-c-int16_t!)
	    (pointer-set-c-uint16!	pointer-set-c-uint16_t!)
	    (pointer-set-c-int32!	pointer-set-c-int32_t!)
	    (pointer-set-c-uint32!	pointer-set-c-uint32_t!)
	    (pointer-set-c-int64!	pointer-set-c-int64_t!)
	    (pointer-set-c-uint64!	pointer-set-c-uint64_t!))
    (for (nausicaa ffi syntax-helpers) expand)
    (for (nausicaa ffi clang type-translation) expand))


(define-syntax pointer-c-ref
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?type ?pointer ?offset)
       #`(#,(%prepend #'pointer-c-ref "pointer-ref-c-"
		      (map-identifier-syntax-object clang-external-type->clang-internal-type #'?type))
	  ?pointer ?offset))
      (?input-form
       (syntax-violation 'pointer-c-ref
	 "invalid C language specification for raw memory getter"
	 (syntax->datum #'?input-form)))
      )))

(define-syntax pointer-c-set!
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?type ?pointer ?offset ?value)
       #`(#,(%enclose #'pointer-c-set!
		      "pointer-set-c-"
		      (map-identifier-syntax-object clang-external-type->clang-internal-type #'?type)
		      "!")
	  ?pointer ?offset ?value))
      (?input-form
       (syntax-violation 'pointer-c-set!
	 "invalid C language specification for raw memory setter"
	 (syntax->datum #'?input-form)))
      )))

(define-syntax pointer-c-accessor
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?type)
       #`(begin #,(%prepend #'pointer-c-ref "pointer-ref-c-"
			    (map-identifier-syntax-object
			     clang-external-type->clang-internal-type #'?type))))
      (?input-form
       (syntax-violation 'pointer-c-accessor
	 "invalid C language specification for raw memory getter"
	 (syntax->datum #'?input-form)))
      )))

(define-syntax pointer-c-mutator
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?type)
       #`(begin #,(%enclose #'pointer-c-set! "pointer-set-c-"
			    (map-identifier-syntax-object
			     clang-external-type->clang-internal-type #'?type)
			    "!")))
      (?input-form
       (syntax-violation 'pointer-c-mutator
	 "invalid C language specification for raw memory setter"
	 (syntax->datum #'?input-form)))
      )))

(define-syntax array-c-ref
  (syntax-rules ()
    ((_ ?type ?pointer ?index)
     (pointer-c-ref ?type ?pointer (* ?index (c-strideof ?type))))))

(define-syntax array-c-set!
  (syntax-rules ()
    ((_ ?type ?pointer ?index ?value)
     (pointer-c-set! ?type ?pointer (* ?index (c-strideof ?type)) ?value))))

(define-syntax array-c-pointer-to
  (syntax-rules ()
    ((_ ?type ?pointer ?index)
     (pointer-add ?pointer (* ?index (c-strideof ?type))))))


;;;; done

)

;;; end of file
