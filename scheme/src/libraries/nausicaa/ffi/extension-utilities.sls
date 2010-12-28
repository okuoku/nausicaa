;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: utilities for packages implementing bindings through the FFI
;;;Date: Tue May 11, 2010
;;;
;;;Abstract
;;;
;;;	This   library  is  for   internal  use   by  the   FFI  creator
;;;	infrastructure;   it  interacts  with   automatically  generated
;;;	libraries.
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa ffi extension-utilities)
  (export define-sizeof-macros)
  (import (rnrs))


(define-syntax %define-helper
  (syntax-rules ()
    ((_ ?name . ?body)
     (define-syntax ?name
       (lambda (stx)
	 (syntax-case stx ()
	   ((?kontext)
	    (datum->syntax #'?kontext (quote (begin . ?body))))))))))


(%define-helper
 define-sizeof-macros

 (define-syntax c-sizeof
   (lambda (stx)
     (syntax-case stx ()

       ((_ ?type ?number-of-elements)
	(identifier? #'?type)
	(let ((type (clang-foreign-type->clang-external-type (syntax->datum #'?type))))
	  (if type
	      #`(* ?number-of-elements #,(%prepend #'?c-sizeof "strideof-" type))
	    #'(ffi:c-strideof ?type))))

       ((_ ?type)
	(identifier? #'?type)
	(let ((type (clang-foreign-type->clang-external-type (syntax->datum #'?type))))
	  (if type
	      #`(begin #,(%prepend #'?c-sizeof "sizeof-" type))
	    #`(ffi:c-sizeof ?type))))

       (?input-form
	(syntax-violation 'c-sizeof
	  "invalid C language sizeof specification"
	  (syntax->datum #'?input-form))))))

 (define-syntax c-strideof
   (lambda (stx)
     (syntax-case stx ()

       ((_ ?type)
	(identifier? #'?type)
	(let ((type (clang-foreign-type->clang-external-type (syntax->datum #'?type))))
	  (if type
	      #`(begin #,(%prepend #'c-strideof "strideof-" type))
	    #'(ffi:c-strideof ?type))))

       (?input-form
	(syntax-violation 'c-strideof
	  "invalid C language strideof specification"
	  (syntax->datum #'?input-form))))))

 (define-syntax c-alignof
   (lambda (stx)
     (syntax-case stx ()

       ((_ ?type)
	(identifier? #'?type)
	(let ((type (clang-foreign-type->clang-external-type (syntax->datum #'?type))))
	  (if type
	      #`(begin #,(%prepend #'c-alignof "alignof-" type))
	    #'(ffi:c-alignof ?type))))

       (?input-form
	(syntax-violation 'c-alignof
	  "invalid C language alignof specification"
	  (syntax->datum #'?input-form))))))

 (define-syntax c-valueof
   (lambda (stx)
     (syntax-case stx ()

       ((_ ?thing)
	(identifier? #'?thing)
	#`(begin #,(%prepend #'c-valueof "valueof-" #'?thing)))

       (?input-form
	(syntax-violation 'c-valueof
	  "invalid C language valueof specification"
	  (syntax->datum #'?input-form))))))

 (define-syntax c-inspect
   (lambda (stx)
     (syntax-case stx ()
       ((_ ?thing)
	(identifier? #'?thing)
	#`(begin #,(%prepend #'c-inspect "inspect-" #'?thing)))
       (?input-form
	(syntax-violation 'c-inspect
	  "invalid C language valueof specification"
	  (syntax->datum #'?input-form))))))

 (define-syntax pointer-c-ref
   (lambda (stx)
     (syntax-case stx ()
       ((_ ?type ?pointer ?offset)
	#`(ffi:pointer-c-ref
	   #,(let ((type (clang-foreign-type->clang-external-type (syntax->datum #'?type))))
	       (if type
		   (datum->syntax #'?type type)
		 #'?type))
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
	#`(ffi:pointer-c-set!
	   #,(let ((type (clang-foreign-type->clang-external-type (syntax->datum #'?type))))
	       (if type
		   (datum->syntax #'?type type)
		 #'?type))
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
	#`(ffi:pointer-c-accessor
	   #,(let ((type (clang-foreign-type->clang-external-type (syntax->datum #'?type))))
	       (if type
		   (datum->syntax #'?type type)
		 #'?type))))
       (?input-form
	(syntax-violation 'pointer-c-accessor
	  "invalid C language specification for raw memory getter"
	  (syntax->datum #'?input-form)))
       )))

 (define-syntax pointer-c-mutator
   (lambda (stx)
     (syntax-case stx ()
       ((_ ?type)
	#`(ffi:pointer-c-mutator
	   #,(let ((type (clang-foreign-type->clang-external-type (syntax->datum #'?type))))
	       (if type
		   (datum->syntax #'?type type)
		 #'?type))))
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
      (ffi:pointer-add ?pointer (* ?index (c-strideof ?type))))))

 )


;;;; done

)

;;; end of file
