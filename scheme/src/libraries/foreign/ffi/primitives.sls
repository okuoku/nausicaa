;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: primitives for foreign-functions interfaces
;;;Date: Fri Oct 30, 2009
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


(library (foreign ffi primitives)
  (export
    shared-object?			libc-shared-object
    open-shared-object			open-shared-object*
    lookup-shared-object		lookup-shared-object*
    make-c-function			make-c-function/with-errno
    make-c-callout			make-c-callout/with-errno
    make-c-callback
    (rename (platform:free-c-callback	free-c-callback))
    define-c-struct-accessor		define-c-struct-mutator
    define-c-struct-accessor-and-mutator
    define-c-struct-field-pointer-accessor)
  (import (rnrs)
    (foreign ffi sizeof)
    (foreign ffi conditions)
    (foreign ffi clang-data-types)
    (foreign ffi pointers)
    (prefix (foreign ffi platform) platform:)
    (only (unimplemented)
	  raise-unimplemented-error))


;;;; helpers

(define (%normalise-foreign-symbol foreign-symbol)
  (if (symbol? foreign-symbol)
      (symbol->string foreign-symbol)
    foreign-symbol))


(define-record-type (<shared-object> make-<shared-object> shared-object?)
  (fields (immutable library-name)
	  (immutable reference)))

(define (open-shared-object library-name)
  (let* ((library-name	(cond ((symbol? library-name)
			       (symbol->string library-name))
			      ((string? library-name)
			       library-name)
			      (else
			       (assertion-violation 'make-shared-object
				 "expected Scheme string or symbol as shared object library name"
				 library-name))))
	 (reference	(platform:open-shared-object library-name)))
    (if reference
	(make-<shared-object> library-name reference)
      #f)))

(define (open-shared-object* library-name)
  (let* ((library-name	(%normalise-foreign-symbol library-name))
	 (reference	(open-shared-object library-name)))
    (or reference
	(raise-unknown-shared-object 'open-shared-object* "unable to open shared object" library-name))))

(define libc-shared-object
  (open-shared-object* LIBC_SHARED_OBJECT_SPEC))

(define (lookup-shared-object shared-object foreign-symbol)
  (platform:lookup-shared-object (<shared-object>-reference shared-object) foreign-symbol))

(define (lookup-shared-object* shared-object foreign-symbol)
  (let* ((foreign-symbol	(%normalise-foreign-symbol foreign-symbol))
	 (pointer		(lookup-shared-object shared-object foreign-symbol)))
    (or pointer
	(raise-unknown-foreign-symbol 'lookup-shared-object*
				      "could not find foreign symbol in foreign library"
				      shared-object foreign-symbol))))


(define (make-c-function shared-object ret-type funcname arg-types)
  (make-c-callout ret-type
		  (lookup-shared-object* shared-object (%normalise-foreign-symbol funcname))
		  arg-types))

(define (make-c-function/with-errno shared-object ret-type funcname arg-types)
  (make-c-callout/with-errno ret-type
			     (lookup-shared-object* shared-object (%normalise-foreign-symbol funcname))
			     arg-types))

(define (make-c-callout ret-type address arg-types)
  (platform:make-c-callout (clang-external-type->clang-type ret-type)
			   address
			   (map clang-external-type->clang-type arg-types)))

(define (make-c-callout/with-errno ret-type address arg-types)
  (platform:make-c-callout/with-errno (clang-external-type->clang-type ret-type)
				      address
				      (map clang-external-type->clang-type arg-types)))

(define (make-c-callback ret-type proc arg-types)
  (platform:make-c-callback (clang-external-type->clang-type ret-type)
			    proc
			    (map clang-external-type->clang-type arg-types)))


(define-syntax define-c-struct-accessor-and-mutator
  (syntax-rules ()
    ((_ ?mutator-name ?accessor-name ?field-offset ?foreign-type-mutator ?foreign-type-accessor)
     (begin
       (define-c-struct-accessor ?accessor-name ?field-offset ?foreign-type-accessor)
       (define-c-struct-mutator  ?mutator-name  ?field-offset ?foreign-type-mutator)))))

(define-syntax define-c-struct-accessor
  (lambda (use-stx)
    (syntax-case use-stx ()
      ((_ ?accessor-name ?field-offset ?foreign-type-accessor)
       (if (syntax->datum (syntax ?field-offset))
	   #'(define-syntax ?accessor-name
	       (syntax-rules ()
		 ((_ struct-pointer)
		  (?foreign-type-accessor struct-pointer ?field-offset))))
	 #'(define-syntax ?accessor-name
	     (syntax-rules ()
	       ((_ struct-pointer)
		(raise-unimplemented-error (quote ?accessor-name))))))))))

(define-syntax define-c-struct-mutator
  (lambda (use-stx)
    (syntax-case use-stx ()
      ((_ ?mutator-name ?field-offset ?foreign-type-mutator)
       (if (syntax->datum (syntax ?field-offset))
	   #'(define-syntax ?mutator-name
	       (syntax-rules ()
		 ((_ struct-pointer value)
		  (?foreign-type-mutator struct-pointer
					?field-offset
					value))))
	 #'(define-syntax ?mutator-name
	     (syntax-rules ()
	       ((_ struct-pointer value)
		(raise-unimplemented-error (quote ?mutator-name))))))))))

(define-syntax define-c-struct-field-pointer-accessor
  (lambda (use-stx)
    (syntax-case use-stx ()
      ((_ ?accessor-name ?field-offset)
       (if (syntax->datum (syntax ?field-offset))
	   #'(define-syntax ?accessor-name
	       (syntax-rules ()
		 ((_ struct-pointer)
		  (pointer-add struct-pointer ?field-offset))))
	 #'(define-syntax ?accessor-name
	     (syntax-rules ()
	       ((_ struct-pointer)
		(raise-unimplemented-error (quote ?accessor-name))))))))))


;;;; done

)

;;; end of file
