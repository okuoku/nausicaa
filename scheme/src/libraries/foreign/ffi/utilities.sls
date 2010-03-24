;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: utility macros for FFI
;;;Date: Mon Nov 30, 2009
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


(library (foreign ffi utilities)
  (export
    define-shared-object
    make-c-function*		make-c-function/with-errno*
    make-c-callout*		make-c-callout/with-errno*
    make-c-callback*
    define-c-functions		define-c-functions/with-errno
    define-c-callouts		define-c-callouts/with-errno
    define-struct-fields
    define-with-struct		with-struct-fields)
  (import (rnrs)
    (only (language-extensions)
	  define-identifier-accessor-mutator)
    (parameters)
    (foreign ffi primitives)
    (for (foreign ffi clang-data-types) expand run))


(define-syntax define-shared-object
  (syntax-rules ()
    ((_ ?name ?library-name)
     (define ?name
       (open-shared-object ?library-name)))))

(define-syntax define-c-functions
  (syntax-rules ()
    ((_ ?shared-object (?name (?retval ?funcname (?arg0 ?arg ...))) ...)
     (begin
       (define ?name
	 (make-c-function* ?shared-object ?retval ?funcname (?arg0 ?arg ...)))
       ...))))

(define-syntax define-c-functions/with-errno
  (syntax-rules ()
    ((_ ?shared-object (?name (?retval ?funcname (?arg0 ?arg ...))) ...)
     (begin
       (define ?name
	 (make-c-function/with-errno* ?shared-object ?retval ?funcname (?arg0 ?arg ...)))
       ...))))

(define-syntax define-c-callouts
  (syntax-rules ()
    ((_ (?name (?retval ?funcname (?arg0 ?arg ...))) ...)
     (begin
       (define ?name
	 (make-c-callout* ?retval ?funcname (?arg0 ?arg ...)))
       ...))))

(define-syntax define-c-callouts/with-errno
  (syntax-rules ()
    ((_ (?name (?retval ?funcname (?arg0 ?arg ...))) ...)
     (begin
       (define ?name
	 (make-c-callout/with-errno* ?retval ?funcname (?arg0 ?arg ...)))
       ...))))


(define-syntax make-c-function*
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?shared-object ?ret-type ?funcname (?arg-type0 ?arg-type ...))
       (with-syntax ((RET	(clang-quote-type-stx-if-external #'?ret-type))
		     ((ARG ...)	(map clang-quote-type-stx-if-external #'(?arg-type0 ?arg-type ...))))
	 #'(make-c-function ?shared-object RET '?funcname (list ARG ...)))))))

(define-syntax make-c-function/with-errno*
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?shared-object ?ret-type ?funcname (?arg-type0 ?arg-type ...))
       (with-syntax ((RET	(clang-quote-type-stx-if-external #'?ret-type))
		     ((ARG ...)	(map clang-quote-type-stx-if-external #'(?arg-type0 ?arg-type ...))))
	 #'(make-c-function/with-errno ?shared-object RET '?funcname (list ARG ...)))))))

(define-syntax make-c-callout*
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?ret-type ?address (?arg-type0 ?arg-type ...))
       (with-syntax ((RET	(clang-quote-type-stx-if-external #'?ret-type))
		     ((ARG ...)	(map clang-quote-type-stx-if-external #'(?arg-type0 ?arg-type ...))))
	 #'(make-c-callout RET ?address (list ARG ...)))))))

(define-syntax make-c-callout/with-errno*
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?ret-type ?address (?arg-type0 ?arg-type ...))
       (with-syntax ((RET	(clang-quote-type-stx-if-external #'?ret-type))
		     ((ARG ...)	(map clang-quote-type-stx-if-external #'(?arg-type0 ?arg-type ...))))
	 #'(make-c-callout/with-errno RET ?address (list ARG ...)))))))

(define-syntax make-c-callback*
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?ret-type ?scheme-function (?arg-type0 ?arg-type ...))
       (with-syntax ((RET	(clang-quote-type-stx-if-external #'?ret-type))
		     ((ARG ...)	(map clang-quote-type-stx-if-external #'(?arg-type0 ?arg-type ...))))
	 #'(make-c-callback RET ?scheme-function (list ARG ...)))))))


(define-syntax %define-struct-field-identifier
  (lambda (stx)
    (define (%name.field name field)
      (string->symbol (string-append (symbol->string name) "." (symbol->string field))))
    (define (%accessor-name struct field)
      (string->symbol (string-append "struct-" (symbol->string struct)
				     "-" (symbol->string field) "-ref")))
    (define (%mutator-name struct field)
      (string->symbol (string-append "struct-" (symbol->string struct)
				     "-" (symbol->string field) "-set!")))
    (syntax-case stx ()
      ((_ ?name ?struct ?accessible-field)
       (let ((name	(syntax->datum #'?name))
	     (struct	(syntax->datum #'?struct))
	     (field	(syntax->datum #'?accessible-field)))
	 (with-syntax ((NAME.FIELD (datum->syntax #'?name (%name.field name field)))
		       (ACCESSOR   (datum->syntax #'?struct (%accessor-name struct field))))
	   #'(define-identifier-accessor-mutator NAME.FIELD ?name ACCESSOR))))
      ((_ ?name ?struct ?accessible-field ?mutable-field)
       (let ((name	(syntax->datum #'?name))
	     (struct	(syntax->datum #'?struct))
	     (afield	(syntax->datum #'?accessible-field))
	     (mfield	(syntax->datum #'?mutable-field)))
	 (with-syntax ((NAME.FIELD (datum->syntax #'?name (%name.field name afield)))
		       (ACCESSOR   (datum->syntax #'?struct (%accessor-name struct afield)))
		       (MUTATOR    (datum->syntax #'?struct (%mutator-name  struct mfield))))
	   #'(define-identifier-accessor-mutator NAME.FIELD ?name ACCESSOR MUTATOR)))))))

(define-syntax %define-struct-field-identifiers
  (syntax-rules ()
    ((_ ?name ?struct (?accessible-field ?mutable-field) ?field-spec ...)
     (begin
       (%define-struct-field-identifier ?name ?struct ?accessible-field ?mutable-field)
       (%define-struct-field-identifiers ?name ?struct ?field-spec ...)))
    ((_ ?name ?struct (?accessible-field) ?field-spec ...)
     (begin
       (%define-struct-field-identifier ?name ?struct ?accessible-field)
       (%define-struct-field-identifiers ?name ?struct ?field-spec ...)))
    ((_ ?name ?struct)
     (values))))

(define-syntax define-struct-fields
  (lambda (stx)
    (define (%define-name struct)
      (string->symbol (string-append "define-" (symbol->string struct))))
    (syntax-case stx ()
      ((_ ?struct ?field-spec ...)
       (with-syntax ((DEFINED (datum->syntax #'?struct (%define-name (syntax->datum #'?struct)))))
	 #'(define-syntax DEFINED
	     (syntax-rules ()
	       ((_ ??name)
		(%define-struct-field-identifiers ??name ?struct ?field-spec ...)))))))))


(define-syntax %struct-field-identifier-syntax
  (syntax-rules ()
    ((_ ?name ?accessor)
     (identifier-syntax
      (_		(?accessor ?name))))
    ((_ ?name ?accessor ?mutator)
     (identifier-syntax
      (_		(?accessor ?name))
      ((set! _ ?expr)	(?mutator ?name ?expr))))))

(define-syntax %with-struct-field-identifier
  (lambda (stx)
    (define (%name.field name field)
      (string->symbol (string-append (symbol->string name) "." (symbol->string field))))
    (define (%accessor-name struct field)
      (string->symbol (string-append "struct-" (symbol->string struct)
				     "-" (symbol->string field) "-ref")))
    (define (%mutator-name struct field)
      (string->symbol (string-append "struct-" (symbol->string struct)
				     "-" (symbol->string field) "-set!")))
    (syntax-case stx ()
      ((_ ?name ?struct (?accessible-field) ?body0 ?body ...)
       (let ((name	(syntax->datum #'?name))
	     (struct	(syntax->datum #'?struct))
	     (field	(syntax->datum #'?accessible-field)))
	 (with-syntax ((NAME.FIELD (datum->syntax #'?name (%name.field name field)))
		       (ACCESSOR   (datum->syntax #'?struct (%accessor-name struct field))))
	   #'(let-syntax ((NAME.FIELD (%struct-field-identifier-syntax ?name ACCESSOR)))
	       ?body0 ?body ...))))
      ((_ ?name ?struct (?accessible-field ?mutable-field) ?body0 ?body ...)
       (let ((name	(syntax->datum #'?name))
	     (struct	(syntax->datum #'?struct))
	     (afield	(syntax->datum #'?accessible-field))
	     (mfield	(syntax->datum #'?mutable-field)))
	 (with-syntax ((NAME.FIELD (datum->syntax #'?name (%name.field name afield)))
		       (ACCESSOR   (datum->syntax #'?struct (%accessor-name struct afield)))
		       (MUTATOR    (datum->syntax #'?struct (%mutator-name  struct mfield))))
	   #'(let-syntax ((NAME.FIELD (%struct-field-identifier-syntax ?name ACCESSOR MUTATOR)))
	       ?body0 ?body ...)))))))

(define-syntax %with-struct-field-identifiers
  (syntax-rules ()
    ((_ ?name ?struct ((?accessible-field ?mutable-field) ?field-spec ...) ?body0 ?body ...)
     (%with-struct-field-identifier
      ?name ?struct (?accessible-field ?mutable-field)
      (%with-struct-field-identifiers ?name ?struct (?field-spec ...) ?body0 ?body ...)))
    ((_ ?name ?struct ((?accessible-field) ?field-spec ...) ?body0 ?body ...)
     (%with-struct-field-identifier
      ?name ?struct (?accessible-field)
      (%with-struct-field-identifiers ?name ?struct (?field-spec ...) ?body0 ?body ...)))
    ((_ ?name ?struct () ?body0 ?body ...)
     (begin ?body0 ?body ...))))

(define-syntax define-with-struct
  (lambda (stx)
    (define (%with-name struct)
      (string->symbol (string-append "with-struct-" (symbol->string struct))))
    (syntax-case stx ()
      ((_ ?struct ?field-spec ...)
       (with-syntax ((WITH (datum->syntax #'?struct (%with-name (syntax->datum #'?struct)))))
	 #'(define-syntax WITH
	     (syntax-rules ()
	       ((_ (?name0 ?name (... ...)) ?body0 ?body (... ...))
		(%with-struct-field-identifiers
		 ?name0 ?struct (?field-spec ...)
		 (%with-struct-field-identifiers (?name (... ...)) ?struct (?field-spec ...)
						 ?body0 ?body (... ...))))
	       ((_ () ?body0 ?body (... ...))
		(begin ?body0 ?body (... ...)))
	       ((_ ?name ?body0 ?body (... ...))
		(%with-struct-field-identifiers ?name ?struct (?field-spec ...)
						?body0 ?body (... ...)))
	       )))))))

(define-syntax with-struct-fields
  (lambda (stx)
    (define (%with-name struct)
      (string->symbol (string-append "with-struct-" (symbol->string struct))))
    (syntax-case stx ()
      ((_ ((?struct ?name) ?struct-spec ...) ?body0 ?body ...)
       (with-syntax ((WITH (datum->syntax #'?struct (%with-name (syntax->datum #'?struct)))))
	 #'(WITH ?name (with-struct-fields (?struct-spec ...) ?body0 ?body ...))))
      ((_ () ?body0 ?body ...)
       #'(begin ?body0 ?body ...)))))


;;;; done

)

;;; end of file
