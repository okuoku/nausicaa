;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: record utilities
;;;Date: Wed Sep  9, 2009
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


#!r6rs
(library (records)
  (export
    record-parent-list			record-parent-list*
    make-record-maker			make-record-maker*

    record-field-accessor		record-field-accessor*
    record-field-mutator		record-field-mutator*

    define-record-accessors
    define-record-accessors/this
    define-record-accessors/parents

    define-record-mutators
    define-record-mutators/this
    define-record-mutators/parents

    define-record-accessors&mutators
    define-record-accessors&mutators/this
    define-record-accessors&mutators/parents

    with-record-accessors
    with-record-mutators
    with-record-accessors&mutators

    with-record-fields			with-record-fields*
    with-virtual-fields				with-virtual-fields*
    define-record-extension)
  (import (rnrs)
    (for (records helpers) run expand))


(define-syntax record-parent-list*
  (syntax-rules ()
    ((_ ?record-name)
     (let-syntax
	 ((dummy (lambda (stx)
		   (syntax-case stx ()
		     ((_)
		      (with-syntax ((ELL (record-parent-list (record-type-descriptor ?record-name))))
			#'(quote ELL)))))))
       (dummy)))))


(define make-record-maker
  (case-lambda
   ((rtd)
    (make-record-maker rtd #f))
   ((rtd init)
    (let ((init-values (make-list (fold-left
				   (lambda (sum rtd)
				     (+ sum (vector-length (record-type-field-names rtd))))
				   0
				   (record-parent-list rtd))
				  init))
	  (maker	(record-constructor (make-record-constructor-descriptor rtd #f #f))))
      (lambda ()
	(apply maker init-values))))))

(define-syntax make-record-maker*
  (syntax-rules ()
    ((_ ?record-name)
     (let-syntax
	 ((dummy (lambda (stx)
		   (syntax-case stx ()
		     ((_)
		      (with-syntax ((RTD (record-type-descriptor ?record-name)))
			#'(make-record-maker 'RTD)))))))
       (dummy)))

    ((_ ?record-name ?init)
     (let-syntax
	 ((dummy (lambda (stx)
		   (syntax-case stx ()
		     ((_ ?kontext)
		      (with-syntax ((RTD    (record-type-descriptor ?record-name))
				    ((INIT) (datum->syntax #'?kontext '(?init))))
			#'(make-record-maker 'RTD INIT)))))))
       (dummy ?record-name)))))


(define-syntax record-field-accessor*
  (syntax-rules ()
    ((_ ?record-name ?field-name)
     (let-syntax
	 ((dummy (lambda (stx)
		   (syntax-case stx ()
		     ((_)
		      (with-syntax
			  ((ACCESSOR (%record-field-accessor (quote ?record-name)
							     (record-type-descriptor ?record-name)
							     (quote ?field-name))))
			#'(quote ACCESSOR)))))))
       (dummy)))))

(define-syntax record-field-mutator*
  (syntax-rules ()
    ((_ ?record-name ?field-name)
     (let-syntax
	 ((dummy (lambda (stx)
		   (syntax-case stx ()
		     ((_)
		      (with-syntax
			  ((MUTATOR (%record-field-mutator (quote ?record-name)
							   (record-type-descriptor ?record-name)
							   (quote ?field-name)
							   #t)))
			#'(quote MUTATOR)))))))
       (dummy)))))


(define-syntax %define-record-thing
  (syntax-rules ()
    ((_ ?-name ?-what ?-which)
     (define-syntax ?-name
       (syntax-rules ()
	 ((_ ?record-name)
	  (?-name ?record-name ?record-name))
	 ((_ ?record-name ?context-identifier)
	  (let-syntax
	      ((dummy (lambda (stx)
			(syntax-case stx ()
			  ((_ ?kontext)
			   (with-syntax (((DEFINES ((... ...) (... ...)))
					  (make-define-forms (quote ?-what) (quote ?-which)
							     (syntax ?kontext)
							     (record-type-descriptor ?record-name))))
			     (syntax (begin DEFINES ((... ...) (... ...))))))))))
	    (dummy ?context-identifier))))))))

(%define-record-thing define-record-accessors			accessor all)
(%define-record-thing define-record-mutators			mutator all)
(%define-record-thing define-record-accessors&mutators		accessor&mutator all)

(%define-record-thing define-record-accessors/parents		accessor parent)
(%define-record-thing define-record-mutators/parents		mutator parent)
(%define-record-thing define-record-accessors&mutators/parents	accessor&mutator parent)

(%define-record-thing define-record-accessors/this		accessor this)
(%define-record-thing define-record-mutators/this		mutator this)
(%define-record-thing define-record-accessors&mutators/this	accessor&mutator this)


(define-syntax %define-with-record
  (syntax-rules ()
    ((_ ?-name ?-what)
     (define-syntax ?-name
       (syntax-rules ()
	 ((_ ?record-name (?field-name (... ...)) ?form0 ?form (... ...))
	  (let-syntax
	      ((dummy (lambda (stx)
			(syntax-case stx ()
			  ((_ ?kontext)
			   (with-syntax
			       (((BINDINGS ((... ...) (... ...)))
				 (make-bindings-for-with (quote ?-what) #'?kontext
							 (record-type-descriptor ?record-name)
							 '(?field-name (... ...))))
				((FORMS ((... ...) (... ...)))
				 (datum->syntax #'?kontext '(?form0 ?form (... ...)))))
			     (syntax (let (BINDINGS ((... ...) (... ...)))
				       FORMS ((... ...) (... ...))))))))))
	    (dummy ?record-name))))))))

(%define-with-record with-record-accessors accessor)
(%define-with-record with-record-mutators mutator)
(%define-with-record with-record-accessors&mutators accessor&mutator)


(define-syntax with-record-fields
  (syntax-rules ()
    ((_ () ?form0 ?form ...)
     (begin ?form0 ?form ...))

    ((_ ((((?var-name ?field-name) ...) ?record-name ?expr))
	?form0 ?form ...)
     (let-syntax
	 ((dummy (lambda (stx)
		   (syntax-case stx ()
		     ((_ ?kontext)
		      (let ((RTD (record-type-descriptor ?record-name)))
			(with-syntax
			    (((EXPR)
			      (datum->syntax #'?kontext '(?expr)))
			     ((VAR (... ...))
			      (datum->syntax #'?kontext '(?var-name ...)))
			     ((ACCESSOR (... ...))
			      `(,(%record-field-accessor '?record-name RTD '?field-name) ...))
			     ((MUTATOR (... ...))
			      `(,(%record-field-mutator  '?record-name RTD '?field-name) ...))
			     ((FORMS (... ...))
			      (datum->syntax #'?kontext '(?form0 ?form ...))))
			  #'(let ((the-record EXPR))
			      (let-syntax
			      	  ((VAR (identifier-syntax (_          ('ACCESSOR the-record))
							   ((set! _ e) ('MUTATOR  the-record e))))
			      	   (... ...))
				FORMS (... ...))))))))))
       (dummy ?record-name)))

    ((_ ((((?var-name ?field-name) ...) ?record-name ?expr) ?bindings ...) ?form0 ?form ...)
     (with-record-fields ((((?var-name ?field-name) ...) ?record-name ?expr))
       (with-record-fields (?bindings ...) ?form0 ?form ...)))

    ((_ (((?field-name ...) ?record-name ?expr) ?bindings ...) ?form0 ?form ...)
     (with-record-fields ((((?field-name ?field-name) ...) ?record-name ?expr))
       (with-record-fields (?bindings ...) ?form0 ?form ...)))

    ((_ ((?field-name ?record-name ?expr) ?bindings ...) ?form0 ?form ...)
     (with-record-fields ((((?field-name ?field-name)) ?record-name ?expr))
       (with-record-fields (?bindings ...) ?form0 ?form ...)))))


(define-syntax with-record-fields*
  (syntax-rules ()
    ((_ () ?form0 ?form ...)
     (begin ?form0 ?form ...))

    ((_ (((?field-name ...) ?record-name ?record-id))
	?form0 ?form ...)
     (let-syntax
	 ((dummy (lambda (stx)
		   (syntax-case stx ()
		     ((_ ?kontext)
		      (let ((RTD (record-type-descriptor ?record-name)))
			(with-syntax
			    (((RECORD-ID)
			      (datum->syntax #'?kontext '(?record-id)))
			     ((VAR (... ...))
			      (datum->syntax #'?kontext
					     (%record-field-identifiers '?record-id '(?field-name ...))))
			     ((ACCESSOR (... ...))
			      `(,(%record-field-accessor '?record-name RTD '?field-name) ...))
			     ((MUTATOR (... ...))
			      `(,(%record-field-mutator  '?record-name RTD '?field-name) ...))
			     ((FORMS (... ...))
			      (datum->syntax #'?kontext '(?form0 ?form ...))))
			  #'(let-syntax
				((VAR (identifier-syntax (_          ('ACCESSOR RECORD-ID))
							 ((set! _ e) ('MUTATOR  RECORD-ID e))))
				 (... ...))
			      FORMS (... ...)))))))))
       (dummy ?record-name)))

    ((_ (((?field-name ...) ?record-name ?record-id) ?bindings ...) ?form0 ?form ...)
     (with-record-fields* (((?field-name ...) ?record-name ?record-id))
       (with-record-fields* (?bindings ...) ?form0 ?form ...)))

    ((_ ((?field-name ?record-name ?record-id) ?bindings ...) ?form0 ?form ...)
     (with-record-fields* (((?field-name) ?record-name ?record-id))
       (with-record-fields* (?bindings ...) ?form0 ?form ...)))))


(define-syntax define-record-extension
  (syntax-rules (fields)
    ((_ ?record-name (fields (?field-name ?accessor ?mutator) ...))
     (let-syntax
	 ((dummy (lambda (stx)
		   (syntax-case stx ()
		     ((_ ?kontext)
		      (let ((RTD (record-type-descriptor ?record-name)))
			(record-extension-add! RTD
					       (quote ?field-name)
					       (datum->syntax #'?kontext '?accessor)
					       (datum->syntax #'?kontext '?mutator))
			...
			(syntax (define dummy #f))))))))
       (dummy ?record-name)))))


(define-syntax with-virtual-fields
  (syntax-rules ()
    ((_ () ?form0 ?form ...)
     (begin ?form0 ?form ...))

    ((_ ((((?var-name ?field-name) ...) ?record-name ?expr))
	?form0 ?form ...)
     (let-syntax
	 ((dummy (lambda (stx)
		   (syntax-case stx ()
		     ((_ ?kontext)
		      (let ((RTD (record-type-descriptor ?record-name)))
			(with-syntax
			    (((EXPR)
			      (datum->syntax #'?kontext '(?expr)))
			     ((VAR (... ...))
			      (datum->syntax #'?kontext '(?var-name ...)))
			     ((ACCESSOR (... ...))
			      `(,(%virtual-field-accessor '?record-name RTD '?field-name) ...))
			     ((MUTATOR (... ...))
			      `(,(%virtual-field-mutator  '?record-name RTD '?field-name) ...))
			     ((FORMS (... ...))
			      (datum->syntax #'?kontext '(?form0 ?form ...))))
			  #'(let ((the-record EXPR))
			      (let-syntax
			      	  ((VAR (identifier-syntax (_          (ACCESSOR the-record))
							   ((set! _ e) (MUTATOR  the-record e))))
			      	   (... ...))
				FORMS (... ...))))))))))
       (dummy ?record-name)))

    ((_ ((((?var-name ?field-name) ...) ?record-name ?expr) ?bindings ...) ?form0 ?form ...)
     (with-virtual-fields ((((?var-name ?field-name) ...) ?record-name ?expr))
       (with-virtual-fields (?bindings ...) ?form0 ?form ...)))

    ((_ (((?field-name ...) ?record-name ?expr) ?bindings ...) ?form0 ?form ...)
     (with-virtual-fields ((((?field-name ?field-name) ...) ?record-name ?expr))
       (with-virtual-fields (?bindings ...) ?form0 ?form ...)))

    ((_ ((?field-name ?record-name ?expr) ?bindings ...) ?form0 ?form ...)
     (with-virtual-fields ((((?field-name ?field-name)) ?record-name ?expr))
       (with-virtual-fields (?bindings ...) ?form0 ?form ...)))))


(define-syntax with-virtual-fields*
  (syntax-rules ()
    ((_ () ?form0 ?form ...)
     (begin ?form0 ?form ...))

    ((_ (((?field-name ...) ?record-name ?record-id))
	?form0 ?form ...)
     (let-syntax
	 ((dummy (lambda (stx)
		   (syntax-case stx ()
		     ((_ ?kontext)
		      (let ((RTD (record-type-descriptor ?record-name)))
			(with-syntax
			    (((RECORD-ID)
			      (datum->syntax #'?kontext '(?record-id)))
			     ((VAR (... ...))
			      (datum->syntax #'?kontext
					     (%record-field-identifiers '?record-id '(?field-name ...))))
			     ((ACCESSOR (... ...))
			      `(,(%virtual-field-accessor '?record-name RTD '?field-name) ...))
			     ((MUTATOR (... ...))
			      `(,(%virtual-field-mutator  '?record-name RTD '?field-name) ...))
			     ((FORMS (... ...))
			      (datum->syntax #'?kontext '(?form0 ?form ...))))
			  #'(let-syntax
				((VAR (identifier-syntax (_          (ACCESSOR RECORD-ID))
							 ((set! _ e) (MUTATOR  RECORD-ID e))))
				 (... ...))
			      FORMS (... ...)))))))))
       (dummy ?record-name)))

    ((_ (((?field-name ...) ?record-name ?record-id) ?bindings ...) ?form0 ?form ...)
     (with-virtual-fields* (((?field-name ...) ?record-name ?record-id))
       (with-virtual-fields* (?bindings ...) ?form0 ?form ...)))

    ((_ ((?field-name ?record-name ?record-id) ?bindings ...) ?form0 ?form ...)
     (with-virtual-fields* (((?field-name) ?record-name ?record-id))
       (with-virtual-fields* (?bindings ...) ?form0 ?form ...)))))


;;;; done

)

;;; end of file
