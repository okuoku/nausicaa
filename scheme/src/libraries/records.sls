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

    with-record-fields)
  (import (rnrs)
    (for (records helpers) run expand))


;;;; helpers

(define-syntax make-list
  (syntax-rules ()
    ((_ ?len ?fill)
     (let ((len ?len))
       (do ((i 0 (+ 1 i))
	    (result '() (cons ?fill result)))
	   ((= i ?len)
	    result))))))


(define-syntax record-parent-list*
  (syntax-rules ()
    ((_ ?record-name)
     (record-parent-list (record-type-descriptor ?record-name)))))

(define (record-parent-list rtd)
  (let loop ((cls `(,rtd))
	     (rtd (record-type-parent rtd)))
    (if rtd
	(loop (cons rtd cls) (record-type-parent rtd))
      (reverse cls))))


(define-syntax make-record-maker*
  (syntax-rules ()
    ((_ ?record-name)
     (make-record-maker (record-type-descriptor ?record-name)))

    ((_ ?record-name ?init)
     (make-record-maker (record-type-descriptor ?record-name) ?init))))

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


(define-syntax record-field-accessor*
  (syntax-rules ()
    ((_ ?record-name ?field-name)
     (record-field-accessor (record-type-descriptor ?record-name) (quote ?field-name)))))

(define (record-field-accessor rtd field-name)
  (if rtd
      (let ((idx (vector-index field-name (record-type-field-names rtd))))
	(if idx
	    (record-accessor rtd idx)
	  (record-field-accessor (record-type-parent rtd) field-name)))
    (assertion-violation 'record-field-accessor
      "unknown field name in record type hierarchy"
      field-name)))

(define-syntax record-field-mutator*
  (syntax-rules ()
    ((_ ?record-name ?field-name)
     (record-field-mutator (record-type-descriptor ?record-name) (quote ?field-name)))))

(define (record-field-mutator rtd field-name)
  (if rtd
      (let ((idx (vector-index field-name (record-type-field-names rtd))))
	(if idx
	    (and (record-field-mutable? rtd idx)
		 (record-mutator rtd idx))
	  (record-field-mutator (record-type-parent rtd) field-name)))
    (assertion-violation 'record-field-mutator
      "unknown field name in record type hierarchy"
      field-name)))


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

		   (define (%record-field-accessor rtd-name rtd field-name)
		     (if rtd
			 (let ((idx (vector-index field-name (record-type-field-names rtd))))
			   (if idx
			       (record-accessor rtd idx)
			     (%record-field-accessor rtd-name (record-type-parent rtd) field-name)))
		       (assertion-violation #f
			 (string-append "unknown field name in record type hierarchy of \""
					(symbol->string rtd-name) "\"")
			 field-name)))

		   (define (%record-field-mutator rtd-name rtd field-name)
		     (if rtd
			 (let ((idx (vector-index field-name (record-type-field-names rtd))))
			   (cond ((not idx)
				  (%record-field-mutator rtd-name (record-type-parent rtd) field-name))
				 ((record-field-mutable? rtd idx)
				  (record-mutator rtd idx))
				 (else
				  (lambda args
				    (assertion-violation #f
				      (string-append "attempt to mutate immutable field for record \""
						     (symbol->string (record-type-name rtd)) "\"")
				      field-name)))))
		       (assertion-violation #f
			 (string-append "unknown field name in record type hierarchy of \""
					(symbol->string rtd-name) "\"")
			 field-name)))

		   (syntax-case stx ()
		     ((_ ?kontext)
		      (with-syntax
			  (((EXPR)	(datum->syntax #'?kontext '(?expr)))
			   ;; ((FIELD (... ...))
			   ;;  (datum->syntax #'?kontext '(?field-name ...)))
			   ((VAR (... ...))
			    (datum->syntax #'?kontext '(?var-name ...)))
			   ((ACCESSOR (... ...))
			    (datum->syntax #'?kontext
					   (list
					    (%record-field-accessor (quote ?record-name)
								    (record-type-descriptor ?record-name)
								    (quote ?field-name))
					    ...)))
			   ((MUTATOR (... ...))
			    (datum->syntax #'?kontext
					   (list
					    (%record-field-mutator  (quote ?record-name)
								    (record-type-descriptor ?record-name)
								    (quote ?field-name))
					    ...)))
			   ((FORMS (... ...))
			    (datum->syntax #'?kontext '(?form0 ?form ...))))
			(syntax (let ((the-record EXPR))
				  (let-syntax
				      ((VAR (identifier-syntax
					     (_			('ACCESSOR the-record))
					     ((set! _ e)	('MUTATOR the-record e))))
				       (... ...))
				    FORMS (... ...))))))))))
       (dummy ?record-name)))

    ((_ ((((?var-name0 ?field-name0) ...) ?record-name0 ?expr0) ?bindings ...) ?form0 ?form ...)
     (with-record-fields ((((?var-name0 ?field-name0) ...) ?record-name0 ?expr0))
       (with-record-fields (?bindings ...) ?form0 ?form ...)))

    ((_ (((?field-name0 ...) ?record-name0 ?expr0) ?bindings ...) ?form0 ?form ...)
     (with-record-fields ((((?field-name0 ?field-name0) ...) ?record-name0 ?expr0))
       (with-record-fields (?bindings ...) ?form0 ?form ...)))

    ((_ ((?field-name0 ?record-name0 ?expr0) ?bindings ...) ?form0 ?form ...)
     (with-record-fields ((((?field-name0 ?field-name0)) ?record-name0 ?expr0))
       (with-record-fields (?bindings ...) ?form0 ?form ...)))))


;;;; done

)

;;; end of file
