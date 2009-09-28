;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: helper functions for macros in (records)
;;;Date: Mon Sep 28, 2009
;;;
;;;Abstract
;;;
;;;	This  library  collects   helper  functions  for  the  (records)
;;;	library;  it is  meant to  be imported  by (records)  for expand
;;;	only.
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


(library (records helpers)
  (export
    make-define-forms		make-bindings-for-with
    field-name->accessor-name	field-name->mutator-name
    field-name->accessor&mutator-name
    vector-index)
  (import (rnrs))


;;;; helpers

(define (vector-index item vec)
  ;;Return the index of ITEM in the vector VEC, or #f if not found.
  ;;
  (let ((len (vector-length vec)))
    (let loop ((i 0))
      (and (< i len)
	   (if (eq? item (vector-ref vec i))
	       i
	     (loop (+ i 1)))))))

(define (field-name->accessor-name rtd-name field-name)
  ;;Given a record  type name (as symbol) and a  field name (as symbol),
  ;;return an identifier to be used as name for the field accessor.
  ;;
  (string->symbol (string-append (symbol->string rtd-name) "-"
				 (symbol->string field-name))))

(define (field-name->mutator-name rtd-name field-name)
  ;;Given a record  type name (as symbol) and a  field name (as symbol),
  ;;return an identifier to be used as name for the field mutator.
  ;;
  (string->symbol (string-append (symbol->string rtd-name) "-"
				 (symbol->string field-name) "-set!")))

(define field-name->accessor&mutator-name
  ;;Given a record  type name (as symbol) and a  field name (as symbol),
  ;;return an identifier  to be used as name for  the field accessor and
  ;;mutator function.
  ;;
  field-name->accessor-name)


(define (make-define-forms what which kontext rtd)
  ;;Main function for the macros:
  ;;
  ;;	define-record-accessors
  ;;	define-record-mutators
  ;;	define-record-accessors&mutators
  ;;
  ;;Return a syntax object associated to the lexical context represented
  ;;by the KONTEXT  identifier.  The object represents a  list of DEFINE
  ;;form to be  inserted into a BEGIN syntax; it  binds the record field
  ;;accessors or mutators for the fields of RTD.
  ;;
  ;;WHAT selects  the type of  bindings, it can  be one of  the symbols:
  ;;accessor, mutator, accessor&mutator.
  ;;
  (define (main rtd-name)
    (case which
      ((this)
       (make-definitions rtd-name rtd))
      ((all parent)
       (let loop ((rtd		(if (eq? 'all which)
				    rtd
				  (record-type-parent rtd)))
		  (result	'()))
	 (if (not rtd)
	     result
	   (loop (record-type-parent rtd)
		 (append result (make-definitions rtd-name rtd))))))))

  (define (make-definitions rtd-name rtd)
    (let ((len (vector-length (record-type-field-names rtd))))
      (let loop ((i    0)
		 (defs '()))
	(if (= i len)
	    defs
	  (loop (+ 1 i)
		(case what
		  ((accessor)
		   (cons (make-accessor kontext rtd-name rtd i)
			 defs))
		  ((mutator)
		   (if (record-field-mutable? rtd i)
		       (cons (make-mutator kontext rtd-name rtd i) defs)
		     defs))
		  ((accessor&mutator)
		   (cons ((if (record-field-mutable? rtd i)
			      make-accessor&mutator
			    make-accessor) kontext rtd-name rtd i)
			 defs))))))))

  (define (make-accessor kontext rtd-name rtd index)
    ;;Return  a   syntax  object  associated  to   the  lexical  context
    ;;represented by  the KONTEXT  identifier.  The object  represents a
    ;;single DEFINE  form; it  binds the record  field accessor  for the
    ;;field at INDEX in RTD.
    ;;
    (let ((accessor-name	(field-name->accessor&mutator-name
				 rtd-name (vector-ref (record-type-field-names rtd) index)))
	  (accessor-proc	(record-accessor rtd index)))
      (datum->syntax kontext `(define ,accessor-name ',accessor-proc))))

  (define (make-mutator kontext rtd-name rtd index)
    ;;Return  a   syntax  object  associated  to   the  lexical  context
    ;;represented by  the KONTEXT  identifier.  The object  represents a
    ;;single  DEFINE form;  it binds  the record  field mutator  for the
    ;;field at INDEX in RTD.
    ;;
    (let ((mutator-name		(field-name->mutator-name
				 rtd-name (vector-ref (record-type-field-names rtd) index)))
	  (mutator-proc		(record-mutator rtd index)))
      (datum->syntax kontext `(define ,mutator-name ',mutator-proc))))

  (define (make-accessor&mutator kontext rtd-name rtd index)
    ;;Return  a   syntax  object  associated  to   the  lexical  context
    ;;represented by  the KONTEXT  identifier.  The object  represents a
    ;;single  DEFINE form;  it binds  an  accessor and  mutator for  the
    ;;record field INDEX in RTD.
    ;;
    (let ((name			(field-name->accessor&mutator-name
				 rtd-name (vector-ref (record-type-field-names rtd) index)))
	  (accessor-proc	(record-accessor rtd index))
	  (mutator-proc		(record-mutator  rtd index)))
      (datum->syntax kontext `(define ,name
				(case-lambda
				 ((o)
				  (',accessor-proc o))
				 ((o v)
				  (',mutator-proc o v)))))))

  (main (record-type-name rtd)))


(define (make-bindings-for-with what kontext rtd field-names)
  ;;Main function for the macros:
  ;;
  ;;	with-record-accessors
  ;;	with-record-mutators
  ;;	with-record-accessors&mutators
  ;;
  ;;Return a syntax object associated to the lexical context represented
  ;;by the KONTEXT identifier.  The object represents a list of bindings
  ;;for a  LET syntax; it binds  the record field  accessors or mutators
  ;;for the fields of RTD whose names are listed in FIELD-NAMES.
  ;;
  ;;WHAT selects  the type of  bindings, it can  be one of  the symbols:
  ;;accessor, mutator, accessor&mutator.
  ;;
  (define (main rtd-name)
    (let loop ((rtd		rtd)
	       (field-names	field-names)
	       (result		'()))
      (cond ((null? field-names)
	     result)
	    (rtd
	     (let-values (((bindings rest-fields)
			   (make-bindings what kontext rtd-name rtd field-names)))
	       (loop (record-type-parent rtd)
		     rest-fields
		     (append result bindings))))
	    (else
	     (assertion-violation #f
	       (string-append "unknown field names in record type hierarchy of \""
			      (symbol->string rtd-name) "\"")
	       field-names)))))

  (define (make-bindings what kontext rtd-name rtd field-names)
    (let ((slots (record-type-field-names rtd)))
      (let loop ((field-names	field-names)
		 (bindings	'())
		 (rest-names	'()))
	(if (null? field-names)
	    (values bindings rest-names)
	  (let* ((name		(car field-names))
		 (idx		(vector-index name slots))
		 (field-names	(cdr field-names)))
	    (if (not idx)
		(loop field-names bindings (cons name rest-names))
	      (case what
		((accessor)
		 (loop field-names
		       (cons (make-with-accessor-binding kontext rtd-name rtd idx)
			     bindings)
		       rest-names))
		((mutator)
		 (if (record-field-mutable? rtd idx)
		     (loop field-names
			   (cons (make-with-mutator-binding kontext rtd-name rtd idx)
				 bindings)
			   rest-names)
		   (assertion-violation #f
		     (string-append "attempt to create mutator for immutable record field of \""
				    (symbol->string rtd-name) "\"")
		     (vector-ref slots idx))))
		((accessor&mutator)
		 (loop field-names
		       (cons ((if (record-field-mutable? rtd idx)
				  make-with-accessor&mutator-binding
				make-with-accessor-binding)
			      kontext rtd-name rtd idx)
			     bindings)
		       rest-names)))))))))

  (define (make-with-accessor-binding kontext rtd-name rtd index)
    ;;Return  a   syntax  object  associated  to   the  lexical  context
    ;;represented by  the KONTEXT  identifier.  The object  represents a
    ;;single  binding  for a  LET  syntax;  it  binds the  record  field
    ;;accessor for the field at INDEX in RTD.
    ;;
    (let ((accessor-name (field-name->accessor-name rtd-name
						    (vector-ref (record-type-field-names rtd) index)))
	  (accessor-proc (record-accessor rtd index)))
      (datum->syntax kontext `(,accessor-name ',accessor-proc))))

  (define (make-with-mutator-binding kontext rtd-name rtd index)
    ;;Return  a   syntax  object  associated  to   the  lexical  context
    ;;represented by  the KONTEXT  identifier.  The object  represents a
    ;;single binding for a LET syntax; it binds the record field mutator
    ;;for the field at INDEX in RTD.
    ;;
    (let ((mutator-name (field-name->mutator-name rtd-name
						  (vector-ref (record-type-field-names rtd) index)))
	  (mutator-proc (record-mutator rtd index)))
      (datum->syntax kontext `(,mutator-name ',mutator-proc))))

  (define (make-with-accessor&mutator-binding kontext rtd-name rtd i)
    ;;Return  a   syntax  object  associated  to   the  lexical  context
    ;;represented by  the KONTEXT  identifier.  The object  represents a
    ;;single  binding for a  LET syntax;  it binds  the an  accessor and
    ;;mutator closure for the field at INDEX in RTD.
    ;;
    (let ((name		(field-name->accessor&mutator-name
			 rtd-name (vector-ref (record-type-field-names rtd) i)))
	  (accessor-proc	(record-accessor rtd i))
	  (mutator-proc	(record-mutator  rtd i)))
      (datum->syntax kontext `(,name (case-lambda
				      ((o)
				       (',accessor-proc o))
				      ((o v)
				       (',mutator-proc o v)))))))

  (main (record-type-name rtd)))


;;;; done

)

;;; end of file
