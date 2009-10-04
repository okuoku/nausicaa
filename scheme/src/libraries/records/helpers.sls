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
    record-parent-list
    record-field-accessor	record-field-mutator
    make-define-forms		make-bindings-for-with
    field-name->accessor-name	field-name->mutator-name
    field-name->accessor&mutator-name
    %record-field-accessor	%record-field-mutator
    %record-field-identifiers
    %virtual-field-accessor	%virtual-field-mutator
    record-extension-add!
    make-list vector-index)
  (import (rnrs))


;;;; utilities

(define-syntax make-list
  (syntax-rules ()
    ((_ ?len ?fill)
     (let ((len ?len))
       (do ((i 0 (+ 1 i))
	    (result '() (cons ?fill result)))
	   ((= i ?len)
	    result))))))

(define (vector-index item vec)
  ;;Return the index of ITEM in the vector VEC, or #f if not found.
  ;;
  (let ((len (vector-length vec)))
    (let loop ((i 0))
      (and (< i len)
	   (if (eq? item (vector-ref vec i))
	       i
	     (loop (+ i 1)))))))

(define delete-duplicates
  (case-lambda
   ((lis)
    (delete-duplicates lis equal?))
   ((lis elt=)
    (let recur ((lis lis))
      (if (null? lis) lis
	(let* ((x (car lis))
	       (tail (cdr lis))
	       (new-tail (recur (delete x tail elt=))))
	  (if (eq? tail new-tail) lis (cons x new-tail))))))))

(define delete
  (case-lambda
   ((x lis)
    (delete x lis equal?))
   ((x lis =)
    (filter (lambda (y) (not (= x y))) lis))))


;;; helpers

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


(define (record-parent-list rtd)
  (let loop ((cls (list rtd))
	     (rtd (record-type-parent rtd)))
    (if rtd
	(loop (cons rtd cls) (record-type-parent rtd))
      (reverse cls))))

(define (record-field-accessor rtd field-name)
  (if rtd
      (let ((idx (vector-index field-name (record-type-field-names rtd))))
	(if idx
	    (record-accessor rtd idx)
	  (record-field-accessor (record-type-parent rtd) field-name)))
    (assertion-violation 'record-field-accessor
      "unknown field name in record type hierarchy"
      field-name)))

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


;;;;helpers for WITH-RECORD-FIELDS and WITH-RECORD-FIELDS*

(define (%record-field-identifiers record-id field-names)
  ;;Return a list  of conventional names associated to  a record binding
  ;;and  a list of  record fields.   RECORD-ID must  be a  Scheme symbol
  ;;representing  an  identifier  to  which  a  record  will  be  bound;
  ;;FIELD-NAMES must be a list  of Scheme symbols representing the names
  ;;of fields in a record type.
  ;;
  ;;Example:
  ;;
  ;;	(define-record-type <alpha>
  ;;	  (fields (mutable a)
  ;;	          (mutable b)))
  ;;
  ;;	(%record-field-identifiers 'thing '(a b))
  ;;	=> (thing.a thing.b)
  ;;
  ;;	(define thing (make-<alpha> 1 2))
  ;;
  (let ((record-id (symbol->string record-id)))
    (map (lambda (name)
	   (string->symbol (string-append record-id "." (symbol->string name))))
      field-names)))

(define (%record-field-accessor rtd-name rtd field-name)
  ;;Return  a function  which, when  applied to  a record  of  type RTD,
  ;;returns the value  of the field whose name  is FIELD-NAME.  RTD must
  ;;be a record type descriptor, FIELD-NAME must be a Scheme symbol.
  ;;
  ;;RTD-NAME must be a Scheme symbol representing the name of the record
  ;;type RTD, it is used only for error messages.
  ;;
  (if rtd
;;;      (let ((ext (hashtable-ref *record-extensions* rtd #f)))
	;;; (if (and ext (assq field-name ext))
	;;;     (cadr (assq field-name ext))
	  (let ((idx (vector-index field-name (record-type-field-names rtd))))
	    (if idx
		(record-accessor rtd idx)
	      (%record-field-accessor rtd-name (record-type-parent rtd) field-name)))
	  ;;;))
    (assertion-violation #f
      (string-append "unknown field name in record type hierarchy of \""
		     (symbol->string rtd-name) "\"")
      field-name)))

(define %record-field-mutator
  ;;Return a function which, when applied  to a record of type RTD and a
  ;;Scheme  object,  mutates  the  value  of the  field  whose  name  is
  ;;FIELD-NAME.  RTD  must be a record type  descriptor, FIELD-NAME must
  ;;be a Scheme symbol.
  ;;
  ;;RTD-NAME must be a Scheme symbol representing the name of the record
  ;;type RTD, it is used only for error messages.
  ;;
  (case-lambda
   ((rtd-name rtd field-name)
    (%record-field-mutator rtd-name rtd field-name #f))
   ((rtd-name rtd field-name false-if-not-found)
    (if rtd
	;;; (let ((ext (hashtable-ref *record-extensions* rtd #f)))
	;;;   (if (and ext (assq field-name ext))
	;;;       (caddr (assq field-name ext))
	    (let ((idx (vector-index field-name (record-type-field-names rtd))))
	      (cond ((not idx)
		     (%record-field-mutator rtd-name (record-type-parent rtd) field-name
					    false-if-not-found))
		    ((record-field-mutable? rtd idx)
		     (record-mutator rtd idx))
		    (else
		     (if false-if-not-found
			 #f
		       (lambda args
			 (assertion-violation #f
			   (string-append "attempt to mutate immutable field for record \""
					  (symbol->string (record-type-name rtd)) "\"")
			   field-name))))))
      ;;;))
      (assertion-violation #f
	(string-append "unknown field name in record type hierarchy of \""
		       (symbol->string rtd-name) "\"")
	field-name)))))


;;;;helpers for WITH-VIRTUAL-FIELDS and WITH-VIRTUAL-FIELDS*

(define-record-type <virtual-field>
  (fields (immutable name)
	  (immutable accessor)
	  (immutable mutator)))

(define *record-extensions*
  (make-eq-hashtable))

(define (record-extension-add! rtd field-name accessor-stx mutator-stx)
  (let-syntax
      ((store-it (syntax-rules ()
		   ((_ ?table)
		    (hashtable-set! ?table field-name
				    (make-<virtual-field> field-name accessor-stx mutator-stx))))))
    (let ((field-table (hashtable-ref *record-extensions* rtd #f)))
      (if field-table
	  (store-it field-table)
	(let ((field-table (make-eq-hashtable)))
	  (store-it field-table)
	  (hashtable-set! *record-extensions* rtd field-table))))))

(define (%virtual-field-accessor rtd-name rtd field-name)
  ;;Return a syntax object expanding  to a procedure which, when applied
  ;;to  a value  of  conventional type  RTD,  returns the  value of  the
  ;;virtual field whose  name is FIELD-NAME.  RTD must  be a record type
  ;;descriptor, FIELD-NAME must be a Scheme symbol.
  ;;
  ;;RTD-NAME must be a Scheme symbol representing the name of the record
  ;;type RTD, it is used only for error messages.
  ;;
  (if rtd
      (let* ((rtd-table	(hashtable-ref *record-extensions* rtd #f))
	     (field	(and rtd-table (hashtable-ref rtd-table field-name #f))))
	(if field
	    (let ((proc (<virtual-field>-accessor field)))
	      (or proc
		  (lambda args
		    (assertion-violation #f
		      (string-append "attempt to access unreadable virtual field \""
				     (symbol->string field-name)
				     "\" for record \""
				     (symbol->string (record-type-name rtd)) "\"")
		      field-name))))
	  (%virtual-field-accessor rtd-name (record-type-parent rtd) field-name)))
    (assertion-violation #f
      (string-append "unknown virtual field name \"" (symbol->string field-name)
		     "\" in record type hierarchy of \"" (symbol->string rtd-name) "\"")
      field-name)))

(define (%virtual-field-mutator rtd-name rtd field-name)
  ;;Return a syntax object expanding  to a procedure which, when applied
  ;;to a value of conventional type RTD and a Scheme object, mutates the
  ;;value of the virtual field whose  name is FIELD-NAME.  RTD must be a
  ;;record type descriptor, FIELD-NAME must be a Scheme symbol.
  ;;
  ;;RTD-NAME must be a Scheme symbol representing the name of the record
  ;;type RTD, it is used only for error messages.
  ;;
  (if rtd
      (let* ((rtd-table	(hashtable-ref *record-extensions* rtd #f))
	     (field	(and rtd-table (hashtable-ref rtd-table field-name #f))))
	(if field
	    (let ((proc (<virtual-field>-mutator field)))
	      (or proc
		  (lambda args
		    (assertion-violation #f
		      (string-append "attempt to access immutable virtual field \""
				     (symbol->string field-name)
				     "\" for record \""
				     (symbol->string (record-type-name rtd)) "\"")
		      field-name))))
	  (%virtual-field-mutator rtd-name (record-type-parent rtd) field-name)))
    (assertion-violation #f
      (string-append "unknown virtual field name \"" (symbol->string field-name)
		     "\" in record type hierarchy of \"" (symbol->string rtd-name) "\"")
      field-name)))


;;;; done

)

;;; end of file
