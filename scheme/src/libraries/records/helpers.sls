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
    record-parent-list		make-record-maker
    record-field-accessor	record-field-mutator
    make-record-extension
    virtual-field-accessor	virtual-field-mutator
    make-define-forms		make-bindings-for-with
    %record-predicate		%record-field-identifiers)
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


;;; helpers

(define (%field-name->accessor-name rtd-name field-name)
  ;;Given a record  type name (as symbol) and a  field name (as symbol),
  ;;return an identifier to be used as name for the field accessor.
  ;;
  (string->symbol (string-append (symbol->string rtd-name) "-"
				 (symbol->string field-name))))

(define (%field-name->mutator-name rtd-name field-name)
  ;;Given a record  type name (as symbol) and a  field name (as symbol),
  ;;return an identifier to be used as name for the field mutator.
  ;;
  (string->symbol (string-append (symbol->string rtd-name) "-"
				 (symbol->string field-name) "-set!")))

(define %field-name->accessor&mutator-name
  ;;Given a record  type name (as symbol) and a  field name (as symbol),
  ;;return an identifier  to be used as name for  the field accessor and
  ;;mutator function.
  ;;
  %field-name->accessor-name)

(define (%record-predicate rtd)
  ;;Return the  record type predicate  associated to RTD.   Support both
  ;;normal record types and conventional record types.
  ;;
  (case (record-type-name rtd)
    ((<fixnum>)			fixnum?)
    ((<integer>)		integer?)
    ((<rational>)		rational?)
    ((<integer-valued>)		integer-valued?)
    ((<rational-valued>)	rational-valued?)
    ((<flonum>)			flonum?)
    ((<real>)			real?)
    ((<real-valued>)		real-valued?)
    ((<complex>)		complex?)
    ((<number>)			number?)

    ((<char>)			char?)
    ((<string>)			string?)
    ((<vector>)			vector?)
    ((<bytevector>)		bytevector?)
    ((<hashtable>)		hashtable?)

    ((<input-port>)		input-port?)
    ((<output-port>)		output-port?)
    ((<binary-port>)		(lambda (obj)
				  (and (port? obj) (binary-port? obj))))
    ((<textual-port>)		(lambda (obj)
				  (and (port? obj) (textual-port? obj))))
    ((<port>)			port?)

    ((<condition>)		condition?)
    ((<record>)			record?)
    ((<pair>)			pair?)
    ((<list>)			list?)

    (else
     (record-predicate rtd))))


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

(define (record-parent-list rtd)
  (let loop ((cls (list rtd))
	     (rtd (record-type-parent rtd)))
    (if rtd
	(loop (cons rtd cls) (record-type-parent rtd))
      (reverse cls))))


(define record-field-accessor
  (case-lambda
   ((rtd field-name)
    (record-field-accessor rtd field-name (record-type-name rtd)))
   ((rtd field-name rtd-name)
    (if rtd
	(let ((idx (vector-index field-name (record-type-field-names rtd))))
	  (if idx
	      (record-accessor rtd idx)
	    (record-field-accessor (record-type-parent rtd) field-name rtd-name)))
      (assertion-violation 'record-field-accessor
	(string-append "unknown field name in record type hierarchy of \""
		       (symbol->string rtd-name) "\"")
	field-name)))))

(define record-field-mutator
  (case-lambda
   ((rtd field-name)
    (record-field-mutator rtd field-name #t (record-type-name rtd)))
   ((rtd field-name false-if-immutable)
    (record-field-mutator rtd field-name false-if-immutable (record-type-name rtd)))
   ((rtd field-name false-if-immutable rtd-name)
    (if rtd
	(let ((idx (vector-index field-name (record-type-field-names rtd))))
	  (cond ((not idx)
		 (record-field-mutator (record-type-parent rtd) field-name
				       false-if-immutable rtd-name))
		((record-field-mutable? rtd idx)
		 (record-mutator rtd idx))
		(else
		 (if false-if-immutable
		     #f
		   (lambda (dummy-a dummy-b)
		     (assertion-violation 'record-field-mutator
		       (string-append "attempt to mutate immutable field of record \""
				      (symbol->string (record-type-name rtd))
				      "\" in record hierarchy of \""
				      (symbol->string rtd-name) "\"")
		       field-name))))))
      (assertion-violation 'record-field-mutator
	(string-append "unknown field name in record type hierarchy of \""
		       (symbol->string rtd-name) "\"")
	field-name)))))


(define-record-type <record-extension>
  (fields (immutable rtd)
	  (immutable field-table)))

(define-record-type <virtual-field>
  (fields (immutable name)
	  (immutable accessor)
	  (immutable mutator)))

;;(define *record-extensions*
;;  (make-eq-hashtable))

(define (make-record-extension rtd fields)
  (let* ((ext   (make-<record-extension> rtd (make-eq-hashtable)))
	 (table (<record-extension>-field-table ext)))
    (for-each (lambda (field)
		(let ((name (car field)))
		  (hashtable-set! table name
				  (make-<virtual-field> name (cadr field) (caddr field)))))
      fields)
;;    (hashtable-set! *record-extensions* rtd ext)
    ext))

(define virtual-field-accessor
  ;;Return a syntax  object expanding to a procedure  object which, when
  ;;applied to  a value of conventional  type RTD, returns  the value of
  ;;the virtual  field whose name is  FIELD-NAME.  RTD must  be a record
  ;;type descriptor, FIELD-NAME must be a Scheme symbol.
  ;;
  ;;RTD-NAME must be a Scheme symbol representing the name of the record
  ;;type RTD, it is used only for error messages.
  ;;
  (case-lambda
   ((ext field-name)
    (virtual-field-accessor ext field-name (record-type-name (record-rtd ext))))
   ((ext field-name ext-name)
    (if ext
	(let* ((field-table (<record-extension>-field-table ext))
	       (field       (hashtable-ref field-table field-name #f)))
	  (if field
	      (let ((proc (<virtual-field>-accessor field)))
		(or proc
		    (lambda args
		      (assertion-violation #f
			(string-append "attempt to access unreadable virtual field \""
				       (symbol->string field-name)
				       "\" for record \"" (symbol->string ext-name) "\"")
			field-name))))
	    (virtual-field-accessor (record-type-parent ext) field-name ext-name)))
      (assertion-violation #f
	(string-append "unknown virtual field name \"" (symbol->string field-name)
		       "\" in record type hierarchy of \"" (symbol->string ext-name) "\"")
	field-name)))))

(define virtual-field-mutator
  ;;Return a syntax  object expanding to a procedure  object which, when
  ;;applied to  a value  of conventional type  EXT and a  Scheme object,
  ;;mutates the  value of  the virtual field  whose name  is FIELD-NAME.
  ;;EXT must  be a record type  descriptor, FIELD-NAME must  be a Scheme
  ;;symbol.
  ;;
  ;;EXT-NAME must be a Scheme symbol representing the name of the record
  ;;type EXT, it is used only for error messages.
  ;;
  (case-lambda
   ((ext field-name)
    (virtual-field-mutator ext field-name #t (record-type-name (record-rtd ext))))
   ((ext field-name false-if-immutable)
    (virtual-field-mutator ext field-name false-if-immutable (record-type-name (record-rtd ext))))
   ((ext field-name false-if-immutable ext-name)
    (if ext
	(let* ((field-table (<record-extension>-field-table ext))
	       (field       (hashtable-ref field-table field-name #f)))
	  (if field
	      (let ((proc (<virtual-field>-mutator field)))
		(or proc
		    (if false-if-immutable
			#f
		      (lambda args
			(assertion-violation #f
			  (string-append "attempt to access immutable virtual field \""
					 (symbol->string field-name)
					 "\" for record \""
					 (symbol->string (record-type-name ext)) "\"")
			  field-name)))))
	    (virtual-field-mutator (record-type-parent ext) field-name false-if-immutable ext-name)))
      (assertion-violation #f
	(string-append "unknown virtual field name \"" (symbol->string field-name)
		       "\" in record type hierarchy of \"" (symbol->string ext-name) "\"")
	field-name)))))


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
    (let ((accessor-name	(%field-name->accessor&mutator-name
				 rtd-name (vector-ref (record-type-field-names rtd) index)))
	  (accessor-proc	(record-accessor rtd index)))
      (datum->syntax kontext `(define ,accessor-name ',accessor-proc))))

  (define (make-mutator kontext rtd-name rtd index)
    ;;Return  a   syntax  object  associated  to   the  lexical  context
    ;;represented by  the KONTEXT  identifier.  The object  represents a
    ;;single  DEFINE form;  it binds  the record  field mutator  for the
    ;;field at INDEX in RTD.
    ;;
    (let ((mutator-name		(%field-name->mutator-name
				 rtd-name (vector-ref (record-type-field-names rtd) index)))
	  (mutator-proc		(record-mutator rtd index)))
      (datum->syntax kontext `(define ,mutator-name ',mutator-proc))))

  (define (make-accessor&mutator kontext rtd-name rtd index)
    ;;Return  a   syntax  object  associated  to   the  lexical  context
    ;;represented by  the KONTEXT  identifier.  The object  represents a
    ;;single  DEFINE form;  it binds  an  accessor and  mutator for  the
    ;;record field INDEX in RTD.
    ;;
    (let ((name			(%field-name->accessor&mutator-name
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
    (let ((accessor-name (%field-name->accessor-name rtd-name
						    (vector-ref (record-type-field-names rtd) index)))
	  (accessor-proc (record-accessor rtd index)))
      (datum->syntax kontext `(,accessor-name ',accessor-proc))))

  (define (make-with-mutator-binding kontext rtd-name rtd index)
    ;;Return  a   syntax  object  associated  to   the  lexical  context
    ;;represented by  the KONTEXT  identifier.  The object  represents a
    ;;single binding for a LET syntax; it binds the record field mutator
    ;;for the field at INDEX in RTD.
    ;;
    (let ((mutator-name (%field-name->mutator-name rtd-name
						   (vector-ref (record-type-field-names rtd) index)))
	  (mutator-proc (record-mutator rtd index)))
      (datum->syntax kontext `(,mutator-name ',mutator-proc))))

  (define (make-with-accessor&mutator-binding kontext rtd-name rtd i)
    ;;Return  a   syntax  object  associated  to   the  lexical  context
    ;;represented by  the KONTEXT  identifier.  The object  represents a
    ;;single  binding for a  LET syntax;  it binds  the an  accessor and
    ;;mutator closure for the field at INDEX in RTD.
    ;;
    (let ((name		(%field-name->accessor&mutator-name
			 rtd-name (vector-ref (record-type-field-names rtd) i)))
	  (accessor-proc	(record-accessor rtd i))
	  (mutator-proc	(record-mutator  rtd i)))
      (datum->syntax kontext `(,name (case-lambda
				      ((o)
				       (',accessor-proc o))
				      ((o v)
				       (',mutator-proc o v)))))))

  (main (record-type-name rtd)))


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


;;;; done

)

;;; end of file
