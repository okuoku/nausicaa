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
    define-record-accessors		define-record-mutators
    define-record-accessors/this	define-record-mutators/this
    define-record-accessors/parents	define-record-mutators/parents

    define-record-accessors/mutators
    ;; define-record-accessors/mutators/this
    ;; define-record-accessors/mutators/parents

    with-record-accessors
    with-record-mutators
    with-record-accessors/mutators)
  (import (rnrs))


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


(define-syntax define-record-accessors
  (syntax-rules ()
    ((_ ?record-name)
     (define-record-accessors ?record-name ?record-name))
    ((_ ?record-name ?context-identifier)
     (let-syntax
	 ((A (lambda (stx)

	       (define (make-accessors-forms kontext rtd)
		 (let ((rtd-name (symbol->string (record-type-name rtd))))
		   (let process-rtd ((rtd	rtd)
				     (result	'()))
		     (if rtd
			 (let* ((slots	(record-type-field-names rtd))
				(len	(vector-length slots)))
			   (let make-def ((i    0)
					  (defs '()))
			     (if (= i len)
				 (process-rtd (record-type-parent rtd)
					      (append result defs))
			       (make-def (+ 1 i)
					 (cons (make-accessor kontext rtd-name rtd slots i)
					       defs)))))
		       result))))

	       (define (make-accessor kontext rtd-name rtd slots i)
		 (let ((accessor-name (slot-name->accessor-name rtd-name (vector-ref slots i)))
		       (accessor-proc (record-accessor rtd i)))
		   (datum->syntax kontext `(define ,accessor-name ',accessor-proc))))

	       (define (slot-name->accessor-name rtd-name slot-name)
		 (string->symbol (string-append rtd-name "-" (symbol->string slot-name))))

	       (syntax-case stx ()
		 ((_ ?kontext)
		  (with-syntax (((B (... ...))
				 (make-accessors-forms (syntax ?kontext)
						       (record-type-descriptor ?record-name))))
		    (syntax (begin B (... ...)))))))))
       (A ?context-identifier)))))


(define-syntax define-record-mutators
  (syntax-rules ()
    ((_ ?record-name)
     (define-record-mutators ?record-name ?record-name))
    ((_ ?record-name ?context-identifier)
     (let-syntax
	 ((A (lambda (stx)

	       (define (make-mutators-forms kontext rtd)
		 (let ((rtd-name (symbol->string (record-type-name rtd))))
		   (let process-rtd ((rtd	rtd)
				     (result	'()))
		     (if rtd
			 (let* ((slots	(record-type-field-names rtd))
				(len	(vector-length slots)))
			   (let make-def ((i    0)
					  (defs '()))
			     (if (= i len)
				 (process-rtd (record-type-parent rtd)
					      (append result defs))
			       (make-def (+ 1 i)
					 (if (record-field-mutable? rtd i)
					     (cons (make-mutator kontext rtd-name rtd slots i) defs)
					   defs)))))
		       result))))

	       (define (make-mutator kontext rtd-name rtd slots i)
		 (let ((mutator-name (slot-name->mutator-name rtd-name (vector-ref slots i)))
		       (mutator-proc (record-mutator rtd i)))
		   (datum->syntax kontext `(define ,mutator-name ',mutator-proc))))

	       (define (slot-name->mutator-name rtd-name slot-name)
		 (string->symbol (string-append rtd-name "-" (symbol->string slot-name) "-set!")))

	       (syntax-case stx ()
		 ((_ ?kontext)
		  (with-syntax (((B (... ...))
				 (make-mutators-forms (syntax ?kontext)
						      (record-type-descriptor ?record-name))))
		    (syntax (begin B (... ...)))))))))
       (A ?context-identifier)))))


(define-syntax define-record-accessors/parents
  (syntax-rules ()
    ((_ ?record-name)
     (define-record-accessors/parents ?record-name ?record-name))
    ((_ ?record-name ?context-identifier)
     (let-syntax
	 ((A (lambda (stx)

	       (define (make-accessors-forms kontext rtd)
		 (let ((rtd-name (symbol->string (record-type-name rtd))))
		   (let process-rtd ((rtd	(record-type-parent rtd))
				     (result    '()))
		     (if rtd
			 (let* ((slots	(record-type-field-names rtd))
				(len	(vector-length slots)))
			   (let make-def ((i    0)
					  (defs '()))
			     (if (= i len)
				 (process-rtd (record-type-parent rtd)
					      (append result defs))
			       (make-def (+ 1 i)
					 (cons (make-accessor kontext rtd-name rtd slots i)
					       defs)))))
		       result))))

	       (define (make-accessor kontext rtd-name rtd slots i)
		 (let ((accessor-name (slot-name->accessor-name rtd-name (vector-ref slots i)))
		       (accessor-proc (record-accessor rtd i)))
		   (datum->syntax kontext `(define ,accessor-name ',accessor-proc))))

	       (define (slot-name->accessor-name rtd-name slot-name)
		 (string->symbol (string-append rtd-name "-" (symbol->string slot-name))))

	       (syntax-case stx ()
		 ((_ ?kontext)
		  (with-syntax (((B (... ...))
				 (make-accessors-forms (syntax ?kontext)
						       (record-type-descriptor ?record-name))))
		    (syntax (begin B (... ...)))))))))
       (A ?context-identifier)))))


(define-syntax define-record-mutators/parents
  (syntax-rules ()
    ((_ ?record-name)
     (define-record-mutators/parents ?record-name ?record-name))
    ((_ ?record-name ?context-identifier)
     (let-syntax
	 ((A (lambda (stx)

	       (define (make-mutators-forms kontext rtd)
		 (let ((rtd-name (symbol->string (record-type-name rtd))))
		   (let process-rtd ((rtd	(record-type-parent rtd))
				     (result	'()))
		     (if rtd
			 (let* ((slots	(record-type-field-names rtd))
				(len	(vector-length slots)))
			   (let make-def ((i    0)
					  (defs '()))
			     (if (= i len)
				 (process-rtd (record-type-parent rtd)
					      (append result defs))
			       (make-def (+ 1 i)
					 (if (record-field-mutable? rtd i)
					     (cons (make-mutator kontext rtd-name rtd slots i) defs)
					   defs)))))
		       result))))

	       (define (make-mutator kontext rtd-name rtd slots i)
		 (let ((mutator-name (slot-name->mutator-name rtd-name (vector-ref slots i)))
		       (mutator-proc (record-mutator rtd i)))
		   (datum->syntax kontext `(define ,mutator-name ',mutator-proc))))

	       (define (slot-name->mutator-name rtd-name slot-name)
		 (string->symbol (string-append rtd-name "-" (symbol->string slot-name) "-set!")))

	       (syntax-case stx ()
		 ((_ ?kontext)
		  (with-syntax (((B (... ...))
				 (make-mutators-forms (syntax ?kontext)
						      (record-type-descriptor ?record-name))))
		    (syntax (begin B (... ...)))))))))
       (A ?context-identifier)))))


(define-syntax define-record-accessors/this
  (syntax-rules ()
    ((_ ?record-name)
     (define-record-accessors/this ?record-name ?record-name))
    ((_ ?record-name ?context-identifier)
     (let-syntax
	 ((A (lambda (stx)

	       (define (make-accessors-forms kontext rtd)
		 (let* ((rtd-name	(symbol->string (record-type-name rtd)))
			(slots		(record-type-field-names rtd))
			(len		(vector-length slots)))
		   (let make-def ((i      0)
				  (result '()))
		     (if (= i len)
			 result
		       (make-def (+ 1 i) (cons (make-accessor kontext rtd-name rtd slots i)
					       result))))))

	       (define (make-accessor kontext rtd-name rtd slots i)
		 (let ((accessor-name (slot-name->accessor-name rtd-name (vector-ref slots i)))
		       (accessor-proc (record-accessor rtd i)))
		   (datum->syntax kontext `(define ,accessor-name ',accessor-proc))))

	       (define (slot-name->accessor-name rtd-name slot-name)
		 (string->symbol (string-append rtd-name "-" (symbol->string slot-name))))

	       (syntax-case stx ()
		 ((_ ?kontext)
		  (with-syntax (((B (... ...))
				 (make-accessors-forms (syntax ?kontext)
						       (record-type-descriptor ?record-name))))
		    (syntax (begin B (... ...)))))))))
       (A ?context-identifier)))))


(define-syntax define-record-mutators/this
  (syntax-rules ()
    ((_ ?record-name)
     (define-record-mutators/this ?record-name ?record-name))
    ((_ ?record-name ?context-identifier)
     (let-syntax
	 ((A (lambda (stx)

	       (define (make-mutators-forms kontext rtd)
		 (let* ((rtd-name	(symbol->string (record-type-name rtd)))
			(slots		(record-type-field-names rtd))
			(len		(vector-length slots)))
		   (let make-def ((i      0)
				  (result '()))
		     (if (= i len)
			 result
		       (make-def (+ 1 i)
				 (if (record-field-mutable? rtd i)
				     (cons (make-mutator kontext rtd-name rtd slots i) result)
				   result))))))

	       (define (make-mutator kontext rtd-name rtd slots i)
		 (let ((mutator-name (slot-name->mutator-name rtd-name (vector-ref slots i)))
		       (mutator-proc (record-mutator rtd i)))
		   (datum->syntax kontext `(define ,mutator-name ',mutator-proc))))

	       (define (slot-name->mutator-name rtd-name slot-name)
		 (string->symbol (string-append rtd-name "-"
						(symbol->string slot-name) "-set!")))

	       (syntax-case stx ()
		 ((_ ?kontext)
		  (with-syntax (((B (... ...))
				 (make-mutators-forms (syntax ?kontext)
						      (record-type-descriptor ?record-name))))
		    (syntax (begin B (... ...)))))))))
       (A ?context-identifier)))))


(define-syntax define-record-accessors/mutators
  (syntax-rules ()
    ((_ ?record-name)
     (define-record-accessors/mutators ?record-name ?record-name))
    ((_ ?record-name ?context-identifier)
     (let-syntax
	 ((A (lambda (stx)

	       (define (make-accessor/mutator-forms kontext rtd)
		 (let ((rtd-name (symbol->string (record-type-name rtd))))
		   (let process-rtd ((rtd	rtd)
				     (result	'()))
		     (if rtd
			 (let* ((slots	(record-type-field-names rtd))
				(len	(vector-length slots)))
			   (let make-def ((i    0)
					  (defs '()))
			     (if (= i len)
				 (process-rtd (record-type-parent rtd)
					      (append result defs))
			       (make-def (+ 1 i)
					 (cons ((if (record-field-mutable? rtd i)
						    make-accessor/mutator
						  make-accessor) kontext rtd-name rtd slots i)
					       defs)))))
		       result))))

	       (define (make-accessor kontext rtd-name rtd slots i)
		 (let ((accessor-name	(slot-name->accessor/mutator-name rtd-name (vector-ref slots i)))
		       (accessor-proc	(record-accessor rtd i)))
		   (datum->syntax kontext `(define ,accessor-name ',accessor-proc))))

	       (define (make-accessor/mutator kontext rtd-name rtd slots i)
		 (let ((name		(slot-name->accessor/mutator-name rtd-name (vector-ref slots i)))
		       (accessor-proc	(record-accessor rtd i))
		       (mutator-proc	(record-mutator  rtd i)))
		   (datum->syntax kontext `(define ,name
					     (case-lambda
					      ((o)
					       (',accessor-proc o))
					      ((o v)
					       (',mutator-proc o v)))))))

	       (define (slot-name->accessor/mutator-name rtd-name slot-name)
		 (string->symbol (string-append rtd-name "-" (symbol->string slot-name))))

	       (syntax-case stx ()
		 ((_ ?kontext)
		  (with-syntax (((B (... ...))
				 (make-accessor/mutator-forms (syntax ?kontext)
							      (record-type-descriptor ?record-name))))
		    (syntax (begin B (... ...)))))))))
       (A ?context-identifier)))))


(define-syntax with-record-accessors
  (syntax-rules ()
    ((_ ?record-name (?field-name ...) ?form0 ?form ...)
     (let-syntax
	 ((A (lambda (stx)

	       (define (make-forms kontext rtd field-names)
		 (let ((rtd-name (symbol->string (record-type-name rtd))))
		   (let loop ((rtd		rtd)
			      (field-names	field-names)
			      (result		'()))
		     (cond ((null? field-names)
			    result)
			   (rtd
			    (let-values (((bindings rest-fields)
					  (process-rtd kontext rtd-name rtd field-names)))
			      (loop (record-type-parent rtd)
				    rest-fields
				    (append result bindings))))
			   (else
			    (assertion-violation #f
			      "unknown field names in record type hierarchy"
			      field-names))))))

	       (define (process-rtd kontext rtd-name rtd field-names)
		 (let ((slots (record-type-field-names rtd)))
		   (let loop ((field-names	field-names)
			      (bindings		'())
			      (rest-names	'()))
		     (if (null? field-names)
			 (values bindings rest-names)
		       (let ((idx (vector-index (car field-names) slots)))
			 (if idx
			     (loop (cdr field-names)
				   (cons (make-accessor-binding kontext rtd-name rtd slots idx)
					 bindings)
				   rest-names)
			   (loop (cdr field-names)
				 bindings
				 (cons (car field-names) rest-names))))))))

	       (define (make-accessor-binding kontext rtd-name rtd slots i)
		 (let ((accessor-name (slot-name->accessor-name rtd-name (vector-ref slots i)))
		       (accessor-proc (record-accessor rtd i)))
		   (datum->syntax kontext `(,accessor-name ',accessor-proc))))

	       (define (slot-name->accessor-name rtd-name slot-name)
		 (string->symbol (string-append rtd-name "-" (symbol->string slot-name))))

	       (define (vector-index item vec)
		 (let ((len (vector-length vec)))
		   (let loop ((i 0))
		     (and (< i len)
			  (if (eq? item (vector-ref vec i))
			      i
			    (loop (+ i 1)))))))

	       (syntax-case stx ()
		 ((_ ?kontext)
		  (with-syntax (((B (... ...))
				 (make-forms (syntax ?kontext)
					     (record-type-descriptor ?record-name)
					     '(?field-name ...)))
				((F (... ...))
				 (datum->syntax (syntax ?kontext)
						'(?form0 ?form ...))))
		    (syntax (let (B (... ...))
			      F (... ...)))))))))
       (A ?record-name)))))


(define-syntax with-record-mutators
  (syntax-rules ()
    ((_ ?record-name (?field-name ...) ?form0 ?form ...)
     (let-syntax
	 ((A (lambda (stx)

	       (define (make-forms kontext rtd field-names)
		 (let ((rtd-name (symbol->string (record-type-name rtd))))
		   (let loop ((rtd		rtd)
			      (field-names	field-names)
			      (result		'()))
		     (cond ((null? field-names)
			    result)
			   (rtd
			    (let-values (((bindings rest-fields)
					  (process-rtd kontext rtd-name rtd field-names)))
			      (loop (record-type-parent rtd)
				    rest-fields
				    (append result bindings))))
			   (else
			    (assertion-violation #f
			      "unknown field names in record type hierarchy"
			      field-names))))))

	       (define (process-rtd kontext rtd-name rtd field-names)
		 (let ((slots (record-type-field-names rtd)))
		   (let loop ((field-names	field-names)
			      (bindings		'())
			      (rest-names	'()))
		     (if (null? field-names)
			 (values bindings rest-names)
		       (let ((idx (vector-index (car field-names) slots)))
			 (if idx
			     (if (record-field-mutable? rtd idx)
				 (loop (cdr field-names)
				       (cons (make-mutator-binding kontext rtd-name rtd slots idx)
					     bindings)
				       rest-names)
			       (assertion-violation #f
				 "attempt to create mutator for immutable record field"
				 (vector-ref slots idx)))
			   (loop (cdr field-names)
				 bindings
				 (cons (car field-names) rest-names))))))))

	       (define (make-mutator-binding kontext rtd-name rtd slots i)
		 (let ((mutator-name (slot-name->mutator-name rtd-name (vector-ref slots i)))
		       (mutator-proc (record-mutator rtd i)))
		   (datum->syntax kontext `(,mutator-name ',mutator-proc))))

	       (define (slot-name->mutator-name rtd-name slot-name)
		 (string->symbol (string-append rtd-name "-"
						(symbol->string slot-name) "-set!")))

	       (define (vector-index item vec)
		 (let ((len (vector-length vec)))
		   (let loop ((i 0))
		     (and (< i len)
			  (if (eq? item (vector-ref vec i))
			      i
			    (loop (+ i 1)))))))

	       (syntax-case stx ()
		 ((_ ?kontext)
		  (with-syntax (((B (... ...))
				 (make-forms (syntax ?kontext)
					     (record-type-descriptor ?record-name)
					     '(?field-name ...)))
				((F (... ...))
				 (datum->syntax (syntax ?kontext)
						'(?form0 ?form ...))))
		    (syntax (let (B (... ...))
			      F (... ...)))))))))
       (A ?record-name)))))


(define-syntax with-record-accessors/mutators
  (syntax-rules ()
    ((_ ?record-name (?field-name ...) ?form0 ?form ...)
     (let-syntax
	 ((A (lambda (stx)

	       (define (make-forms kontext rtd field-names)
		 (let ((rtd-name (symbol->string (record-type-name rtd))))
		   (let loop ((rtd		rtd)
			      (field-names	field-names)
			      (result		'()))
		     (cond ((null? field-names)
			    result)
			   (rtd
			    (let-values (((bindings rest-fields)
					  (process-rtd kontext rtd-name rtd field-names)))
			      (loop (record-type-parent rtd)
				    rest-fields
				    (append result bindings))))
			   (else
			    (assertion-violation #f
			      "unknown field names in record type hierarchy"
			      field-names))))))

	       (define (process-rtd kontext rtd-name rtd field-names)
		 (let ((slots (record-type-field-names rtd)))
		   (let loop ((field-names	field-names)
			      (bindings		'())
			      (rest-names	'()))
		     (if (null? field-names)
			 (values bindings rest-names)
		       (let ((idx (vector-index (car field-names) slots)))
			 (if idx
			     (loop (cdr field-names)
				   (cons ((if (record-field-mutable? rtd idx)
					      make-accessor/mutator-binding
					    make-accessor-binding)
					  kontext rtd-name rtd slots idx)
					 bindings)
				   rest-names)
			   (loop (cdr field-names)
				 bindings
				 (cons (car field-names) rest-names))))))))

	       (define (make-accessor/mutator-binding kontext rtd-name rtd slots i)
		 (let ((name		(slot-name->accessor/mutator-name rtd-name
									  (vector-ref slots i)))
		       (accessor-proc	(record-accessor rtd i))
		       (mutator-proc	(record-mutator  rtd i)))
		   (datum->syntax kontext `(,name (case-lambda
						   ((o)
						    (',accessor-proc o))
						   ((o v)
						    (',mutator-proc o v)))))))

	       (define (make-accessor-binding kontext rtd-name rtd slots i)
		 (let ((accessor-name	(slot-name->accessor/mutator-name rtd-name
									  (vector-ref slots i)))
		       (accessor-proc	(record-accessor rtd i)))
		   (datum->syntax kontext `(,accessor-name ',accessor-proc))))

	       (define (slot-name->accessor/mutator-name rtd-name slot-name)
		 (string->symbol (string-append rtd-name "-"
						(symbol->string slot-name))))

	       (define (vector-index item vec)
		 (let ((len (vector-length vec)))
		   (let loop ((i 0))
		     (and (< i len)
			  (if (eq? item (vector-ref vec i))
			      i
			    (loop (+ i 1)))))))

	       (syntax-case stx ()
		 ((_ ?kontext)
		  (with-syntax (((B (... ...))
				 (make-forms (syntax ?kontext)
					     (record-type-descriptor ?record-name)
					     '(?field-name ...)))
				((F (... ...))
				 (datum->syntax (syntax ?kontext)
						'(?form0 ?form ...))))
		    (syntax (let (B (... ...))
			      F (... ...)))))))))
       (A ?record-name)))))


;;;; done

)

;;; end of file
