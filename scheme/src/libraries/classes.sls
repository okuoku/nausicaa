;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: record types as classes
;;;Date: Thu Apr  1, 2010
;;;
;;;Abstract
;;;
;;;
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
(library (classes)
  (export

    ;; definitions
    define-class
    define-record-extension

    ;; constructors
    make
    make-record-maker			make-record-maker*

    ;; predicates
    is-a?				record-is-a?
    record-type-parent?

    ;; inspection
    record-type-of
    record-parent-list			record-parent-list*
    record-field-accessor		record-field-accessor*
    record-field-mutator		record-field-mutator*
    virtual-field-accessor		virtual-field-accessor*
    virtual-field-mutator		virtual-field-mutator*

    ;; field bindings
    define-record-accessors
    define-record-accessors/this
    define-record-accessors/parents

    define-record-mutators
    define-record-mutators/this
    define-record-mutators/parents

    define-record-accessors&mutators
    define-record-accessors&mutators/this
    define-record-accessors&mutators/parents

    ;; field access
    field-ref				field-set!
    with-record-accessors		with-record-mutators
    with-record-accessors&mutators
    with-record-fields			with-record-fields*
    with-fields
    ;;with-fields*

    ;; builtin conventional record type names
    <top> <builtin>
    <pair> <list>
    <char> <string> <vector> <bytevector> <hashtable>
    <record> <condition>
    <port> <binary-port> <input-port> <output-port> <textual-port>
    <fixnum> <flonum> <integer> <integer-valued> <rational> <rational-valued>
    <real> <real-valued> <complex> <number>

    ;; bindings for builtin record type descriptors
    <pair-rtd> <list-rtd>
    <char-rtd> <string-rtd> <vector-rtd> <bytevector-rtd> <hashtable-rtd>
    <record-rtd> <condition-rtd>
    <port-rtd> <binary-port-rtd> <input-port-rtd> <output-port-rtd> <textual-port-rtd>
    <fixnum-rtd> <flonum-rtd> <integer-rtd> <integer-valued-rtd> <rational-rtd> <rational-valued-rtd>
    <real-rtd> <real-valued-rtd> <complex-rtd> <number-rtd>

    ;; builtin extensions
    <pair*> <list*>
    <vector*> <bytevector*> <hashtable*>
    <number*> <port*>
    <condition*> <string*> <char*>

    ;; generic functions infrastructure
    define-generic declare-method add-method define-generic/merge
    call-next-method next-method?

    ;; predefined generic functions
    object->string)
  (import (rnrs)
    (only (language-extensions)
	  begin0
	  with-accessor-and-mutator)
    (rnrs mutable-pairs (6))
    (parameters)
    (for (records helpers) run expand)
    (for (records builtins) run expand)
    (records extensions))


;;;; helpers

(define-syntax take-left
  (syntax-rules ()
    ((_ ?dotted ?k)
     (let loop ((ret    '())
		(dotted ?dotted)
		(k      ?k))
       (if (zero? k)
	   (reverse ret)
	 (loop (cons (car dotted) ret)
	       (cdr dotted)
	       (- k 1)))))))

(define-syntax for-all*
  ;;Test that the lists have equal  length and all the elements are EQ?;
  ;;return true or false.
  ;;
  ;;This is  more than SRFI-1's EVERY,  because EVERY does  not test for
  ;;equal length.  It is not like R6RS's FOR-ALL, because FOR-ALL raises
  ;;an error if the length is different.
  ;;
  (syntax-rules ()
    ((_ ?eq ?ell1 ?ell2)
     (let loop ((ell1 ?ell1)
		(ell2 ?ell2))
       (cond ((null? ell1)
	      (null? ell2))
	     ((null? ell2)
	      (null? ell1))
	     ((?eq (car ell1) (car ell2))
	      (loop (cdr ell1) (cdr ell2)))
	     (else #f))))))


(define-syntax define-class
  (syntax-rules (fields mutable immutable parent protocol sealed opaque parent-rtd nongenerative
			virtual-fields)

    ((_ (?name ?constructor ?predicate) ?clause ...)
     (%define-class/sort-clauses
      (?name ?constructor ?predicate)
      ()	;collected mutable fields
      ()	;collected immutable fields
      ()	;collected mutable virtual fields
      ()	;collected immutable virtual fields
      (fields) (parent) (protocol) (sealed) (opaque) (parent-rtd) (nongenerative)
      ?clause ...))

    ((_ ?name ?clause ...)
     (%define-class/expand-name ?name ?clause ...))))

(define-syntax %define-class/expand-name
  (lambda (stx)
    (define (%maker name)
      (string->symbol (string-append "make-" name)))
    (define (%predicate name)
      (string->symbol (string-append name "?")))
    (syntax-case stx (fields mutable immutable parent protocol sealed opaque parent-rtd nongenerative
			     virtual-fields)
      ((_ ?name ?clause ...)
       (let ((name (symbol->string (syntax->datum #'?name))))
	 (with-syntax ((MAKER     (datum->syntax #'?name (%maker     name)))
		       (PREDICATE (datum->syntax #'?name (%predicate name))))
	   #'(%define-class/sort-clauses
	      (?name MAKER PREDICATE)
	      () ;collected mutable fields
	      () ;collected immutable fields
	      () ;collected mutable virtual fields
	      () ;collected immutable virtual fields
	      (fields) (parent) (protocol) (sealed) (opaque) (parent-rtd) (nongenerative)
	      ?clause ...))))
      )))

(define-syntax %define-class/sort-clauses
  ;;Sorts all  the auxiliary  syntaxes.  Collects the  specifications of
  ;;mutable and immutable fields.
  ;;
  ;;(Note by Marco  Maggi, Thu Apr 1, 2010) The  expansion of this macro
  ;;adds all the auxiliary syntaxes which are missing in the input form;
  ;;later  the unused ones  are removed  by %DEFINE-CLASS/FILTER-UNUSED;
  ;;adding and removing  is useless, but for now it gives  me a sense of
  ;;control and expandability, so I keep it like this.
  ;;
  (syntax-rules (fields mutable immutable parent protocol sealed opaque parent-rtd nongenerative
			virtual-fields)

    ;;Gather the PARENT clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (parent ?parent-name) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ... ?parent-name)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd		?pad ...)
      (nongenerative	?non ...)
      ?clause ...))

    ;;Gather the PROTOCOL clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (protocol ?protocol-proc) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ... ?protocol-proc)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      ?clause ...))

    ;;Gather the SEALED clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (sealed ?sealed) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ... ?sealed)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      ?clause ...))

    ;;Gather the OPAQUE clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (opaque ?opaque) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ... ?opaque)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      ?clause ...))

    ;;Gatherthe PARENT-RTD clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (parent-rtd ?parent-rtd ?parent-cd) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ... ?parent-rtd ?parent-cd)
      (nongenerative	?non ...)
      ?clause ...))

    ;;Gather the NONGENERATIVE non-empty clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (nongenerative ?nongenerative) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ... ?nongenerative)
      ?clause ...))

    ;;Gather the NONGENERATIVE empty clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (nongenerative) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ... #t)
      ?clause ...))

    ;;Gather mutable FIELDS clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (fields (mutable ?field0 ?field ...) ?field-clause ...) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ... (?field0 ?field ...))
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ... (mutable ?field0 ?field ...))
      (parent		?par ...)
      (protocol	?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (fields ?field-clause ...) ?clause ...))

    ;;Gather immutable FIELDS clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (fields (immutable ?field0 ?field ...) ?field-clause ...) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ... (?field0 ?field ...))
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ... (immutable ?field0 ?field ...))
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (fields ?field-clause ...) ?clause ...))

    ;;Gather   immutable  FIELDS   clause  declared   without  IMMUTABLE
    ;;auxiliary syntax.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (fields ?field ?field-clause ...) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ... (?field))
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ... (immutable ?field))
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (fields ?field-clause ...) ?clause ...))

    ;;Remove empty, leftover, FIELDS clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol	?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (fields) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      ?clause ...))

    ;;Gather mutable VIRTUAL-FIELDS clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (virtual-fields (mutable ?field0 ?field ...) ?field-clause ...) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...  (?field0 ?field ...))
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol	?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (virtual-fields ?field-clause ...) ?clause ...))

    ;;Gather immutable VIRTUAL-FIELDS clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (virtual-fields (immutable ?field0 ?field ...) ?field-clause ...) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ... (?field0 ?field ...))
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (virtual-fields ?field-clause ...) ?clause ...))

    ;;Gather immutable VIRTUAL-FIELDS  clause declared without IMMUTABLE
    ;;auxiliary syntax.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (virtual-fields ?field ?field-clause ...) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ... (?field))
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (fields ?field-clause ...) ?clause ...))

    ;;Remove empty, leftover, VIRTUAL-FIELDS clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol	?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (virtual-fields) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      ?clause ...))

    ;;No    more    clauses    to    gather,    hand    everything    to
    ;;%DEFINE-CLASS/FILTER-UNUSED.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...))
     (%define-class/filter-unused
      (?name ...)
      ()	;collected clauses
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)))
    ))

(define-syntax %define-class/filter-unused
  ;;Removes auxiliary syntaxes which cannot be empty.
  ;;
  (lambda (stx)
    (syntax-case stx (fields parent protocol sealed opaque parent-rtd nongenerative)

      ;;Remove unused FIELDS form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (fields)
				    ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))
      ;;Collect used FIELDS form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (fields ?e0 ?e ...)
				    ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ... (fields ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))

      ;;Remove unused PARENT form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (parent)
				    ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))
      ;;Collect used PARENT form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (parent ?e0 ?e ...)
				    ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ... (parent ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))

      ;;Remove unused PROTOCOL form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (protocol)
				    ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))
      ;;Collect used PROTOCOL form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (protocol ?e0 ?e ...)
				    ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ... (protocol ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))

      ;;Remove unused SEALED form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (sealed)
				    ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))
      ;;Collect used SEALED form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (sealed ?e0 ?e ...) ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ... (sealed ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))

      ;;Remove unused OPAQUE form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (opaque) ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))
      ;;Collect used OPAQUE form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (opaque ?e0 ?e ...) ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ... (opaque ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))

      ;;Remove unused PARENT-RTD form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (parent-rtd) ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))
      ;;Collect used PARENT-RTD form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (parent-rtd ?e0 ?e ...) ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ... (parent-rtd ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))

      ;;Remove unused NONGENERATIVE form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (nongenerative)
				    ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))
      ;;Collect empty-but-used NONGENERATIVE form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (nongenerative #t) ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ... (nongenerative))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))
      ;;Collect used NONGENERATIVE form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (nongenerative ?e0 ?e ...)
				    ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ... (nongenerative ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))

      ;;No    more    clauses   to    filter,    hand   everything    to
      ;;%DEFINE-CLASS/EXPAND-FIELD-NAMES.
      ((%define-class/filter-unused (?name ?constructor ?predicate)
				    (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...))
       #'(begin
	   (define-record-type (?name ?constructor ?predicate) ?collected-clause ...)
	   (%define-class/expand-field-names ?name
					     ()	;expanded mutable fields
					     ()	;expanded immutable fields
					     ()	;expanded virtual mutable fields
					     ()	;expanded virtual immutable fields
					     (?collected-mutable-field ...)
					     (?collected-immutable-field ...)
					     (?collected-mutable-virtual-field ...)
					     (?collected-immutable-virtual-field ...))))
      )))

(define-syntax %define-class/expand-field-names
  ;;Expand the collected  lists of fields to have  explicit accessor and
  ;;mutator names.
  ;;
  (lambda (stx)
    (define (%accessor name field)
      (string->symbol (string-append name "-" field)))
    (define (%mutator name field)
      (string->symbol (string-append name "-" field "-set!")))
    (syntax-case stx ()

      ;;NOTE:  mutable  field  clauses  either have  both  accessor  and
      ;;mutator names or  have none; they cannot have  only the accessor
      ;;name.

      ;;Expand mutable field clause with neither accessor nor mutator names.
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ((?mutable-name) ?mutable-field ...)
	  (?immutable-field ...)
	  (?virtual-mutable-field ...)
	  (?virtual-immutable-field ...))
       (let ((name  (symbol->string (syntax->datum #'?name)))
	     (field (symbol->string (syntax->datum #'?mutable-name))))
	 (with-syntax ((ACCESSOR (datum->syntax #'?name (%accessor name field)))
		       (MUTATOR  (datum->syntax #'?name (%mutator  name field))))
	   #'(%define-class/expand-field-names
	      ?name
	      (?expanded-mutable-field ... (?mutable-name ACCESSOR MUTATOR))
	      (?expanded-immutable-field ...)
	      (?expanded-virtual-mutable-field ...)
	      (?expanded-virtual-immutable-field ...)
	      (?mutable-field ...)
	      (?immutable-field ...)
	      (?virtual-mutable-field ...)
	      (?virtual-immutable-field ...)))))

      ;;Pass through mutable field clause with accessor and mutator name.
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ((?mutable-name ?mutable-accessor ?mutable-mutator) ?mutable-field ...)
	  (?immutable-field ...)
	  (?virtual-mutable-field ...)
	  (?virtual-immutable-field ...))
       #'(%define-class/expand-field-names
	  ?name
	  (?expanded-mutable-field ... (?mutable-name ?mutable-accessor ?mutable-mutator))
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  (?mutable-field ...)
	  (?immutable-field ...)
	  (?virtual-mutable-field ...)
	  (?virtual-immutable-field ...)))

      ;;Expand immutable field clause without accessor name.
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ()	;no more mutable field clauses
	  ((?immutable-name) ?immutable-field ...)
	  (?virtual-mutable-field ...)
	  (?virtual-immutable-field ...))
       (let ((name  (symbol->string (syntax->datum #'?name)))
	     (field (symbol->string (syntax->datum #'?immutable-name))))
	 (with-syntax ((ACCESSOR (datum->syntax #'?name (%accessor name field))))
	   #'(%define-class/expand-field-names
	      ?name
	      (?expanded-mutable-field ...)
	      (?expanded-immutable-field ... (?immutable-name ACCESSOR))
	      (?expanded-virtual-mutable-field ...)
	      (?expanded-virtual-immutable-field ...)
	      () ;no more mutable field clauses
	      (?immutable-field ...)
	      (?virtual-mutable-field ...)
	      (?virtual-immutable-field ...)))))

      ;;Pass through immutable field clause with accessor name.
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ()	;no more mutable field clauses
	  ((?immutable-name ?immutable-accessor) ?immutable-field ...)
	  (?virtual-mutable-field ...)
	  (?virtual-immutable-field ...))
       #'(%define-class/expand-field-names
	  ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ... (?immutable-name ?immutable-accessor))
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ()	;no more mutable field clauses
	  (?immutable-field ...)
	  (?virtual-mutable-field ...)
	  (?virtual-immutable-field ...)))

      ;;Expand virtual mutable field clause with neither accessor nor mutator names.
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ()	;no more mutable field clauses
	  ()	;no more immutable field clauses
	  ((?virtual-mutable-name) ?virtual-mutable-field ...)
	  (?virtual-immutable-field ...))
       (let ((name  (symbol->string (syntax->datum #'?name)))
	     (field (symbol->string (syntax->datum #'?virtual-mutable-name))))
	 (with-syntax ((ACCESSOR (datum->syntax #'?name (%accessor name field)))
		       (MUTATOR  (datum->syntax #'?name (%mutator  name field))))
	   #'(%define-class/expand-field-names
	      ?name
	      (?expanded-mutable-field ...)
	      (?expanded-immutable-field ...)
	      (?expanded-virtual-mutable-field ... (?virtual-mutable-name ACCESSOR MUTATOR))
	      (?expanded-virtual-immutable-field ...)
	      ()	;no more mutable field clauses
	      ()	;no more immutable field clauses
	      (?virtual-mutable-field ...)
	      (?virtual-immutable-field ...)))))

      ;;Pass through virtual mutable field clause with accessor and mutator name.
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ()	;no more mutable field clauses
	  ()	;no more immutable field clauses
	  ((?virtual-mutable-name ?virtual-mutable-accessor ?virtual-mutable-mutator)
	   ?virtual-mutable-field ...)
	  (?virtual-immutable-field ...))
       #'(%define-class/expand-field-names
	  ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ... (?virtual-mutable-name
						?virtual-mutable-accessor
						?virtual-mutable-mutator))
	  (?expanded-virtual-immutable-field ...)
	  ()	;no more mutable field clauses
	  ()	;no more immutable field clauses
	  (?virtual-mutable-field ...)
	  (?virtual-immutable-field ...)))

      ;;Expand virtual immutable field clause without accessor name.
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ()	;no more mutable field clauses
	  ()	;no more immutable field clauses
	  ()	;no more virtual mutable field clauses
	  ((?virtual-immutable-name) ?virtual-immutable-field ...))
       (let ((name  (symbol->string (syntax->datum #'?name)))
	     (field (symbol->string (syntax->datum #'?virtual-immutable-name))))
	 (with-syntax ((ACCESSOR (datum->syntax #'?name (%accessor name field))))
	   #'(%define-class/expand-field-names
	      ?name
	      (?expanded-mutable-field ...)
	      (?expanded-immutable-field ...)
	      (?expanded-virtual-mutable-field ...)
	      (?expanded-virtual-immutable-field ... (?virtual-immutable-name ACCESSOR))
	      ()	;no more mutable field clauses
	      ()	;no more immutable field clauses
	      ()	;no more virtual mutable field clauses
	      (?virtual-immutable-field ...)))))

      ;;Pass through immutable field clause with accessor name.
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ()	;no more mutable field clauses
	  ()	;no more immutable field clauses
	  ()	;no more virtual mutable field clauses
	  ((?virtual-immutable-name ?virtual-immutable-accessor) ?virtual-immutable-field ...))
       #'(%define-class/expand-field-names
	  ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ... (?virtual-immutable-name ?virtual-immutable-accessor))
	  ()	;no more mutable field clauses
	  ()	;no more immutable field clauses
	  ()	;no more virtual mutable field clauses
	  (?virtual-immutable-field ...)))

      ;;No    more    lists    to    expand,    hand    everything    to
      ;;%DEFINE-CLASS/OUTPUT-FORMS.
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ()	;no more mutable field clauses
	  ()	;no more immutable field clauses
	  ()	;no more virtual mutable field clauses
	  ())	;no more virtual immutable field clauses
       #'(%define-class/output-forms ?name
				     (?expanded-mutable-field ...)
				     (?expanded-immutable-field ...)
				     (?expanded-virtual-mutable-field ...)
				     (?expanded-virtual-immutable-field ...)))
      )))

(define-syntax %define-class/output-forms
  (lambda (stx)
    (define (%accessor name)
      (string->symbol (string-append "with-record-fields-of-" (symbol->string name))))
    (syntax-case stx ()
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...))
       (with-syntax ((ACCESSOR (datum->syntax #'?name (%accessor (syntax->datum #'?name)))))
	 #'(define-syntax ACCESSOR
	     (syntax-rules ()
	       ((_  ?name ?body0 ?body (... ...))
		(%with-record-fields ?name (?expanded-mutable-field
					    ...
					    ?expanded-immutable-field ...
					    ?expanded-virtual-mutable-field ...
					    ?expanded-virtual-immutable-field ...)
				     ?body0 ?body (... ...))))))))))

(define-syntax %with-record-fields
  (lambda (stx)
    (define (%field name field)
      (string->symbol (string-append (symbol->string name) "." (symbol->string field))))
    (syntax-case stx ()

      ;;Process a field clause with both accessor and mutator.
      ((_ ?name ((?field ?accessor ?mutator) ?clause ...) ?body0 ?body ...)
       (with-syntax ((FIELD (datum->syntax #'?name (%field (syntax->datum #'?name)
							   (syntax->datum #'?field)))))
	 #'(with-accessor-and-mutator ((FIELD ?name ?accessor ?mutator))
				      (%with-record-fields ?name (?clause ...) ?body0 ?body ...))))

      ;;Process a field clause with accessor only.
      ((_ ?name ((?field ?accessor) ?clause ...) ?body0 ?body ...)
       (with-syntax ((FIELD (datum->syntax #'?name (%field (syntax->datum #'?name)
							   (syntax->datum #'?field)))))
	 #'(with-accessor-and-mutator ((FIELD ?name ?accessor))
				      (%with-record-fields ?name (?clause ...) ?body0 ?body ...))))

      ;;No more field clauses, output the body.
      ((_ ?name () ?body0 ?body ...)
       #'(begin ?body0 ?body ...))
      )))

;; (define-syntax record-fields-with
;;   (lambda (stx)
;;     (define (%accessor class)
;;       (string->symbol (string-append "with-record-fields-of-" (symbol->string class))))
;;     (syntax-case stx ()
;;       ((_ ?class)
;;        (datum->syntax #'?class (%accessor (syntax->datum #'?class)))))))

(define-syntax with-fields
  (lambda (stx)
    (define (%accessor class)
      (string->symbol (string-append "with-record-fields-of-" (symbol->string class))))
    (syntax-case stx ()

      ((_ ((?class ?name) ?clause ...) ?body0 ?body ...)
       (with-syntax ((ACCESSOR (datum->syntax #'?class (%accessor (syntax->datum #'?class)))))
	 #'(ACCESSOR ?name (with-fields (?clause ...) ?body0 ?body ...))))

      ((_ () ?body0 ?body ...)
       #'(begin ?body0 ?body ...)))))


;;;; constructors

(define-syntax make-record-maker*
  ;;When there  is no init value, we  can build the maker  in the expand
  ;;phase;  when the  init value  is present,  we need  to  delay actual
  ;;building to the run phase.
  (syntax-rules ()
    ((_ ?record-name)
     (let-syntax
	 ((dummy (lambda (stx)
		   #`(quote #,(make-record-maker (record-type-descriptor ?record-name))))))
       (dummy)))
    ((_ ?record-name ?init)
     (let-syntax
	 ((dummy (lambda (stx)
		   (syntax-case stx ()
		     ((_ ?kontext)
		      #`(make-record-maker (record-type-descriptor ?record-name)
					   #,(datum->syntax #'?kontext '?init)))))))
       (dummy ?record-name)))))

(define-syntax make
  (syntax-rules ()
    ((_ ?record-name ?arg ...)
     (let-syntax
	 ((dummy (lambda (stx)
		   #`((quote #,(record-constructor
				(record-constructor-descriptor ?record-name)))
		      ?arg ...))))
       (dummy)))))


;;;; predicates

(define (record-is-a? obj rtd)
  (eq? rtd (record-type-of obj)))

(define-syntax is-a?
  (syntax-rules ()
    ((_ ?obj ?record-name)
     (let-syntax
	 ((dummy (lambda (stx)
		   #`((quote #,(%record-predicate (record-type-descriptor ?record-name)))
		      ?obj))))
       (dummy)))))

(define (record-type-parent? rtd1 rtd2)
  (cond ((eq? rtd1 rtd2)	#t)
	((eq? rtd1 (record-type-descriptor <top>))   #f)
   	((eq? rtd2 (record-type-descriptor <top>))   #t)
	(else
	 (memq rtd2 (record-parent-list rtd1)))))


;;;; inspection

(define-syntax record-parent-list*
  (syntax-rules ()
    ((_ ?record-name)
     (let-syntax
	 ((dummy (lambda (stx)
		   #`(quote #,(record-parent-list (record-type-descriptor ?record-name))))))
       (dummy)))))

(define (record-type-of obj)
  ;;Return the  record type  descriptor associated to  OBJ, if obj  is a
  ;;RECORD; else  return the RTD  of <top>.  The  order of the  tests is
  ;;important.  More specialised types must come first.
  ;;
  (cond

   ;;This  is  here  as  a  special exception  because  in  Larceny  the
   ;;hashtable  is a  record.  We  have  to process  it before  applying
   ;;RECORD?
   ((hashtable?	obj)		(record-type-descriptor <hashtable>))

   ((record? obj)
    (record-rtd obj))

   ((number? obj)
    ;;Order does matter here!!!
    (cond ((fixnum?		obj)	(record-type-descriptor <fixnum>))
	  ((integer?		obj)	(record-type-descriptor <integer>))
	  ((rational?		obj)	(record-type-descriptor <rational>))
	  ((integer-valued?	obj)	(record-type-descriptor <integer-valued>))
	  ((rational-valued?	obj)	(record-type-descriptor <rational-valued>))
	  ((flonum?		obj)	(record-type-descriptor <flonum>))
	  ((real?		obj)	(record-type-descriptor <real>))
	  ((real-valued?	obj)	(record-type-descriptor <real-valued>))
	  ((complex?		obj)	(record-type-descriptor <complex>))
	  (else				(record-type-descriptor <number>))))
   ((char?		obj)		(record-type-descriptor <char>))
   ((string?		obj)		(record-type-descriptor <string>))
   ((vector?		obj)		(record-type-descriptor <vector>))
   ((bytevector?	obj)		(record-type-descriptor <bytevector>))
   ((port?		obj)
    ;;Order here is arbitrary.
    (cond ((input-port?		obj)	(record-type-descriptor <input-port>))
	  ((output-port?	obj)	(record-type-descriptor <output-port>))
	  ((binary-port?	obj)	(record-type-descriptor <binary-port>))
	  ((textual-port?	obj)	(record-type-descriptor <textual-port>))
	  (else				(record-type-descriptor <port>))))
   ((condition?		obj)		(record-type-descriptor <condition>))
   ((record?		obj)		(record-type-descriptor <record>))
   ((pair?		obj)
    ;;Order does matter  here!!!  Better leave these at  the end because
    ;;qualifying a long list can be time-consuming.
    (cond ((list?	obj)	(record-type-descriptor <list>))
	  (else			(record-type-descriptor <pair>))))
   (else (record-type-descriptor <top>))))


;;;; basic field access

(define-syntax record-field-accessor*
  (syntax-rules ()
    ((_ ?record-name ?field-name)
     (let-syntax
	 ((dummy (lambda (stx)
		   #`(quote #,(record-field-accessor (record-type-descriptor ?record-name)
						     '?field-name)))))
       (dummy)))))

(define-syntax record-field-mutator*
  (syntax-rules ()
    ((_ ?record-name ?field-name)
     (record-field-mutator* ?record-name ?field-name #f))

    ((_ ?record-name ?field-name ?false-if-immutable)
     (let-syntax
	 ((dummy (lambda (stx)
		   #`(quote #,(record-field-mutator (record-type-descriptor ?record-name)
						    '?field-name ?false-if-immutable)))))
       (dummy)))))

(define-syntax virtual-field-accessor*
  (syntax-rules ()
    ((_ ?extension-record ?field-name)
     (let-syntax
	 ((dummy (lambda (stx)
		   #`(quote #,(virtual-field-accessor ?extension-record (quote ?field-name))))))
       (dummy)))))

(define-syntax virtual-field-mutator*
  (syntax-rules ()
    ((_ ?extension-record ?field-name)
     (virtual-field-mutator* ?extension-record ?field-name #f))

    ((_ ?extension-record ?field-name ?false-if-immutable)
     (let-syntax
	 ((dummy (lambda (stx)
		   #`(quote #,(virtual-field-mutator ?extension-record (quote ?field-name)
						     ?false-if-immutable)))))
       (dummy)))))

(define-syntax field-ref
  (syntax-rules ()
    ((_ ?expr ?field-name)
     (let ((obj ?expr))
       ((record-field-accessor (record-rtd obj) ?field-name) obj)))))

(define-syntax field-set!
  (syntax-rules ()
    ((_ ?expr ?field-name ?value)
     (let ((obj ?expr))
       ((record-field-mutator (record-rtd obj) ?field-name) obj ?value)))))


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
							     #'?kontext
							     (record-type-descriptor ?record-name))))
			     #'(begin DEFINES ((... ...) (... ...)))))))))
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
			     #'(let (BINDINGS ((... ...) (... ...)))
				 FORMS ((... ...) (... ...)))))))))
	    (dummy ?record-name))))))))

(%define-with-record with-record-accessors accessor)
(%define-with-record with-record-mutators mutator)
(%define-with-record with-record-accessors&mutators accessor&mutator)


(define-syntax with-record-fields
  (syntax-rules ()
    ((_ () ?form0 ?form ...)
     (begin ?form0 ?form ...))

    ((_ ((() ?record-name ?expr) ?bindings ...) ?form0 ?form ...)
     (with-record-fields (?bindings ...) ?form0 ?form ...))

    ((_ ((((?var-name ?field-name) ?field-spec ...) ?record-name ?expr) ?bindings ...)
	?form0 ?form ...)
     (let ((id ?expr))
       (let-syntax
	   ((?var-name (identifier-syntax
			(_          ((record-field-accessor* ?record-name ?field-name)    id))
			((set! _ e) ((record-field-mutator*  ?record-name ?field-name #f) id e)))))
	 (with-record-fields (((?field-spec ...) ?record-name id))
	   (with-record-fields (?bindings ...) ?form0 ?form ...)))))

    ((_ (((?field-name ?field-spec ...) ?record-name ?expr) ?bindings ...) ?form0 ?form ...)
     (with-record-fields ((((?field-name ?field-name) ?field-spec ...) ?record-name ?expr))
       (with-record-fields (?bindings ...) ?form0 ?form ...)))

    ((_ ((?field-name ?record-name ?expr) ?bindings ...) ?form0 ?form ...)
     (with-record-fields ((((?field-name ?field-name)) ?record-name ?expr))
       (with-record-fields (?bindings ...) ?form0 ?form ...)))))


(define-syntax with-record-fields*
  (lambda (stx)
    (syntax-case stx ()
      ((_ () ?form0 ?form ...)
       #'(begin ?form0 ?form ...))

      ((_ ((() ?record-name ?expr) ?bindings ...) ?form0 ?form ...)
       #'(with-record-fields* (?bindings ...) ?form0 ?form ...))

      ((_ (((?field-name) ?record-name ?record-id)) ?form0 ?form ...)
       (let ((vars (%record-field-identifiers (syntax->datum #'?record-id)
					      (list (syntax->datum #'?field-name)))))
	 (with-syntax (((VAR) (datum->syntax #'?field-name vars)))
	   #'(let-syntax
		 ((VAR (identifier-syntax
			(_
			 ((record-field-accessor* ?record-name ?field-name) ?record-id))
			((set! _ e)
			 ((record-field-mutator*  ?record-name ?field-name) ?record-id e)))))
		  ?form0 ?form ...))))

      ((_ (((?field-name0 ?field-name ...) ?record-name ?record-id) ?bindings ...) ?form0 ?form ...)
       #'(with-record-fields* (((?field-name0) ?record-name ?record-id))
	   (with-record-fields* (((?field-name ...) ?record-name ?record-id))
	     (with-record-fields* (?bindings ...) ?form0 ?form ...))))

      ((_ ((?field-name ?record-name ?record-id) ?bindings ...) ?form0 ?form ...)
       #'(with-record-fields* (((?field-name) ?record-name ?record-id))
	   (with-record-fields* (?bindings ...) ?form0 ?form ...))))))

;; (define-syntax with-record-fields*
;;   (syntax-rules ()
;;     ((_ () ?form0 ?form ...)
;;      (begin ?form0 ?form ...))

;;     ((_ (((?field-name ...) ?record-name ?record-id) ?bindings ...) ?form0 ?form ...)
;;      (let-syntax
;; 	 ((dummy (lambda (stx)
;; 		   (syntax-case stx ()
;; 		     ((_ ?kontext)
;; 		      (let ((RTD (record-type-descriptor ?record-name)))
;; 			(with-syntax
;; 			    ((ID (datum->syntax #'?kontext '?record-id))
;; 			     ((VAR (... ...))
;; 			      (datum->syntax #'?kontext
;; 					     (%record-field-identifiers '?record-id '(?field-name ...))))
;; 			     ((ACCESSOR (... ...)) `(,(record-field-accessor RTD '?field-name)    ...))
;; 			     ((MUTATOR (... ...))  `(,(record-field-mutator  RTD '?field-name #f) ...))
;; 			     ((FORMS (... ...))    (datum->syntax #'?kontext '(?form0 ?form ...)))
;; 			     ((BINDS (... ...))    (datum->syntax #'?kontext '(?bindings ...))))
;; 			  #'(let-syntax
;; 				((VAR (identifier-syntax (_          ('ACCESSOR ID))
;; 							 ((set! _ e) ('MUTATOR  ID e))))
;; 				 (... ...))
;; 			      (with-record-fields* (BINDS (... ...)) FORMS (... ...))))))))))
;;        (dummy ?record-name)))

;;     ((_ ((?field-name ?record-name ?record-id) ?bindings ...) ?form0 ?form ...)
;;      (with-record-fields* (((?field-name) ?record-name ?record-id))
;;        (with-record-fields* (?bindings ...) ?form0 ?form ...)))))


#;(define-syntax with-fields
  (syntax-rules ()
    ((_ () ?form0 ?form ...)
     (begin ?form0 ?form ...))

    ((_ ((() ?extension-record ?expr) ?bindings ...) ?form0 ?form ...)
     (with-fields (?bindings ...) ?form0 ?form ...))

    ((_ ((((?var-name ?field-name) ?field-spec ...) ?extension-record ?expr) ?bindings ...)
	?form0 ?form ...)
     (let ((id ?expr))
       (let-syntax
	   ((?var-name (identifier-syntax
			(_          ((virtual-field-accessor* ?extension-record ?field-name)    id))
			((set! _ e) ((virtual-field-mutator*  ?extension-record ?field-name #f) id e)))))
	 (with-fields (((?field-spec ...) ?extension-record id))
	   (with-fields (?bindings ...) ?form0 ?form ...)))))

    ((_ (((?field-name ?field-spec ...) ?extension-record ?expr) ?bindings ...) ?form0 ?form ...)
     (with-fields ((((?field-name ?field-name) ?field-spec ...) ?extension-record ?expr))
       (with-fields (?bindings ...) ?form0 ?form ...)))

    ((_ ((?field-name ?extension-record ?expr) ?bindings ...) ?form0 ?form ...)
     (with-fields ((((?field-name ?field-name)) ?extension-record ?expr))
       (with-fields (?bindings ...) ?form0 ?form ...)))))


#;(define-syntax with-fields*
  (lambda (stx)
    (syntax-case stx ()
      ((_ () ?form0 ?form ...)
       #'(begin ?form0 ?form ...))

      ((_ ((() ?extension-record ?expr) ?bindings ...) ?form0 ?form ...)
       #'(with-fields* (?bindings ...) ?form0 ?form ...))

      ((_ (((?field-name) ?extension-record ?record-id)) ?form0 ?form ...)
       (let ((var (%record-field-identifiers (syntax->datum #'?record-id)
					     (list (syntax->datum #'?field-name)))))
	 (with-syntax (((VAR) (datum->syntax #'?field-name var)))
	   #'(let-syntax
		 ((VAR (identifier-syntax
			(_
			 ((virtual-field-accessor* ?extension-record ?field-name) ?record-id))
			((set! _ e)
			 ((virtual-field-mutator*  ?extension-record ?field-name) ?record-id e)))))
		  ?form0 ?form ...))))

      ((_ (((?field-name0 ?field-name ...) ?extension-record ?record-id) ?bindings ...) ?form0 ?form ...)
       #'(with-fields* (((?field-name0) ?extension-record ?record-id))
	   (with-fields* (((?field-name ...) ?extension-record ?record-id))
	     (with-fields* (?bindings ...) ?form0 ?form ...))))

      ((_ ((?field-name ?extension-record ?record-id) ?bindings ...) ?form0 ?form ...)
       #'(with-fields* (((?field-name) ?extension-record ?record-id))
	   (with-fields* (?bindings ...) ?form0 ?form ...))))))

;; (define-syntax with-fields*
;;   (syntax-rules ()
;;     ((_ () ?form0 ?form ...)
;;      (begin ?form0 ?form ...))

;;     ((_ (((?field-name ...) ?extension-record ?record-id))
;; 	?form0 ?form ...)
;;      (let-syntax
;; 	 ((dummy (lambda (stx)
;; 		   (syntax-case stx ()
;; 		     ((_ ?kontext)
;; 		      (with-syntax
;; 			  (((RECORD-ID)
;; 			    (datum->syntax #'?kontext '(?record-id)))
;; 			   ((VAR (... ...))
;; 			    (datum->syntax #'?kontext
;; 					   (%record-field-identifiers '?record-id '(?field-name ...))))
;; 			   ((ACCESSOR (... ...))
;; 			    `(,(virtual-field-accessor ?extension-record '?field-name) ...))
;; 			   ((MUTATOR (... ...))
;; 			    `(,(virtual-field-mutator  ?extension-record '?field-name #f) ...))
;; 			   ((FORMS (... ...))
;; 			    (datum->syntax #'?kontext '(?form0 ?form ...))))
;; 			#'(let-syntax
;; 			      ((VAR (identifier-syntax (_          ('ACCESSOR RECORD-ID))
;; 						       ((set! _ e) ('MUTATOR  RECORD-ID e))))
;; 			       (... ...))
;; 			    FORMS (... ...))))))))
;;        (dummy ?extension-record)))

;;     ((_ (((?field-name ...) ?extension-record ?record-id) ?bindings ...) ?form0 ?form ...)
;;      (with-fields* (((?field-name ...) ?extension-record ?record-id))
;;        (with-fields* (?bindings ...) ?form0 ?form ...)))

;;     ((_ ((?field-name ?extension-record ?record-id) ?bindings ...) ?form0 ?form ...)
;;      (with-fields* (((?field-name) ?extension-record ?record-id))
;;        (with-fields* (?bindings ...) ?form0 ?form ...)))))


;;;; next method implementation

(define next-method-func-parm (make-parameter #f))
(define next-method-pred-parm (make-parameter #f))

(define-syntax call-next-method
  (syntax-rules ()
    ((_)
     (let ((f (next-method-func-parm)))
       (if f (f)
	 (assertion-violation 'call-next-method
	   "invoked call-next-method outside of a generic function"))))))

(define-syntax next-method?
  (syntax-rules ()
    ((_)
     (let ((f (next-method-pred-parm)))
       (if f (f)
	 (assertion-violation 'call-next-method
	   "invoked next-method? outside of a generic function"))))))


;;;; generic functions

(define-record-type special-argument
  (opaque #t)
  (sealed #t)
  (nongenerative))

(define :method-adder
  (make-special-argument))

(define :method-alist
  (make-special-argument))

(define :method-alist-set!
  (make-special-argument))

(define-syntax define-generic
  (syntax-rules ()
    ((_ ?name)
     (define ?name (make-generic-function)))))

(define-syntax define-generic/merge
  (syntax-rules ()
    ((_ ?name ?gf0 ?gf ...)
     (define ?name
       (merge-generic-functions ?gf0 ?gf ...)))))

(define (make-generic-function)
  (let* ((method-alist	'())
	 (cache		#f)
	 (method-adder	(lambda (signature has-rest closure)
			  (set! method-alist
				(%add-method-to-method-alist method-alist
							     signature has-rest closure)))))
    (define-syntax %assert-no-methods
      (syntax-rules ()
	((_ ?signature)
	 (assertion-violation #f "no method defined for the argument's types"
			      (map record-type-name ?signature)))
	((_)
	 (assertion-violation #f "no method defined for the argument's types"))))

    ;; (case-lambda
    ;;  (()
    ;;   (cond (method-with-no-args
    ;; 	     (method-with-no-args))
    ;; 	    (method-with-no-args-and-rest
    ;; 	     (method-with-no-args-and-rest))
    ;; 	    (else
    ;; 	     (%assert-no-methods))))
    ;;  ((arg)
    ;;   (cond ((eq? arg :method-adder)
    ;; 	     method-adder)
    ;; 	    ((eq? arg :method-alist)
    ;; 	     method-alist)))
    ;;  )

    (lambda args
      (if (and (pair? args)
      	       (special-argument? (car args)))
      	  (let ((arg (car args)))
      	    (cond ((eq? arg :method-adder)
		   (when cache
		     (hashtable-clear! cache))
      		   method-adder)
		  ((eq? arg :method-alist)
      		   method-alist)
		  ((eq? arg :method-alist-set!)
		   (when cache
		     (hashtable-clear! cache))
      		   (set! method-alist (cadr args)))
      		  (else
      		   (assertion-violation #f "internal error with invalid special argument" arg))))
	(let-syntax
	    ((apply-function/stx (syntax-rules ()
				   ((_ ?closure)
				    (apply ?closure args))))
	     (consume-closure (syntax-rules ()
				((_ ?closure-list)
				 (begin0
				     (car ?closure-list)
				   (set! ?closure-list (cdr ?closure-list)))))))
	  (letrec*
	      ((signature
		(map record-type-of args))

	       (applicable-methods
		(cond ((and cache (hashtable-ref cache signature #f))
		       => (lambda (methods) methods))
		      (else
		       (let ((methods (%compute-applicable-methods signature method-alist)))
			 (unless cache
			   (set! cache (make-hashtable signature-hash eq?)))
			 (hashtable-set! cache signature methods)
			 methods))))

	       (method-called?  #f)

	       (is-a-next-method-available?
		(lambda ()
		  (null? applicable-methods)))

	       (apply-function
		(lambda (f) (apply-function/stx f)))

	       (call-methods
		(lambda ()
		  (cond ((pair? applicable-methods)
			 (unless method-called?
			   (set! method-called? #t))
			 (apply-function/stx (consume-closure applicable-methods)))
			(method-called?
			 (assertion-violation #f
			   "called next method but no more methods available"))
			(else
			 (%assert-no-methods signature))))))
	    (parametrise ((next-method-func-parm call-methods)
			  (next-method-pred-parm is-a-next-method-available?))
	      (call-methods))))))))


;;;; syntaxes to define and add methods

(define-syntax declare-method
  ;;Define a new method and store it in the given generic function.
  ;;
  (syntax-rules ()

    ;;This is for the syntax:
    ;;
    ;;	(declare-method (doit (a <alpha>) (b <beta>))
    ;;	  ---)
    ;;
    ((_ (?generic-function . ?args) . ?body)
     (%collect-types-and-arguments ?generic-function ?args () () . ?body))

    ;;This is for the syntax:
    ;;
    ;;	(declare-method doit ((a <alpha>) (b <beta>))
    ;;	  ---)
    ;;
    ((_ ?generic-function ?args . ?body)
     (%collect-types-and-arguments ?generic-function ?args () () . ?body))))

(define-syntax %collect-types-and-arguments
  ;;Analyse the list  of method arguments collecting a  list of names, a
  ;;list of types and a boolean representing the rest argument.  Finally
  ;;call the ADD-METHOD syntax to add the method.
  ;;
  (syntax-rules ()
    ((_ ?generic-function ((?next-arg-name ?next-record-name) . ?args)
	(?record-name ...)
	(?arg-name    ...) . ?body)
     ;;Matches the  form when  the next argument  to be processed  has a
     ;;type.
     (%collect-types-and-arguments ?generic-function ?args
				   (?record-name ... ?next-record-name)
				   (?arg-name    ... ?next-arg-name)
				   . ?body))

    ((_ ?generic-function (?next-arg-name . ?args) (?record-name ...) (?arg-name ...) . ?body)
     ;;Matches the  form when the next  argument to be  processed has no
     ;;type.
     (%collect-types-and-arguments ?generic-function ?args
				   (?record-name ... <top>)
				   (?arg-name    ... ?next-arg-name)
				   . ?body))

    ((_ ?generic-function () (?record-name ...) (?arg-name ...) . ?body)
     ;;Matches the form  when all the arguments have  been processed and
     ;;NO  rest argument  is present.   This  MUST come  before the  one
     ;;below.
     (add-method ?generic-function (?record-name ...)
		 #f ;means no rest argument
		 (lambda (?arg-name ...) . ?body)))

    ((_ ?generic-function ?rest-name (?record-name ...) (?arg-name ...) . ?body)
     ;;Matches the form  when all the arguments have  been processed and
     ;;only the  rest argument is there.   This MUST come  after the one
     ;;above.
     (add-method ?generic-function (?record-name ...)
		 #t ;means rest argument is present
		 (lambda (?arg-name ... . ?rest-name) . ?body)))))

(define-syntax add-method
  (syntax-rules ()
    ((_ ?generic-function (?record-name ...) ?has-rest ?closure)
     ((?generic-function :method-adder)
      (list (record-type-descriptor ?record-name) ...) ;this is the signature
      ?has-rest ?closure))))


;;;; method alists
;;
;;The  collection of methods  in a  generic function  is an  alist; each
;;entry has the format:
;;
;;	((has-rest . signature) . closure)
;;
;;the key is a pair whose CAR  is the HAS-REST boolean, and whose CDR is
;;the  SIGNATURE of  the  method;  this allows  two  methods with  equal
;;signatures to be distinct if one supports rest arguments and the other
;;does not.
;;

(define make-method-entry-key		cons)

(define-syntax method-alist-cons
  (syntax-rules ()
    ((_ ?key ?closure ?method-alist)
     (cons (cons ?key ?closure) ?method-alist))))

(define method-entry-key		car)
(define method-entry-closure		cdr)

(define method-entry-accept-rest?	caar)
(define method-entry-signature		cdar)

(define method-entry-closure-set!	set-cdr!)


;;;; actually adding methods

(define (%add-method-to-method-alist method-alist signature has-rest closure)
  ;;Add a  method's entry to the  alist of methods;  return the modified
  ;;method alist.
  ;;
  ;;A new method entry is added  only if no method with the selected key
  ;;already  exists.  If  a  method  with the  key  already exists,  its
  ;;closure is overwritten with the new one.
  ;;
  (let ((key (make-method-entry-key has-rest signature)))
    (cond ((find (lambda (method-entry)
		   (for-all* eq? key (method-entry-key method-entry)))
		 method-alist)
	   => (lambda (method-entry)
		(method-entry-closure-set! method-entry closure)
		method-alist))
	  (else
	   (method-alist-cons key closure method-alist)))))


;;;; methods dispatching

(define (%compute-applicable-methods call-signature method-alist)
  ;;Filter out from  METHOD-ALIST the methods not applicable  to a tuple
  ;;of arguments with types in  the tuple CALL-SIGNATURE.  Then sort the
  ;;list of applicable  methods so that the more  specific are the first
  ;;ones.  Return the sorted list of applicable closures.
  ;;
  (map method-entry-closure
    (list-sort
     (lambda (method1 method2)
       (%more-specific-method? method1 method2 call-signature))
     (filter
	 (lambda (method)
	   (%applicable-method? call-signature
				(method-entry-signature    method)
				(method-entry-accept-rest? method)))
       method-alist))))

(define (%applicable-method? call-signature signature has-rest)
  ;;Return true if a method with SIGNATURE as tuple of arguments' record
  ;;types can be  applied to a tuple of  arguments having CALL-SIGNATURE
  ;;as record types.  HAS-REST must  be true if the method supports rest
  ;;arguments.
  (let ((len      (length signature))
	(call-len (length call-signature)))
    (cond
     ;;If SIGNATURE has  the same length of the  call signature, test it
     ;;for applicability.
     ((= call-len len)
      (for-all* record-type-parent? call-signature signature))

     ;;If  the closure  supports rest  arguments, compare  only  as much
     ;;record types as there are in SIGNATURE.
     ((and has-rest (> call-len len))
      (for-all* record-type-parent? (take-left call-signature len) signature))

     ;;This method is not applicable.
     (else #f))))

(define (%more-specific-method? method1 method2 call-signature)
  ;;Return true if METHOD1 is more specific than METHOD2 with respect to
  ;;CALL-SIGNATURE.   This  function   must  be  applied  to  applicable
  ;;methods.  The longest signature is more specific, by definition.
  ;;
  (let* ((signature1	(cdar method1))
	 (signature2	(cdar method2))
	 (len1		(length signature1))
	 (len2		(length signature2)))
    (cond ((> len1 len2) #t)
	  ((< len1 len2) #f)
	  (else ;(= len1 len2)
	   (let loop ((signature1     signature1)
		      (signature2     signature2)
		      (call-signature call-signature))
	     (if (null? signature1)

		 ;;If we  are here: The two signatures  have EQ?  values
		 ;;(and  equal  length).    We  want  this:  If  METHOD2
		 ;;supports  rest arguments and  METHOD1 does  not, then
		 ;;METHOD1  is  more  specific.   This test  reduces  to
		 ;;testing if METHOD2 supports rest arguments.
		 (method-entry-accept-rest? method2)

	       (let ((rtd1 (car signature1))
		     (rtd2 (car signature2)))
		 (cond
		  ((eq? rtd1 rtd2)
		   (loop (cdr signature1) (cdr signature2) (cdr call-signature)))
		  ((record-type-parent? rtd1 rtd2) #t)
		  (else #f)))))))))


;;;; generic functions merging

(define (merge-generic-functions gf . generics)
  (let ((ma  (merge-method-alists (gf :method-alist)
				  (map (lambda (gf)
					 (gf :method-alist))
				    generics)))
	(new (make-generic-function)))
    (new :method-alist-set! ma)
    new))

(define-syntax list-copy
  (syntax-rules ()
    ((_ ?ell)
     (let loop ((ell ?ell))
       (if (pair? ell)
	   (cons (car ell) (loop (cdr ell)))
	 ell)))))

(define-syntax merge-method-alists
  (syntax-rules ()
    ((_ ?ma ?method-alists)
     (let loop ((ma		(list-copy ?ma))
		(method-alists	?method-alists))
       (if (null? method-alists)
	   ma
	 (loop (merge-two-method-alists ma (car method-alists))
	       (cdr method-alists)))))))

(define-syntax merge-two-method-alists
  ;;Merge ?MA1 into ?MA and return a new alist.
  ;;
  (syntax-rules ()
    ((_ ?ma ?ma1)
     (let loop ((ma  ?ma)
		(ma1 ?ma1))
       (if (null? ma1)
	   ma
	 (loop (maybe-merge-method (car ma1) ma) (cdr ma1)))))))

(define-syntax maybe-merge-method
  ;;Add CANDIDATE-METHOD-ENTRY to METHOD-ALIST and return the new alist.
  ;;Adding happens  only if  a method with  the same signature  and rest
  ;;arguments support does not already exist in METHOD-ALIST.
  ;;
  (syntax-rules ()
    ((_ ?candidate-method-entry ?method-alist)
     (let* ((candidate-method-entry	?candidate-method-entry)
	    (method-alist		?method-alist)
	    (key			(method-entry-key candidate-method-entry)))
       (unless (find (lambda (method-entry)
		       (for-all* eq? key (method-entry-key method-entry)))
		     method-alist)
	 (cons candidate-method-entry method-alist))))))


(define (signature-hash signature)
  (fold-left (lambda (nil rtd)
	       (+ nil (symbol-hash (record-type-name rtd))))
	     0
	     signature))


;;;; predefined generic functions

(define-generic object->string)

(declare-method (object->string o)
  (call-with-string-output-port
   (lambda (port)
     (display o port))))


;;;; done

)

;;; end of file
