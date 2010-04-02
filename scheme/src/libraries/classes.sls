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
    define-class			define/with

    ;; constructors
    make

    ;; predicates
    is-a?				record-is-a?
    record-type-parent?

    ;; inspection
    record-type-of
    record-parent-list			record-parent-list*

    ;; field access
    with-fields
    let-fields				let*-fields
    letrec-fields			letrec*-fields

    ;; builtin conventional record type names
    <top> <builtin>
    <pair> <list>
    <char> <string> <vector> <bytevector> <hashtable>
    <record> <condition>
    <port> <binary-port> <input-port> <output-port> <textual-port>
    <fixnum> <flonum> <integer> <integer-valued> <rational> <rational-valued>
    <real> <real-valued> <complex> <number>

    with-record-fields-of-<top>
    with-record-fields-of-<builtin>
    with-record-fields-of-<pair>
    with-record-fields-of-<list>
    with-record-fields-of-<char>
    with-record-fields-of-<string>
    with-record-fields-of-<vector>
    with-record-fields-of-<bytevector>
    with-record-fields-of-<hashtable>
    with-record-fields-of-<record>
    with-record-fields-of-<condition>
    with-record-fields-of-<port>
    with-record-fields-of-<binary-port>
    with-record-fields-of-<input-port>
    with-record-fields-of-<output-port>
    with-record-fields-of-<textual-port>
    with-record-fields-of-<fixnum>
    with-record-fields-of-<flonum>
    with-record-fields-of-<integer>
    with-record-fields-of-<integer-valued>
    with-record-fields-of-<rational>
    with-record-fields-of-<rational-valued>
    with-record-fields-of-<real>
    with-record-fields-of-<real-valued>
    with-record-fields-of-<complex>
    with-record-fields-of-<number>)
  (import (rnrs)
    (only (language-extensions)
	  begin0
	  with-accessor-and-mutator))


;;;; helpers

(define-syntax make-list
  (syntax-rules ()
    ((_ ?len ?fill)
     (let ((len ?len))
       (do ((i 0 (+ 1 i))
	    (result '() (cons ?fill result)))
	   ((= i ?len)
	    result))))))


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
      (parent-rtd	?pad ...)
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
    ;;%DEFINE-CLASS/TOP-as-PARENT.
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
     (%define-class/top-as-parent
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
      (nongenerative	?non ...)))
    ))

(define-syntax %define-class/top-as-parent
  ;;If  the class  definition used  neither  the PARENT  clause nor  the
  ;;PARENT-RTD clause,  make the type derived by  "<top>".  Finally hand
  ;;everything to %DEFINE-CLASS/ENSURE-NONGENERATIVE.
  ;;
  (syntax-rules ()

    ((_ (?name ...)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(fields		?fie ...)
	(parent)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd)
	(nongenerative	?non ...))
     (%define-class/ensure-nongenerative
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		<top>)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd)
      (nongenerative	?non ...)))

    ((_ (?name ...)
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
	(nongenerative	?non ...))
     (%define-class/ensure-nongenerative
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
      (nongenerative	?non ...)))
    ))

(define-syntax %define-class/ensure-nongenerative
  ;;If the class  definition used no NONGENERATIVE clause,  add an empty
  ;;NONGENERATIVE     clause.      Finally     hand    everything     to
  ;;%DEFINE-CLASS/FILTER-UNUSED.
  ;;
  (syntax-rules ()

    ((_ (?name ...)
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
	(nongenerative))
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
      (nongenerative)))

    ((_ (?name ...)
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
	       ((_ ?name ?body0 ?body (... ...))
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


;;;; fields access syntaxes

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

(define-syntax let-fields
  (syntax-rules ()
    ((_ (((?var ?class) ?init) ...) ?body0 ?body ...)
     (let ((?var ?init) ...)
       (with-fields ((?class ?var) ...) ?body0 ?body ...)))))

(define-syntax let*-fields
  (syntax-rules ()
    ((_ (((?var0 ?class0) ?init0) ((?var ?class) ?init) ...) ?body0 ?body ...)
     (let ((?var0 ?init0))
       (with-fields ((?class0 ?var0))
	 (let*-fields (((?var ?class) ?init) ...) ?body0 ?body ...))))

    ((_ () ?body0 ?body ...)
     (begin ?body0 ?body ...))))

(define-syntax letrec-fields
  (syntax-rules ()
    ((_ (((?var ?class) ?init) ...) ?body0 ?body ...)
     (let ((?var #f) ...)
       (with-fields ((?class ?var) ...)
	 (set! ?var ?init) ...
	 ?body0 ?body ...)))))

(define-syntax letrec*-fields
  (syntax-rules ()
    ((_ (((?var ?class) ?init) ...) ?body0 ?body ...)
     (let ((?var #f) ...)
       (with-fields ((?class ?var) ...)
	 (set! ?var ?init) ...
	 ?body0 ?body ...)))))

(define-syntax define/with
  (syntax-rules ()
    ((_ (?name (?arg ?class) ...) ?body0 ?body ...)
     (define (?name ?arg ...)
       (with-fields ((?class ?arg) ...)
	 ?body0 ?body ...)))

    ((_ (?name ?arg ...) ?body0 ?body ...)
     (define (?name ?arg ...) ?body0 ?body ...))

    ((_ ?name ?expr)
     (define ?name ?expr))

    ((_ ?name)
     (define ?name))))


(define-record-type <top>
  (nongenerative nausicaa:builtin:<top>))

(define-syntax with-record-fields-of-<top>
  (syntax-rules ()
    ((_ ?name ?body0 ?body ...)
     (begin ?body0 ?body ...))))

(define-class <builtin>
  (nongenerative nausicaa:builtin:<builtin>))

;;; --------------------------------------------------------------------

(define-syntax define-builtin-class
  (lambda (stx)
    (define (%uid name)
      (string->symbol (string-append "nausicaa:builtin:" (symbol->string name))))
    (syntax-case stx ()
      ((_ ?name ?clause ...)
       (with-syntax ((UID (datum->syntax #'?name (%uid (syntax->datum #'?name)))))
	 #'(define-class ?name
	     (parent <builtin>)
	     (nongenerative UID)
	     ?clause ...))))))

(define-builtin-class <pair>
  (virtual-fields (immutable car car)
		  (immutable cdr cdr)))

(define-builtin-class <list>
  (virtual-fields (immutable car car)
		  (immutable cdr cdr)
		  (immutable length length)))

(define-builtin-class <char>
  (virtual-fields (immutable upcase	char-upcase)
		  (immutable downcase	char-downcase)
		  (immutable titlecase	char-titlecase)
		  (immutable foldcase	char-foldcase)))

(define-builtin-class <string>
  (virtual-fields (immutable length	string-length)
		  (immutable upcase	string-upcase)
		  (immutable downcase	string-downcase)
		  (immutable titlecase	string-titlecase)
		  (immutable foldcase	string-foldcase)))

(define-builtin-class <vector>
  (virtual-fields (immutable length vector-length)))

(define-builtin-class <bytevector>
  (virtual-fields (immutable length bytevector-length)))

(define-builtin-class <hashtable>
  (virtual-fields (immutable size hashtable-size)
		  (immutable keys hashtable-keys)
		  (immutable entries hashtable-entries)))

;;; --------------------------------------------------------------------

(define-builtin-class <record>)

(define-builtin-class <condition>
  (virtual-fields (immutable message	condition-message)
		  (immutable who	condition-who)
		  (immutable irritants	condition-irritants)))

;;; --------------------------------------------------------------------

(define-builtin-class <port>
  (virtual-fields (immutable transcoder port-transcoder)
		  (immutable textual? textual-port?)
		  (immutable binary? binary-port?)
		  (immutable has-port-position? port-has-port-position?)
		  (immutable has-set-port-position? port-has-set-port-position!?)
		  (mutable port-position port-position set-port-position!)
		  (immutable eof? port-eof?)
		  (immutable input? input-port?)
		  (immutable output? output-port?)))

(define-class <input-port>
  (parent <port>)
  (nongenerative nausicaa:builtin:<input-port>))

(define-class <output-port>
  (parent <port>)
  (nongenerative nausicaa:builtin:<output-port>))

(define-class <binary-port>
  (parent <port>)
  (nongenerative nausicaa:builtin:<binary-port>))

(define-class <textual-port>
  (parent <port>)
  (nongenerative nausicaa:builtin:<textual-port>))

;;; --------------------------------------------------------------------

(define-builtin-class <number>
  (virtual-fields (immutable exact	exact)
		  (immutable inexact	inexact)

		  (immutable exact?	exact?)
		  (immutable inexact?	inexact?)

		  (immutable zero?	zero?)
		  (immutable positive?	positive?)
		  (immutable negative?	negative?)

		  (immutable odd?	odd?)
		  (immutable even?	even?)

		  (immutable finite?	finite?)
		  (immutable infinite?	infinite?)
		  (immutable nan?	nan?)

		  (immutable real-part	real-part)
		  (immutable imag-part	imag-part)
		  (immutable magnitude	magnitude)
		  (immutable angle	angle)

		  (immutable numerator	numerator)
		  (immutable denominator denominator)

		  (immutable floor	floor)
		  (immutable ceiling	ceiling)
		  (immutable truncate	truncate)
		  (immutable round	round)))

(define-class <complex>
  (parent <number>)
  (nongenerative nausicaa:builtin:<complex>))

(define-class <real-valued>
  (parent <complex>)
  (nongenerative nausicaa:builtin:<real-valued>))

(define-class <real>
  (parent <real-valued>)
  (nongenerative nausicaa:builtin:<real>))

(define-class <rational-valued>
  (parent <real>)
  (nongenerative nausicaa:builtin:<rational-valued>))

(define-class <flonum>
  (parent <real>)
  (nongenerative nausicaa:builtin:<flonum>))

(define-class <rational>
  (parent <rational-valued>)
  (nongenerative nausicaa:builtin:<rational>))

(define-class <integer-valued>
  (parent <rational-valued>)
  (nongenerative nausicaa:builtin:<integer-valued>))

(define-class <integer>
  (parent <integer-valued>)
  (nongenerative nausicaa:builtin:<integer>))

(define-class <fixnum>
  (parent <integer>)
  (nongenerative nausicaa:builtin:<fixnum>))


;;;; constructors

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

(define (record-type-parent? rtd1 rtd2)
  (cond ((eq? (record-type-uid rtd1) (record-type-uid rtd2))	#t)
	((eq? (record-type-uid rtd1) (record-type-uid (record-type-descriptor <top>)))   #f)
   	((eq? (record-type-uid rtd2) (record-type-uid (record-type-descriptor <top>)))   #t)
	(else
	 (memq (record-type-uid rtd2) (map record-type-uid (record-parent-list rtd1))))))

(define (record-is-a? obj rtd)
  (eq? (record-type-uid rtd) (record-type-uid (record-type-of obj))))

(define-syntax is-a?
  (syntax-rules ()
    ((_ ?obj ?record-name)
     ((%record-predicate (record-type-descriptor ?record-name)) ?obj))))

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


;;;; inspection

(define-syntax record-parent-list*
  (syntax-rules ()
    ((_ ?record-name)
     (record-parent-list (record-type-descriptor ?record-name)))))

(define (record-parent-list rtd)
  (let loop ((cls (list rtd))
	     (rtd (record-type-parent rtd)))
    (if rtd
	(loop (cons rtd cls) (record-type-parent rtd))
      (reverse cls))))

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


;;;; done

)

;;; end of file
