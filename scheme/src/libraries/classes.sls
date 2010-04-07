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

    define-class			make
    define/with				define/with*
    lambda/with				lambda/with*
    case-lambda/with			case-lambda/with*
    let-fields				let*-fields
    letrec-fields			letrec*-fields
    with-fields
    is-a?				record-is-a?
    record-type-parent?
    record-type-of
    record-parent-list			record-parent-list*

    <top> <builtin>
    <pair> <list>
    <char> <string> <vector> <bytevector> <hashtable>
    <record> <condition>
    <port> <binary-port> <input-port> <output-port> <textual-port>
    <fixnum> <flonum> <integer> <integer-valued> <rational> <rational-valued>
    <real> <real-valued> <complex> <number>

    <top>-with-record-fields-of
    <builtin>-with-record-fields-of
    <pair>-with-record-fields-of
    <list>-with-record-fields-of
    <char>-with-record-fields-of
    <string>-with-record-fields-of
    <vector>-with-record-fields-of
    <bytevector>-with-record-fields-of
    <hashtable>-with-record-fields-of
    <record>-with-record-fields-of
    <condition>-with-record-fields-of
    <port>-with-record-fields-of
    <binary-port>-with-record-fields-of
    <input-port>-with-record-fields-of
    <output-port>-with-record-fields-of
    <textual-port>-with-record-fields-of
    <fixnum>-with-record-fields-of
    <flonum>-with-record-fields-of
    <integer>-with-record-fields-of
    <integer-valued>-with-record-fields-of
    <rational>-with-record-fields-of
    <rational-valued>-with-record-fields-of
    <real>-with-record-fields-of
    <real-valued>-with-record-fields-of
    <complex>-with-record-fields-of
    <number>-with-record-fields-of)
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
			virtual-fields methods method)

    ((_ (?name ?constructor ?predicate) ?clause ...)
     (%define-class/sort-clauses
      (quote (define-class (?name ?constructor ?predicate) ?clause ...))
      (?name ?constructor ?predicate)
      ()	;collected mutable fields
      ()	;collected immutable fields
      ()	;collected mutable virtual fields
      ()	;collected immutable virtual fields
      ()	;collected methods
      ()	;collected functions
      (fields) (parent) (protocol) (sealed) (opaque) (parent-rtd) (nongenerative)
      ?clause ...))

    ((_ ?name ?clause ...)
     (%define-class/expand-name (quote (define-class (?name ?constructor ?predicate) ?clause ...))
				?name ?clause ...))))

(define-syntax %define-class/expand-name
  (lambda (stx)
    (define (%constructor name)
      (string->symbol (string-append "make-" name)))
    (define (%predicate name)
      (string->symbol (string-append name "?")))
    (syntax-case stx (fields mutable immutable parent protocol sealed opaque parent-rtd nongenerative
			     virtual-fields methods method)
      ((_ (quote ?input-form) ?name ?clause ...)
       (let ((name (symbol->string (syntax->datum #'?name))))
	 (with-syntax ((CONSTRUCTOR  (datum->syntax #'?name (%constructor name)))
		       (PREDICATE    (datum->syntax #'?name (%predicate   name))))
	   #'(%define-class/sort-clauses
	      (quote ?input-form)
	      (?name CONSTRUCTOR PREDICATE)
	      () ;collected mutable fields
	      () ;collected immutable fields
	      () ;collected mutable virtual fields
	      () ;collected immutable virtual fields
	      () ;collected methods
	      () ;collected functions
	      (fields) (parent) (protocol) (sealed) (opaque) (parent-rtd) (nongenerative)
	      ?clause ...))))
      )))


(define-syntax %define-class/sort-clauses
  ;;Sorts all  the auxiliary  syntaxes.  Collects the  specifications of
  ;;mutable and immutable fields.   Expands the field clauses given with
  ;;no accessor and  mutator names to clauses with  accessor and mutator
  ;;names automatically built  from the record type name  (as defined by
  ;;R6RS).  Expands  the method clauses  given with no function  name to
  ;;clauses with function name  automatically built from the record type
  ;;name.
  ;;
  ;;(Note by Marco  Maggi, Thu Apr 1, 2010) The  expansion of this macro
  ;;adds all the auxiliary syntaxes which are missing in the input form;
  ;;later  the unused ones  are removed  by %DEFINE-CLASS/FILTER-UNUSED;
  ;;adding and removing  is useless, but for now it gives  me a sense of
  ;;control and expandability, so I keep it like this.
  ;;
  (lambda (stx)
    (define (%accessor name field)
      (string->symbol (string-append name "-" field)))
    (define (%mutator name field)
      (string->symbol (string-append name "-" field "-set!")))
    (define %function %accessor)
    (syntax-case stx (fields mutable immutable parent protocol sealed opaque parent-rtd nongenerative
			     virtual-fields methods method)

      ;;Gather the PARENT clause.
      ((%define-class/sort-clauses
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(parent ?parent-name) ?clause ...)
       #'(%define-class/sort-clauses
	  (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
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
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(protocol ?protocol-proc) ?clause ...)
       #'(%define-class/sort-clauses
	  (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (fields		?fie ...)
	  (parent		?par ...)
	  (protocol		?pro ... ?protocol-proc)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  ?clause ...))

      ;;Gather the SEALED clause.
      ((%define-class/sort-clauses
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol		?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(sealed ?sealed) ?clause ...)
       #'(%define-class/sort-clauses
	  (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (fields		?fie ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ... ?sealed)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  ?clause ...))

      ;;Gather the OPAQUE clause.
      ((%define-class/sort-clauses
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(opaque ?opaque) ?clause ...)
       #'(%define-class/sort-clauses
	  (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (fields		?fie ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ... ?opaque)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  ?clause ...))

      ;;Gather the PARENT-RTD clause.
      ((%define-class/sort-clauses
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(parent-rtd ?parent-rtd ?parent-cd) ?clause ...)
       #'(%define-class/sort-clauses
	  (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (fields		?fie ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ... ?parent-rtd ?parent-cd)
	  (nongenerative	?non ...)
	  ?clause ...))

      ;;Gather the NONGENERATIVE empty clause.
      ((%define-class/sort-clauses
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(nongenerative) ?clause ...)
       #'(%define-class/sort-clauses
	  (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (fields		?fie ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  ?clause ...))

      ;;Gather the NONGENERATIVE non-empty clause.
      ((%define-class/sort-clauses
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(nongenerative ?uid) ?clause ...)
       #'(%define-class/sort-clauses
	  (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (fields		?fie ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ... ?uid)
	  ?clause ...))

;;; --------------------------------------------------------------------

      ;;Gather the FIELDS clause.
      ((%define-class/sort-clauses
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields) ;must be empty here to detect multiple usages of FIELDS
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields ?field-clause ...) ?clause ...)
       #'(%define-class/sort-clauses/fields
	  (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (fields)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (fields ?field-clause ...) ?clause ...))

      ;;Raise a syntax  violation if the FIELDS clause  is used multiple
      ;;times.
      ((%define-class/sort-clauses
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie0 ?fie ...) ;at least one
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields ?field-clause ...) ?clause ...)
       #'(syntax-violation 'define-class
	   "\"fields\" clause used multiple times in class definition"
	   (quote ?input-form) (quote (fields ?field-clause ...))))

;;; --------------------------------------------------------------------

      ;;Gather the VIRTUAL-FIELDS clause.
      ((%define-class/sort-clauses
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	() ;must be empty here to detect multiple usages of VIRTUAL-FIELDS
	() ;must be empty here to detect multiple usages of VIRTUAL-FIELDS
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields ?field-clause ...) ?clause ...)
       #'(%define-class/sort-clauses/virtual-fields
	  (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ... )
	  (?collected-immutable-field ...)
	  ()
	  ()
	  (?collected-method ...)
	  (?collected-function ...)
	  (fields		?fie ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (virtual-fields ?field-clause ...) ?clause ...))

      ;;Raise a syntax  violation if the FIELDS clause  is used multiple
      ;;times.
      ((%define-class/sort-clauses
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field0 ?collected-mutable-virtual-field ...) ;at least one
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields ?field-clause ...) ?clause ...)
       #'(syntax-violation 'define-class
	   "\"virtual-fields\" clause used multiple times in class definition"
	   (quote ?input-form) (quote (virtual-fields ?field-clause ...))))
      ((%define-class/sort-clauses
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field0 ?collected-immutable-virtual-field ...) ;at least one
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields ?field-clause ...) ?clause ...)
       #'(syntax-violation 'define-class
	   "\"virtual-fields\" clause used multiple times in class definition"
	   (quote ?input-form) (quote (virtual-fields ?field-clause ...))))

;;; --------------------------------------------------------------------

      ;;Gather METHODS clause.
      ((%define-class/sort-clauses
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	() ;it has to be empty to detect mutiple use of METHODS
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(methods ?method-clause ...) ?clause ...)
       #'(%define-class/sort-clauses/methods
	  (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  ()
	  (?collected-function ...)
	  (fields		?fie ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (methods ?method-clause ...) ?clause ...))

      ;;Raise a syntax violation if  the METHODS clause is used multiple
      ;;times.
      ((%define-class/sort-clauses
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field0 ?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method0 ?collected-method ...) ;at least one
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(methods ?method-clause ...) ?clause ...)
       #'(syntax-violation 'define-class
	   "\"methods\" clause used multiple times in class definition"
	   (quote ?input-form) (quote (methods ?method-clause ...))))

;;; --------------------------------------------------------------------

      ;;Gather METHOD clause.
      ((%define-class/sort-clauses
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(method (?method . ?args) . ?body) ?clause ...)
       #'(%define-class/sort-clauses
	  (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ... (?method function-name))
	  (?collected-function ... (define/with (function-name . ?args) . ?body))
	  (fields		?fie ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  ?clause ...))

;;; --------------------------------------------------------------------

      ;;No more clauses to gather.  If the class definition used neither
      ;;the  PARENT clause  nor  the PARENT-RTD  clause,  make the  type
      ;;derived    by    "<top>".     Finally   hand    everything    to
      ;;%DEFINE-CLASS/FILTER-UNUSED.
      ;;
      ((_ (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (fields		?fie ...)
	  (parent)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd)
	  (nongenerative	?non ...))
       #'(%define-class/filter-unused
	  (quote ?input-form) (?name ?constructor ?predicate)
	  ()	;collected clauses
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (fields		?fie ...)
	  (parent		<top>)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd)
	  (nongenerative	?non ...)))

      ;;No    more   clauses    to   gather.     Hand    everything   to
      ;;%DEFINE-CLASS/FILTER-UNUSED.
      ;;
      ((_ (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (fields		?fie ...)
	  (parent		?par ...)
	  (protocol	?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd	?pad ...)
	  (nongenerative	?non ...))
       #'(%define-class/filter-unused
	  (quote ?input-form) (?name ?constructor ?predicate)
	  ()	;collected clauses
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (fields		?fie ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd	?pad ...)
	  (nongenerative	?non ...)))
      )))


(define-syntax %define-class/sort-clauses/fields
  (lambda (stx)
    (define (%accessor name field)
      (string->symbol (string-append name "-" field)))
    (define (%mutator name field)
      (string->symbol (string-append name "-" field "-set!")))
    (syntax-case stx (fields mutable immutable parent protocol sealed opaque parent-rtd nongenerative
			     virtual-fields methods method)

      ;;Gather mutable FIELDS clause with explicit selection of accessor
      ;;and mutator names.
      ((%define-class/sort-clauses/fields
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields (mutable ?field ?field-accessor ?field-mutator) ?field-clause ...) ?clause ...)
       #'(%define-class/sort-clauses/fields
	  (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ... (?field ?field-accessor ?field-mutator))
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (fields		?fie ... (mutable ?field ?field-accessor ?field-mutator))
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (fields ?field-clause ...) ?clause ...))

      ;;Gather  mutable  FIELDS   clause  with  automatically  generated
      ;;accessor and mutator names.
      ((%define-class/sort-clauses/fields
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields (mutable ?field) ?field-clause ...) ?clause ...)
       (let ((name  (symbol->string (syntax->datum #'?name)))
	     (field (symbol->string (syntax->datum #'?field))))
	 (with-syntax ((ACCESSOR (datum->syntax #'?name (%accessor name field)))
		       (MUTATOR  (datum->syntax #'?name (%mutator  name field))))
	   #'(%define-class/sort-clauses/fields
	      (quote ?input-form) (?name ?constructor ?predicate)
	      (?collected-mutable-field ... (?field ACCESSOR MUTATOR))
	      (?collected-immutable-field ...)
	      (?collected-mutable-virtual-field ...)
	      (?collected-immutable-virtual-field ...)
	      (?collected-method ...)
	      (?collected-function ...)
	      (fields		?fie ... (mutable ?field ACCESSOR MUTATOR))
	      (parent		?par ...)
	      (protocol		?pro ...)
	      (sealed		?sea ...)
	      (opaque		?opa ...)
	      (parent-rtd		?pad ...)
	      (nongenerative	?non ...)
	      (fields ?field-clause ...) ?clause ...))))

      ;;Gather  immutable  FIELDS  clause  with  explicit  selection  of
      ;;accessor name.
      ((%define-class/sort-clauses/fields
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields (immutable ?field ?accessor) ?field-clause ...) ?clause ...)
       #'(%define-class/sort-clauses/fields
	  (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ... (?field ?accessor))
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (fields		?fie ... (immutable ?field ?accessor))
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (fields ?field-clause ...) ?clause ...))

      ;;Gather  immutable  FIELDS  clause with  automatically  generated
      ;;accessor name.
      ((%define-class/sort-clauses/fields
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields (immutable ?field) ?field-clause ...) ?clause ...)
       (let ((name  (symbol->string (syntax->datum #'?name)))
	     (field (symbol->string (syntax->datum #'?field))))
	 (with-syntax ((ACCESSOR (datum->syntax #'?name (%accessor name field))))
	   #'(%define-class/sort-clauses/fields
	      (quote ?input-form) (?name ?constructor ?predicate)
	      (?collected-mutable-field ...)
	      (?collected-immutable-field ... (?field ACCESSOR))
	      (?collected-mutable-virtual-field ...)
	      (?collected-immutable-virtual-field ...)
	      (?collected-method ...)
	      (?collected-function ...)
	      (fields		?fie ... (immutable ?field ACCESSOR))
	      (parent		?par ...)
	      (protocol		?pro ...)
	      (sealed		?sea ...)
	      (opaque		?opa ...)
	      (parent-rtd		?pad ...)
	      (nongenerative	?non ...)
	      (fields ?field-clause ...) ?clause ...))))

      ;;Gather   immutable  FIELDS   clause  declared   without  IMMUTABLE
      ;;auxiliary syntax.
      ((%define-class/sort-clauses/fields
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields ?field ?field-clause ...) ?clause ...)
       (let ((name  (symbol->string (syntax->datum #'?name)))
	     (field (symbol->string (syntax->datum #'?field))))
	 (with-syntax ((ACCESSOR (datum->syntax #'?name (%accessor name field))))
	   #'(%define-class/sort-clauses/fields
	      (quote ?input-form) (?name ?constructor ?predicate)
	      (?collected-mutable-field ...)
	      (?collected-immutable-field ... (?field ACCESSOR))
	      (?collected-mutable-virtual-field ...)
	      (?collected-immutable-virtual-field ...)
	      (?collected-method ...)
	      (?collected-function ...)
	      (fields		?fie ... (immutable ?field ACCESSOR))
	      (parent		?par ...)
	      (protocol		?pro ...)
	      (sealed		?sea ...)
	      (opaque		?opa ...)
	      (parent-rtd	?pad ...)
	      (nongenerative	?non ...)
	      (fields ?field-clause ...) ?clause ...))))

      ;;Remove empty, leftover, FIELDS clause.
      ((%define-class/sort-clauses/fields
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields) ?clause ...)
       #'(%define-class/sort-clauses
	  (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (fields		?fie ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  ?clause ...))

      )))


(define-syntax %define-class/sort-clauses/virtual-fields
  (lambda (stx)
    (define (%accessor name field)
      (string->symbol (string-append name "-" field)))
    (define (%mutator name field)
      (string->symbol (string-append name "-" field "-set!")))
    (syntax-case stx (fields mutable immutable parent protocol sealed opaque parent-rtd nongenerative
			     virtual-fields methods method)

      ;;Gather mutable VIRTUAL-FIELDS  clause with explicit selection of
      ;;accessor and mutator names.
      ((%define-class/sort-clauses/virtual-fields
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields (mutable ?field ?accessor ?mutator) ?field-clause ...) ?clause ...)
       #'(%define-class/sort-clauses/virtual-fields
	  (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...  (?field ?accessor ?mutator))
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (fields		?fie ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (virtual-fields ?field-clause ...) ?clause ...))

      ;;Gather   mutable   VIRTUAL-FIELDS   clause  with   automatically
      ;;generated accessor and mutator names.
      ((%define-class/sort-clauses/virtual-fields
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields (mutable ?field) ?field-clause ...) ?clause ...)
       (let ((name  (symbol->string (syntax->datum #'?name)))
	     (field (symbol->string (syntax->datum #'?field))))
	 (with-syntax ((ACCESSOR (datum->syntax #'?name (%accessor name field)))
		       (MUTATOR  (datum->syntax #'?name (%mutator  name field))))
	   #'(%define-class/sort-clauses/virtual-fields
	      (quote ?input-form) (?name ?constructor ?predicate)
	      (?collected-mutable-field ...)
	      (?collected-immutable-field ...)
	      (?collected-mutable-virtual-field ...  (?field ACCESSOR MUTATOR))
	      (?collected-immutable-virtual-field ...)
	      (?collected-method ...)
	      (?collected-function ...)
	      (fields		?fie ...)
	      (parent		?par ...)
	      (protocol		?pro ...)
	      (sealed		?sea ...)
	      (opaque		?opa ...)
	      (parent-rtd	?pad ...)
	      (nongenerative	?non ...)
	      (virtual-fields ?field-clause ...) ?clause ...))))

      ;;Gather immutable  VIRTUAL-FIELDS clause with  explicit selection
      ;;of accessor name.
      ((%define-class/sort-clauses/virtual-fields
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields (immutable ?field ?accessor) ?field-clause ...) ?clause ...)
       #'(%define-class/sort-clauses/virtual-fields
	  (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ... (?field ?accessor))
	  (?collected-method ...)
	  (?collected-function ...)
	  (fields		?fie ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (virtual-fields ?field-clause ...) ?clause ...))

      ;;Gather   immutable  VIRTUAL-FIELDS  clause   with  automatically
      ;;generated accessor name.
      ((%define-class/sort-clauses/virtual-fields
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields (immutable ?field) ?field-clause ...) ?clause ...)
       (let ((name  (symbol->string (syntax->datum #'?name)))
	     (field (symbol->string (syntax->datum #'?field))))
	 (with-syntax ((ACCESSOR (datum->syntax #'?name (%accessor name field))))
	   #'(%define-class/sort-clauses/virtual-fields
	      (quote ?input-form) (?name ?constructor ?predicate)
	      (?collected-mutable-field ...)
	      (?collected-immutable-field ...)
	      (?collected-mutable-virtual-field ...)
	      (?collected-immutable-virtual-field ... (?field ACCESSOR))
	      (?collected-method ...)
	      (?collected-function ...)
	      (fields		?fie ...)
	      (parent		?par ...)
	      (protocol		?pro ...)
	      (sealed		?sea ...)
	      (opaque		?opa ...)
	      (parent-rtd	?pad ...)
	      (nongenerative	?non ...)
	      (virtual-fields ?field-clause ...) ?clause ...))))

      ;;Gather immutable VIRTUAL-FIELDS  clause declared without IMMUTABLE
      ;;auxiliary syntax.
      ((%define-class/sort-clauses/virtual-fields
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields ?field ?field-clause ...) ?clause ...)
       (let ((name  (symbol->string (syntax->datum #'?name)))
	     (field (symbol->string (syntax->datum #'?field))))
	 (with-syntax ((ACCESSOR (datum->syntax #'?name (%accessor name field))))
	   #'(%define-class/sort-clauses/virtual-fields
	      (quote ?input-form) (?name ?constructor ?predicate)
	      (?collected-mutable-field ...)
	      (?collected-immutable-field ...)
	      (?collected-mutable-virtual-field ...)
	      (?collected-immutable-virtual-field ... (?field ACCESSOR))
	      (?collected-method ...)
	      (?collected-function ...)
	      (fields		?fie ...)
	      (parent		?par ...)
	      (protocol		?pro ...)
	      (sealed		?sea ...)
	      (opaque		?opa ...)
	      (parent-rtd	?pad ...)
	      (nongenerative	?non ...)
	      (virtual-fields ?field-clause ...) ?clause ...))))

      ;;Remove empty, leftover, VIRTUAL-FIELDS clause.
      ((%define-class/sort-clauses/virtual-fields
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields) ?clause ...)
       #'(%define-class/sort-clauses
	  (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (fields		?fie ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  ?clause ...))
      )))


(define-syntax %define-class/sort-clauses/methods
  (lambda (stx)
    (define (%function name field)
      (string->symbol (string-append name "-" field)))
    (syntax-case stx (fields mutable immutable parent protocol sealed opaque parent-rtd nongenerative
			     virtual-fields methods method)

      ;;Gather METHODS clause with explicit selection of function name.
      ((%define-class/sort-clauses/methods
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(methods (?method ?function) ?method-clause ...) ?clause ...)
       #'(%define-class/sort-clauses/methods
	  (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ... (?method ?function))
	  (?collected-function ...)
	  (fields		?fie ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (methods ?method-clause ...) ?clause ...))

      ;;Gather  METHODS  clause  with automatically  generated  function
      ;;name.
      ((%define-class/sort-clauses/methods
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(methods (?method) ?method-clause ...) ?clause ...)
       (let ((name  (symbol->string (syntax->datum #'?name)))
	     (field (symbol->string (syntax->datum #'?method))))
	 (with-syntax ((FUNCTION (datum->syntax #'?name (%function name field))))
	   #'(%define-class/sort-clauses/methods
	      (quote ?input-form) (?name ?constructor ?predicate)
	      (?collected-mutable-field ...)
	      (?collected-immutable-field ...)
	      (?collected-mutable-virtual-field ...)
	      (?collected-immutable-virtual-field ...)
	      (?collected-method ... (?method FUNCTION))
	      (?collected-function ...)
	      (fields		?fie ...)
	      (parent		?par ...)
	      (protocol		?pro ...)
	      (sealed		?sea ...)
	      (opaque		?opa ...)
	      (parent-rtd	?pad ...)
	      (nongenerative	?non ...)
	      (methods ?method-clause ...) ?clause ...))))

      ;;Gather METHODS clause declared with only the symbol.
      ((%define-class/sort-clauses/methods
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(methods ?method ?method-clause ...) ?clause ...)
       (let ((name  (symbol->string (syntax->datum #'?name)))
	     (field (symbol->string (syntax->datum #'?method))))
	 (with-syntax ((FUNCTION (datum->syntax #'?name (%function name field))))
	   #'(%define-class/sort-clauses/methods
	      (quote ?input-form) (?name ?constructor ?predicate)
	      (?collected-mutable-field ...)
	      (?collected-immutable-field ...)
	      (?collected-mutable-virtual-field ...)
	      (?collected-immutable-virtual-field ...)
	      (?collected-method ... (?method FUNCTION))
	      (?collected-function ...)
	      (fields		?fie ...)
	      (parent		?par ...)
	      (protocol		?pro ...)
	      (sealed		?sea ...)
	      (opaque		?opa ...)
	      (parent-rtd	?pad ...)
	      (nongenerative	?non ...)
	      (methods ?method-clause ...) ?clause ...))))

      ;;Remove empty, leftover, METHODS clause.
      ((%define-class/sort-clauses/methods
	(quote ?input-form) (?name ?constructor ?predicate)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(methods) ?clause ...)
       #'(%define-class/sort-clauses
	  (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-mutable-field ...)
	  (?collected-immutable-field ...)
	  (?collected-mutable-virtual-field ...)
	  (?collected-immutable-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (fields		?fie ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  ?clause ...))

      )))


(define-syntax %define-class/filter-unused
  ;;Removes auxiliary syntaxes which cannot be empty.
  ;;
  (lambda (stx)
    (syntax-case stx (fields parent protocol sealed opaque parent-rtd nongenerative)

      ;;Remove unused PARENT form.
      ((%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				    (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (?collected-method ...)
				    (?collected-function ...)
				    (parent)
				    ?clause ...)
       #'(%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				      (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      (?collected-method ...)
				      (?collected-function ...)
				      ?clause ...))
      ;;Collect used PARENT form.
      ((%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				    (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (?collected-method ...)
				    (?collected-function ...)
				    (parent ?e0 ?e ...)
				    ?clause ...)
       #'(%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				      (?collected-clause ... (parent ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      (?collected-method ...)
				      (?collected-function ...)
				      ?clause ...))

      ;;Remove unused FIELDS form.
      ((%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				    (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (?collected-method ...)
				    (?collected-function ...)
				    (fields)
				    ?clause ...)
       #'(%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				      (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      (?collected-method ...)
				      (?collected-function ...)
				      ?clause ...))
      ;;Collect used FIELDS form.
      ((%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				    (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (?collected-method ...)
				    (?collected-function ...)
				    (fields ?e0 ?e ...)
				    ?clause ...)
       #'(%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				      (?collected-clause ... (fields ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      (?collected-method ...)
				      (?collected-function ...)
				      ?clause ...))

      ;;Remove unused PROTOCOL form.
      ((%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				    (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (?collected-method ...)
				    (?collected-function ...)
				    (protocol)
				    ?clause ...)
       #'(%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				      (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      (?collected-method ...)
				      (?collected-function ...)
				      ?clause ...))
      ;;Collect used PROTOCOL form.
      ((%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				    (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (?collected-method ...)
				    (?collected-function ...)
				    (protocol ?e0 ?e ...)
				    ?clause ...)
       #'(%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				      (?collected-clause ... (protocol ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      (?collected-method ...)
				      (?collected-function ...)
				      ?clause ...))

      ;;Remove unused SEALED form.
      ((%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				    (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (?collected-method ...)
				    (?collected-function ...)
				    (sealed)
				    ?clause ...)
       #'(%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				      (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      (?collected-method ...)
				      (?collected-function ...)
				      ?clause ...))
      ;;Collect used SEALED form.
      ((%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				    (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (?collected-method ...)
				    (?collected-function ...)
				    (sealed ?e0 ?e ...) ?clause ...)
       #'(%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				      (?collected-clause ... (sealed ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      (?collected-method ...)
				      (?collected-function ...)
				      ?clause ...))

      ;;Remove unused OPAQUE form.
      ((%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				    (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (?collected-method ...)
				    (?collected-function ...)
				    (opaque) ?clause ...)
       #'(%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				      (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      (?collected-method ...)
				      (?collected-function ...)
				      ?clause ...))
      ;;Collect used OPAQUE form.
      ((%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				    (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (?collected-method ...)
				    (?collected-function ...)
				    (opaque ?e0 ?e ...) ?clause ...)
       #'(%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				      (?collected-clause ... (opaque ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      (?collected-method ...)
				      (?collected-function ...)
				      ?clause ...))

      ;;Remove unused PARENT-RTD form.
      ((%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				    (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (?collected-method ...)
				    (?collected-function ...)
				    (parent-rtd) ?clause ...)
       #'(%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				      (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      (?collected-method ...)
				      (?collected-function ...)
				      ?clause ...))
      ;;Collect used PARENT-RTD form.
      ((%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				    (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (?collected-method ...)
				    (?collected-function ...)
				    (parent-rtd ?e0 ?e ...) ?clause ...)
       #'(%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				      (?collected-clause ... (parent-rtd ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      (?collected-method ...)
				      (?collected-function ...)
				      ?clause ...))

      ;;Collect NONGENERATIVE form.
      ((%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				    (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (?collected-method ...)
				    (?collected-function ...)
				    (nongenerative ?uid ...)
				    ?clause ...)
       #'(%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				      (?collected-clause ... (nongenerative ?uid ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      (?collected-method ...)
				      (?collected-function ...)
				      ?clause ...))

      ;;No    more    clauses   to    filter,    hand   everything    to
      ;;%DEFINE-CLASS/EXPAND-FIELD-NAMES.
      ((%define-class/filter-unused (quote ?input-form) (?name ?constructor ?predicate)
				    (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (?collected-method ...)
				    (?collected-function ...))
       #'(%define-class/output-forms (quote ?input-form) (?name ?constructor ?predicate)
				     (?collected-clause ...)
				     (?collected-mutable-field ...)
				     (?collected-immutable-field ...)
				     (?collected-mutable-virtual-field ...)
				     (?collected-immutable-virtual-field ...)
				     (?collected-method ...)
				     (?collected-function ...)))
      )))


(define-syntax %define-class/output-forms
  (lambda (stx)
    (define (%with-fields name)
      (string->symbol (string-append (symbol->string name) "-with-record-fields-of")))
    (define (duplicated-ids? ell)
      (if (null? ell)
	  #f
	(let inner ((x  (car ell))
		    (ls (cdr ell)))
	  (if (null? ls)
	      (duplicated-ids? (cdr ell))
	    (if (bound-identifier=? x (car ls))
		x
	      (inner x (cdr ls)))))))
    (syntax-case stx ()

      ((_ (quote ?input-form) (?name ?constructor ?predicate)
	  (?collected-clause ...)
	  ((?mutable-field ?mf ...) ...)
	  ((?immutable-field ?if ...) ...)
	  ((?virtual-mutable-field ?vmf ...) ...)
	  ((?virtual-immutable-field ?vif ...) ...)
	  ((?method ?m ...) ...)
	  (?collected-function ...))
       (let ((id (duplicated-ids? #'(?mutable-field
				     ... ?immutable-field ...
				     ?virtual-mutable-field ...
				     ?virtual-immutable-field ...
				     ?method ...))))
	 (if id
	     #`(raise (condition
		       (make-who-condition 'define-class)
		       (make-message-condition "duplicated field names in class definition")
		       (make-irritants-condition (quote (#,id)))
		       (make-syntax-violation (quote ?input-form) #f)))
	   (with-syntax ((WITH-FIELDS (datum->syntax #'?name (%with-fields (syntax->datum #'?name)))))
	     #'(begin
		 (define-record-type (?name ?constructor ?predicate) ?collected-clause ...)
		 ?collected-function ...
		 (define-syntax WITH-FIELDS
		   (syntax-rules ()
		     ((_ ?name ?body0 ?body (... ...))
		      (%with-record-fields ?name ((?mutable-field ?mf ...) ...
						  (?immutable-field ?if ...) ...
						  (?virtual-mutable-field ?vmf ...) ...
						  (?virtual-immutable-field ?vif ...) ...)
					   (%with-methods ?name ((?method ?m ...) ...)
							  ?body0 ?body (... ...))
					   ))))))
	   )))
      )))

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

(define-syntax %with-methods
  (lambda (stx)
    (define (%field name field)
      (string->symbol (string-append (symbol->string name) "." (symbol->string field))))
    (syntax-case stx ()

      ((_ ?name ((?field ?function-name) ?clause ...) ?body0 ?body ...)
       (with-syntax ((FIELD (datum->syntax #'?name (%field (syntax->datum #'?name)
							   (syntax->datum #'?field)))))
	 #'(let-syntax ((FIELD (syntax-rules ()
				 ((_ ?arg (... ...))
				  (?function-name ?name ?arg (... ...))))))
	     (%with-methods ?name (?clause ...) ?body0 ?body ...))))

      ;;No more field clauses, output the body.
      ((_ ?name () ?body0 ?body ...)
       #'(begin ?body0 ?body ...))
      )))


;;;; fields access syntaxes

(define-syntax with-fields
  ;;Hand  everything to  %WITH-FIELDS.   This is  because the  recursive
  ;;%WITH-FIELDS needs  to allow  input forms which  we do not  want for
  ;;WITH-FIELDS.
  ;;
  ;;We  allow  an  empty list  of  clauses  because  it is  useful  when
  ;;expanding macros into WITH-FIELDS forms.
  ;;
  (syntax-rules ()
    ((_ (?clause ...) ?body0 ?body ...)
     (%with-fields (?clause ...) ?body0 ?body ...))))

(define-syntax %with-fields
  (lambda (stx)
    (define (%accessor class)
      (string->symbol (string-append (symbol->string class) "-with-record-fields-of")))
    (syntax-case stx ()

      ((_ ((?name ?class0 ?class ...) ?clause ...) ?body0 ?body ...)
       (with-syntax ((ACCESSOR (datum->syntax #'?class0 (%accessor (syntax->datum #'?class0)))))
	 #'(ACCESSOR ?name (%with-fields ((?name ?class ...) ?clause ...) ?body0 ?body ...))))

      ((_ ((?name) ?clause ...) ?body0 ?body ...)
       #'(%with-fields (?clause ...) ?body0 ?body ...))

      ((_ () ?body0 ?body ...)
       #'(begin ?body0 ?body ...)))))

(define-syntax let-fields
  (syntax-rules ()
    ((_ (((?var ?class0 ?class ...) ?init) ...) ?body0 ?body ...)
     (let ((?var ?init) ...)
       (with-fields ((?var ?class0 ?class ...) ...) ?body0 ?body ...)))))

(define-syntax let*-fields
  (syntax-rules ()

    ((_ (((?var0 ?class0 ?class ...) ?init0) ?binding ...) ?body0 ?body ...)
     (let ((?var0 ?init0))
       (with-fields ((?var0 ?class0 ?class ...))
	 (let*-fields (?binding ...) ?body0 ?body ...))))

    ((_ () ?body0 ?body ...)
     (begin ?body0 ?body ...))))

(define-syntax letrec-fields
  (syntax-rules ()
    ((_ (((?var ?class0 ?class ...) ?init) ...) ?body0 ?body ...)
     (let ((?var #f) ...)
       (with-fields ((?var ?class0 ?class ...) ...)
	 (set! ?var ?init) ...
	 ?body0 ?body ...)))))

(define-syntax letrec*-fields
  (syntax-rules ()
    ((_ (((?var ?class0 ?class ...) ?init) ...) ?body0 ?body ...)
     (let ((?var #f) ...)
       (with-fields ((?var ?class0 ?class ...) ...)
	 (set! ?var ?init) ...
	 ?body0 ?body ...)))))


(define-syntax define/with
  (syntax-rules ()
    ((_ (?name . ?formals) . ?body)
     (define ?name
       (lambda/with ?formals . ?body)))
    ((_ ?variable ?expression)
     (define ?variable ?expression))
    ((_ ?variable)
     (define ?variable))))

(define-syntax define/with*
  (syntax-rules ()
    ((_ (?name . ?formals) . ?body)
     (define ?name (lambda/with* ?formals . ?body)))
    ((_ ?variable ?expression)
     (define ?variable ?expression))
    ((_ ?variable)
     (define ?variable))))

(define-syntax lambda/with
  (syntax-rules ()
    ((_ ?formals . ?body)
     (%lambda/collect-classes-and-arguments #f ?formals
					    () ;collected classes
					    () ;collected args
					    . ?body))))

(define-syntax lambda/with*
  (syntax-rules ()
    ((_ ?formals . ?body)
     (%lambda/collect-classes-and-arguments #t ?formals
					    () ;collected classes
					    () ;collected args
					    . ?body))))

(define-syntax %lambda/collect-classes-and-arguments
  ;;Analyse the list of formals  collecting a list of argument names and
  ;;a list of class names.
  ;;
  (syntax-rules ()

    ;;Matches when the next argument to be processed has a type.
    ((_ ?add-assertions ((?arg ?cls0 ?cls ...) . ?args) (?collected-cls ...) (?collected-arg ...) . ?body)
     (%lambda/collect-classes-and-arguments ?add-assertions ?args
					    (?collected-cls ... (?cls0 ?cls ...))
					    (?collected-arg ... ?arg)
					    . ?body))

    ;;Matches when the next argument to be processed has no type.
    ((_ ?add-assertions (?arg . ?args) (?collected-cls ...) (?collected-arg ...) . ?body)
     (%lambda/collect-classes-and-arguments ?add-assertions ?args
					    (?collected-cls ... (<top>))
					    (?collected-arg ... ?arg)
					    . ?body))

    ;;Matches when  all the  arguments have been  processed and  NO rest
    ;;argument is present.  This MUST come before the one below.
    ((_ #f () ((?collected-cls ...) ...) (?collected-arg ...) . ?body)
     (lambda (?collected-arg ...)
       (with-fields ((?collected-arg ?collected-cls ...) ...) . ?body)))
    ((_ #t () ((?collected-cls ...) ...) (?collected-arg ...) . ?body)
     (lambda (?collected-arg ...)
       (%add-assertions ((?collected-cls ...) ...) (?collected-arg ...))
       (with-fields ((?collected-arg ?collected-cls ...) ...) . ?body)))

    ;;Matches two cases: (1) when  all the arguments have been processed
    ;;and only  the rest argument is  there; (2) when the  formals is an
    ;;identifier (lambda args ---).
    ((_ #f ?rest ((?collected-cls ...) ...) (?collected-arg ...) . ?body)
     (lambda (?collected-arg ... . ?rest)
       (with-fields ((?collected-arg ?collected-cls ...) ...) . ?body)))
    ((_ #t ?rest ((?collected-cls ...) ...) (?collected-arg ...) . ?body)
     (lambda (?collected-arg ... . ?rest)
       (%add-assertions ((?collected-cls ...) ...) (?collected-arg ...))
       (with-fields ((?collected-arg ?collected-cls ...) ...) . ?body)))
    ))

(define-syntax %add-assertions
  (syntax-rules (<top>)

    ;;Drop arguments of class "<top>",  all the values are implicitly of
    ;;type "<top>".
    ((_ ((<top>) ?cls ...) (?arg0 ?arg ...))
     (%add-assertions (?cls ...) (?arg ...)))

    ;;Add an assertion.
    ((_ ((?cls0 ...) ?cls ...) (?arg0 ?arg ...))
     (begin (assert (is-a? ?arg0 ?cls0)) ...
	    (%add-assertions (?cls ...) (?arg ...))))

    ;;No more arguments.
    ((_ () ())
     (values))))


(define-syntax case-lambda/with
  (syntax-rules ()
    ((_ (?formals . ?body) ...)
     (%case-lambda/collect-classes-and-arguments
      #f
      ()	;collected CASE-LAMBDA clauses
      ()	;collected classes in current CASE-LAMBDA clause
      ()	;collected args in current CASE-LAMBDA clause
      (?formals . ?body)
      ...))))

(define-syntax case-lambda/with*
  (syntax-rules ()
    ((_ (?formals . ?body) ...)
     (%case-lambda/collect-classes-and-arguments
      #t
      ()	;collected CASE-LAMBDA clauses
      ()	;collected classes in current CASE-LAMBDA clause
      ()	;collected args in current CASE-LAMBDA clause
      (?formals . ?body)
      ...))))

(define-syntax %case-lambda/collect-classes-and-arguments
  ;;Analyse the list of formals  collecting a list of argument names and
  ;;a list of class names.
  ;;
  (syntax-rules ()

    ;;Matches when  the next  argument to be  processed, in  the current
    ;;CASE-LAMBDA clause, has a type.
    ((_ ?add-assertions
	(?collected-case-lambda-clause ...)
	(?collected-cls ...)
	(?collected-arg ...)
	(((?arg ?cls0 ?cls ...) . ?args) . ?body)
	?case-lambda-clause ...)
     (%case-lambda/collect-classes-and-arguments
      ?add-assertions
      (?collected-case-lambda-clause ...)
      (?collected-cls ... (?cls0 ?cls ...))
      (?collected-arg ... ?arg)
      (?args . ?body)
      ?case-lambda-clause ...))

    ;;Matches when  the next  argument to be  processed, in  the current
    ;;CASE-LAMBDA clause, has no type.
    ((_ ?add-assertions
    	(?collected-case-lambda-clause ...)
    	(?collected-cls ...)
    	(?collected-arg ...)
    	((?arg . ?args) . ?body)
    	?case-lambda-clause ...)
     (%case-lambda/collect-classes-and-arguments
      ?add-assertions
      (?collected-case-lambda-clause ...)
      (?collected-cls ... (<top>))
      (?collected-arg ... ?arg)
      (?args . ?body)
      ?case-lambda-clause ...))

    ;;Matches when all the arguments in a clause have been processed and
    ;;NO rest argument is present.  This MUST come before the one below.
    ((_ #f
	(?collected-case-lambda-clause ...)
	((?collected-cls ...) ...)
	(?collected-arg ...)
	(() . ?body)
	?case-lambda-clause ...)
     (%case-lambda/collect-classes-and-arguments
      #f
      (?collected-case-lambda-clause ... ((?collected-arg ...)
					  (with-fields ((?collected-arg ?collected-cls ...)
							...)
					    . ?body)))
      ()
      ()
      ?case-lambda-clause ...))
    ((_ #t
	(?collected-case-lambda-clause ...)
	((?collected-cls ...) ...)
	(?collected-arg ...)
	(() . ?body)
	?case-lambda-clause ...)
     (%case-lambda/collect-classes-and-arguments
      #t
      (?collected-case-lambda-clause ... ((?collected-arg ...)
					  (%add-assertions ((?collected-cls ...) ...)
							   (?collected-arg ...))
					  (with-fields ((?collected-arg ?collected-cls ...)
							...)
					    . ?body)))
      ()
      ()
      ?case-lambda-clause ...))

    ;;Matches two  cases: (1)  when all the  arguments in a  clause have
    ;;been processed and  only the rest argument is  there; (2) when the
    ;;formals in a clause is an identifier (args ---).
    ((_ #f
	(?collected-case-lambda-clause ...)
	((?collected-cls ...) ...)
	(?collected-arg ...)
	(?rest . ?body)
	?case-lambda-clause ...)
     (%case-lambda/collect-classes-and-arguments
      #f
      (?collected-case-lambda-clause ... ((?collected-arg ... . ?rest)
					  (with-fields ((?collected-arg ?collected-cls ...)
							...)
					    . ?body)))
      ()
      ()
      ?case-lambda-clause ...))
    ((_ #t
	(?collected-case-lambda-clause ...)
	((?collected-cls ...) ...)
	(?collected-arg ...)
	(?rest . ?body)
	?case-lambda-clause ...)
     (%case-lambda/collect-classes-and-arguments
      #t
      (?collected-case-lambda-clause ... ((?collected-arg ... . ?rest)
					  (%add-assertions ((?collected-cls ...) ...)
							   (?collected-arg ...))
					  (with-fields ((?collected-arg ?collected-cls ...)
							...)
					    . ?body)))
      ()
      ()
      ?case-lambda-clause ...))

    ;;Matches when all the CASE-LAMBDA clauses have been collected.
    ((_ ?add-assertions (?collected-case-lambda-clause ...) () ())
     (case-lambda ?collected-case-lambda-clause ...))

    ))


(define-record-type <top>
  (nongenerative nausicaa:builtin:<top>))

(define-syntax <top>-with-record-fields-of
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
  (nongenerative nausicaa:builtin:<real>)
  (virtual-fields (immutable abs)))

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
     ((record-constructor (record-constructor-descriptor ?record-name)) ?arg ...))))


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
