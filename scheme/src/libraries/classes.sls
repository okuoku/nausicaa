;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: record types as classes
;;;Date: Thu Apr  1, 2010
;;;
;;;Abstract
;;;
;;;	This file is not licensed under the GPL because I want people to
;;;	freely take this code out of Nausicaa and try stuff.
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
    class-type-descriptor		class-constructor-descriptor
    define/with				define/with*
    lambda/with				lambda/with*
    case-lambda/with			case-lambda/with*
    let-fields				let*-fields
    letrec-fields			letrec*-fields
    with-fields
    is-a?
    record-type-parent?
    record-type-of
    record-parent-list			class-parent-list

    <top> <builtin>
    <pair> <list>
    <char> <string> <vector> <bytevector> <hashtable>
    <record> <condition>
    <port> <binary-port> <input-port> <output-port> <textual-port>
    <fixnum> <flonum> <integer> <integer-valued> <rational> <rational-valued>
    <real> <real-valued> <complex> <number>
    )
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
  (lambda (stx)
    (define (%constructor name)
      (string->symbol (string-append "make-" name)))
    (define (%predicate name)
      (string->symbol (string-append name "?")))
    (syntax-case stx (fields mutable immutable parent protocol sealed opaque parent-rtd nongenerative
			     virtual-fields methods method predicate)

      ((_ (?name ?constructor ?predicate) ?clause ...)
       (for-all identifier? (list (syntax ?name) (syntax ?constructor) (syntax ?predicate)))
       #'(%define-class/sort-clauses
	  (define-class (?name ?constructor ?predicate) ?clause ...)
	  (?name ?constructor ?predicate)
	  ()	;collected concrete fields
	  ()	;collected virtual fields
	  ()	;collected methods
	  ()	;collected functions
	  (predicate) (parent) (protocol) (sealed) (opaque) (parent-rtd) (nongenerative)
	  ?clause ...))

      ((_ ?name ?clause ...)
       (identifier? (syntax ?name))
       (let ((name (symbol->string (syntax->datum #'?name))))
	 (with-syntax ((CONSTRUCTOR  (datum->syntax #'?name (%constructor name)))
		       (PREDICATE    (datum->syntax #'?name (%predicate   name))))
	   #'(%define-class/sort-clauses
	      (define-class ?name ?clause ...)
	      (?name CONSTRUCTOR PREDICATE)
	      () ;collected concrete fields
	      () ;collected virtual fields
	      () ;collected methods
	      () ;collected functions
	      (predicate) (parent) (protocol) (sealed) (opaque) (parent-rtd) (nongenerative)
	      ?clause ...))))

      ((_ ?name-spec . ?clauses)
       (syntax-violation 'define-class
	 "invalid name specification in class definition"
	 (syntax->datum (syntax ?input-form))
	 (syntax->datum (syntax ?name-spec))))
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
  (lambda (stx)
    (define (%accessor name field)
      (string->symbol (string-append name "-" field)))
    (define (%mutator name field)
      (string->symbol (string-append name "-" field "-set!")))
    (define %function %accessor)
    (define (%sinner msg input-form subform)
      (syntax-violation 'define-class msg (syntax->datum input-form) (syntax->datum subform)))

    (syntax-case stx (fields mutable immutable parent protocol sealed opaque parent-rtd nongenerative
			     virtual-fields methods method predicate)

      ;;Gather the PARENT clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		. ?parent-rest)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(parent ?parent-name) ?clause ...)
       (let ((form	(syntax ?input-form))
	     (subform	(syntax (parent ?parent-name))))
	 (cond ((not (null? (syntax->datum (syntax ?parent-rest))))
		(%sinner "parent clause given twice in class definition" form subform))
	       ((not (identifier? (syntax ?parent-name)))
		(%sinner "invalid parent clause in class definition" form subform))
	       (else
		#'(%define-class/sort-clauses
		   ?input-form (?name ?constructor ?predicate)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-function ...)
		   (predicate		?pre ...)
		   (parent		?parent-name)
		   (protocol		?pro ...)
		   (sealed		?sea ...)
		   (opaque		?opa ...)
		   (parent-rtd		?pad ...)
		   (nongenerative	?non ...)
		   ?clause ...)))))

      ;;Gather the PREDICATE clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	. ?predicate-rest)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(predicate ?function-name) ?clause ...)
       (let ((form	(syntax ?input-form))
	     (subform	(syntax (predicate ?function-name))))
	 (cond ((not (null? (syntax->datum (syntax ?predicate-rest))))
		(%sinner "predicate clause given twice in class definition" form subform))
	       ((not (identifier? (syntax ?function-name)))
		(%sinner "invalid predicate clause in class definition" form subform))
	       (else
		#'(%define-class/sort-clauses
		   ?input-form (?name ?constructor ?predicate)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-function ...)
		   (predicate		?function-name)
		   (parent		?par ...)
		   (protocol		?pro ...)
		   (sealed		?sea ...)
		   (opaque		?opa ...)
		   (parent-rtd		?pad ...)
		   (nongenerative	?non ...)
		   ?clause ...)))))

      ;;Gather the PROTOCOL clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?par ...)
	(protocol	. ?protocol-rest)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(protocol ?protocol-proc) ?clause ...)
       (let ((form	(syntax ?input-form))
	     (subform	(syntax (protocol ?protocol-proc))))
	 (cond ((not (null? (syntax->datum (syntax ?protocol-rest))))
		(%sinner "protocol clause given twice in class definition" form subform))
	       (else
		#'(%define-class/sort-clauses
		   ?input-form (?name ?constructor ?predicate)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-function ...)
		   (predicate		?pre ...)
		   (parent		?par ...)
		   (protocol		?protocol-proc)
		   (sealed		?sea ...)
		   (opaque		?opa ...)
		   (parent-rtd		?pad ...)
		   (nongenerative	?non ...)
		   ?clause ...)))))

      ;;Gather the SEALED clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		. ?sealed-rest)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(sealed ?sealed) ?clause ...)
       (let ((form	(syntax ?input-form))
	     (subform	(syntax (sealed ?sealed))))
	 (cond ((not (null? (syntax->datum (syntax ?sealed-rest))))
		(%sinner "sealed clause given twice in class definition" form subform))
	       ((not (boolean? (syntax->datum #'?sealed)))
		(%sinner "invalid sealed clause in class definition" form subform))
	       (else
		#'(%define-class/sort-clauses
		   ?input-form (?name ?constructor ?predicate)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-function ...)
		   (predicate		?pre ...)
		   (parent		?par ...)
		   (protocol		?pro ...)
		   (sealed		?sealed)
		   (opaque		?opa ...)
		   (parent-rtd		?pad ...)
		   (nongenerative	?non ...)
		   ?clause ...)))))

      ;;Gather the OPAQUE clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		. ?opaque-rest)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(opaque ?opaque) ?clause ...)
       (let ((form	(syntax ?input-form))
	     (subform	(syntax (opaque ?opaque))))
	 (cond ((not (null? (syntax->datum (syntax ?opaque-rest))))
		(%sinner "opaque clause given twice in class definition" form subform))
	       ((not (boolean? (syntax->datum #'?opaque)))
		(%sinner "invalid opaque clause in class definition" form subform))
	       (else
		#'(%define-class/sort-clauses
		   ?input-form (?name ?constructor ?predicate)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-function ...)
		   (predicate		?pre ...)
		   (parent		?par ...)
		   (protocol		?pro ...)
		   (sealed		?sea ...)
		   (opaque		?opaque)
		   (parent-rtd		?pad ...)
		   (nongenerative	?non ...)
		   ?clause ...)))))

      ;;Gather the PARENT-RTD clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	. ?parent-rtd-rest)
	(nongenerative	?non ...)
	(parent-rtd ?parent-rtd ?parent-cd) ?clause ...)
       (let ((form	(syntax ?input-form))
	     (subform	(syntax (parent-rtd ?parent-rtd ?parent-cd))))
	 (cond ((not (null? (syntax->datum (syntax ?parent-rtd-rest))))
		(%sinner "parent-rtd clause given twice in class definition" form subform))
	       (else
		#'(%define-class/sort-clauses
		   ?input-form (?name ?constructor ?predicate)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-function ...)
		   (predicate		?pre ...)
		   (parent		?par ...)
		   (protocol		?pro ...)
		   (sealed		?sea ...)
		   (opaque		?opa ...)
		   (parent-rtd		?parent-rtd ?parent-cd)
		   (nongenerative	?non ...)
		   ?clause ...)))))

      ;;Gather the NONGENERATIVE empty clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	. ?nongenerative-rest)
	(nongenerative) ?clause ...)
       (let ((form	(syntax ?input-form))
	     (subform	(syntax (nongenerative))))
	 (cond ((not (null? (syntax->datum (syntax ?nongenerative-rest))))
		(%sinner "nongenerative clause given twice in class definition" form subform))
	       (else
		#`(%define-class/sort-clauses
		   ?input-form (?name ?constructor ?predicate)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-function ...)
		   (predicate		?pre ...)
		   (parent		?par ...)
		   (protocol		?pro ...)
		   (sealed		?sea ...)
		   (opaque		?opa ...)
		   (parent-rtd		?pad ...)
		   ;;We have  to do this  to properly generate  a unique
		   ;;UID.  We cannot rely on the expander renaming a raw
		   ;;symbol we may introduce here.
		   (nongenerative	#,@(generate-temporaries #'(?name)))
		   ?clause ...)))))

      ;;Gather the NONGENERATIVE non-empty clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	. ?nongenerative-rest)
	(nongenerative ?uid) ?clause ...)
       (let ((form	(syntax ?input-form))
	     (subform	(syntax (nongenerative ?uid))))
	 (cond ((not (null? (syntax->datum (syntax ?nongenerative-rest))))
		(%sinner "nongenerative clause given twice in class definition" form subform))
	       ((not (identifier? (syntax ?uid)))
		(%sinner "invalid nongenerative clause in class definition" form subform))
	       (else
		#'(%define-class/sort-clauses
		   ?input-form (?name ?constructor ?predicate)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-function ...)
		   (predicate		?pre ...)
		   (parent		?par ...)
		   (protocol		?pro ...)
		   (sealed		?sea ...)
		   (opaque		?opa ...)
		   (parent-rtd		?pad ...)
		   (nongenerative	?uid)
		   ?clause ...)))))

;;; --------------------------------------------------------------------

      ;;Gather  the  FIELDS clause.   Notice  that  FIELDS  can be  used
      ;;multiple times.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields ?field-clause ...) ?clause ...)
       #'(%define-class/sort-clauses/fields
	  ?input-form (?name ?constructor ?predicate)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (predicate		?pre ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (fields ?field-clause ...) ?clause ...))

;;; --------------------------------------------------------------------

      ;;Gather   the    VIRTUAL-FIELDS   clause.    Notice    that   the
      ;;VIRTUAL-FIELDS clause can be used multiple times.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields ?field-clause ...) ?clause ...)
       #'(%define-class/sort-clauses/virtual-fields
	  ?input-form (?name ?constructor ?predicate)
	  (?collected-concrete-field ... )
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (predicate		?pre ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (virtual-fields ?field-clause ...) ?clause ...))

;;; --------------------------------------------------------------------

      ;;Gather METHODS  clause.  Notice that  the METHODS clause  can be
      ;;used multiple times.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(methods ?method-clause ...) ?clause ...)
       #'(%define-class/sort-clauses/methods
	  ?input-form (?name ?constructor ?predicate)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (predicate		?pre ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (methods ?method-clause ...) ?clause ...))

;;; --------------------------------------------------------------------

      ;;Gather METHOD clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(method (?method . ?args) . ?body) ?clause ...)
       #'(%define-class/sort-clauses
	  ?input-form (?name ?constructor ?predicate)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ... (?method function-name))
	  (?collected-function ... (define/with (function-name . ?args) . ?body))
	  (predicate		?pre ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  ?clause ...))

;;; --------------------------------------------------------------------

      ;;No    more   clauses    to   gather.     Hand    everything   to
      ;;%DEFINE-CLASS/FIX-PARENT.
      ;;
      ((_ ?input-form (?name ?constructor ?predicate)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (predicate		?pre ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...))
       #'(%define-class/fix-parent
	  ?input-form (?name ?constructor ?predicate)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (predicate		?pre ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)))

      ((_ ?input-form . ?stuff)
       (syntax-violation 'define-class
	 "invalid class definition"
	 (syntax->datum (syntax ?input-form))))

      )))


(define-syntax %define-class/sort-clauses/fields
  (lambda (stx)
    (define (%accessor name field)
      (string->symbol (string-append name "-" field)))
    (define (%mutator name field)
      (string->symbol (string-append name "-" field "-set!")))
    (syntax-case stx (fields mutable immutable parent protocol sealed opaque parent-rtd nongenerative
			     virtual-fields methods method predicate)

      ;;Gather mutable FIELDS clause with explicit selection of accessor
      ;;and mutator names.
      ((%define-class/sort-clauses/fields
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields (mutable ?field ?field-accessor ?field-mutator) ?field-clause ...) ?clause ...)
       #'(%define-class/sort-clauses/fields
	  ?input-form (?name ?constructor ?predicate)
	  (?collected-concrete-field ... (mutable ?field ?field-accessor ?field-mutator))
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (predicate		?pre ...)
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
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
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
	      ?input-form (?name ?constructor ?predicate)
	      (?collected-concrete-field ... (mutable ?field ACCESSOR MUTATOR))
	      (?collected-virtual-field ...)
	      (?collected-method ...)
	      (?collected-function ...)
	      (predicate	?pre ...)
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
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields (immutable ?field ?accessor) ?field-clause ...) ?clause ...)
       #'(%define-class/sort-clauses/fields
	  ?input-form (?name ?constructor ?predicate)
	  (?collected-concrete-field ... (immutable ?field ?accessor))
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (predicate		?pre ...)
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
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
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
	      ?input-form (?name ?constructor ?predicate)
	      (?collected-concrete-field ... (immutable ?field ACCESSOR))
	      (?collected-virtual-field ...)
	      (?collected-method ...)
	      (?collected-function ...)
	      (predicate	?pre ...)
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
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
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
	      ?input-form (?name ?constructor ?predicate)
	      (?collected-concrete-field ... (immutable ?field ACCESSOR))
	      (?collected-virtual-field ...)
	      (?collected-method ...)
	      (?collected-function ...)
	      (predicate	?pre ...)
	      (parent		?par ...)
	      (protocol		?pro ...)
	      (sealed		?sea ...)
	      (opaque		?opa ...)
	      (parent-rtd	?pad ...)
	      (nongenerative	?non ...)
	      (fields ?field-clause ...) ?clause ...))))

      ;;Remove empty, leftover, FIELDS clause.
      ((%define-class/sort-clauses/fields
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields) ?clause ...)
       #'(%define-class/sort-clauses
	  ?input-form (?name ?constructor ?predicate)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (predicate		?pre ...)
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
			     virtual-fields methods method predicate)

      ;;Gather mutable VIRTUAL-FIELDS  clause with explicit selection of
      ;;accessor and mutator names.
      ((%define-class/sort-clauses/virtual-fields
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields (mutable ?field ?accessor ?mutator) ?field-clause ...) ?clause ...)
       #'(%define-class/sort-clauses/virtual-fields
	  ?input-form (?name ?constructor ?predicate)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...  (mutable ?field ?accessor ?mutator))
	  (?collected-method ...)
	  (?collected-function ...)
	  (predicate		?pre ...)
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
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
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
	      ?input-form (?name ?constructor ?predicate)
	      (?collected-concrete-field ...)
	      (?collected-virtual-field ...  (mutable ?field ACCESSOR MUTATOR))
	      (?collected-method ...)
	      (?collected-function ...)
	      (predicate	?pre ...)
	      (parent		?par ...)
	      (protocol		?pro ...)
	      (sealed		?sea ...)
	      (opaque		?opa ...)
	      (parent-rtd	?pad ...)
	      (nongenerative	?non ...)
	      (virtual-fields ?field-clause ...) ?clause ...))))

;;; --------------------------------------------------------------------

      ;;Gather immutable  VIRTUAL-FIELDS clause with  explicit selection
      ;;of accessor name.
      ((%define-class/sort-clauses/virtual-fields
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields (immutable ?field ?accessor) ?field-clause ...) ?clause ...)
       #'(%define-class/sort-clauses/virtual-fields
	  ?input-form (?name ?constructor ?predicate)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ... (immutable ?field ?accessor))
	  (?collected-method ...)
	  (?collected-function ...)
	  (predicate		?pre ...)
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
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
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
	      ?input-form (?name ?constructor ?predicate)
	      (?collected-concrete-field ...)
	      (?collected-virtual-field ... (immutable ?field ACCESSOR))
	      (?collected-method ...)
	      (?collected-function ...)
	      (predicate	?pre ...)
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
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
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
	      ?input-form (?name ?constructor ?predicate)
	      (?collected-concrete-field ...)
	      (?collected-virtual-field ... (immutable ?field ACCESSOR))
	      (?collected-method ...)
	      (?collected-function ...)
	      (predicate	?pre ...)
	      (parent		?par ...)
	      (protocol		?pro ...)
	      (sealed		?sea ...)
	      (opaque		?opa ...)
	      (parent-rtd	?pad ...)
	      (nongenerative	?non ...)
	      (virtual-fields ?field-clause ...) ?clause ...))))

;;; --------------------------------------------------------------------

      ;;Remove empty, leftover, VIRTUAL-FIELDS clause.
      ((%define-class/sort-clauses/virtual-fields
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields) ?clause ...)
       #'(%define-class/sort-clauses
	  ?input-form (?name ?constructor ?predicate)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (predicate		?pre ...)
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
			     virtual-fields methods method predicate)

      ;;Gather METHODS clause with explicit selection of function name.
      ((%define-class/sort-clauses/methods
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(methods (?method ?function) ?method-clause ...) ?clause ...)
       #'(%define-class/sort-clauses/methods
	  ?input-form (?name ?constructor ?predicate)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ... (?method ?function))
	  (?collected-function ...)
	  (predicate		?pre ...)
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
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
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
	      ?input-form (?name ?constructor ?predicate)
	      (?collected-concrete-field ...)
	      (?collected-virtual-field ...)
	      (?collected-method ... (?method FUNCTION))
	      (?collected-function ...)
	      (predicate	?pre ...)
	      (parent		?par ...)
	      (protocol		?pro ...)
	      (sealed		?sea ...)
	      (opaque		?opa ...)
	      (parent-rtd	?pad ...)
	      (nongenerative	?non ...)
	      (methods ?method-clause ...) ?clause ...))))

      ;;Gather METHODS clause declared with only the symbol.
      ((%define-class/sort-clauses/methods
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
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
	      ?input-form (?name ?constructor ?predicate)
	      (?collected-concrete-field ...)
	      (?collected-virtual-field ...)
	      (?collected-method ... (?method FUNCTION))
	      (?collected-function ...)
	      (predicate	?pre ...)
	      (parent		?par ...)
	      (protocol		?pro ...)
	      (sealed		?sea ...)
	      (opaque		?opa ...)
	      (parent-rtd	?pad ...)
	      (nongenerative	?non ...)
	      (methods ?method-clause ...) ?clause ...))))

      ;;Remove empty, leftover, METHODS clause.
      ((%define-class/sort-clauses/methods
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(methods) ?clause ...)
       #'(%define-class/sort-clauses
	  ?input-form (?name ?constructor ?predicate)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  (predicate		?pre ...)
	  (parent		?par ...)
	  (protocol		?pro ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  ?clause ...))

      )))


(define-syntax %define-class/fix-parent
  ;;Normalise the definition by processing or removing the PARENT clause
  ;;and validating  the PARENT-RTD  clause.  Finally hand  everything to
  ;;%DEFINE-CLASS/MAKE-FIELDS-VECTOR.
  ;;
  (syntax-rules (parent protocol sealed opaque parent-rtd nongenerative predicate)

    ;;If the class definition used both PARENT and PARENT-RTD, raise an error.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?parent0 ?parent ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?parent-rtd0 ?parent-rtd ...)
	(nongenerative	?non ...))
     (syntax-violation 'define-class
       "both parent and parent-rtd used in class definition"
       (syntax->datum #'?input-form)))

    ;;If the  class definition  used neither the  PARENT clause  nor the
    ;;PARENT-RTD clause, make the type derived by "<top>".
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent) ;no parent
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd) ;no parent-rtd
	(nongenerative	?non ...))
     (%define-class/make-fields-vector
      ?input-form (?name ?constructor ?predicate)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-function ...)
      (predicate	?pre ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	(record-type-descriptor <top>)
			(record-constructor-descriptor <top>))
      (nongenerative	?non ...)))

;;; --------------------------------------------------------------------
;;; process PARENT clause

    ;;If the class  definition used PARENT with more  than one argument,
    ;;raise an error.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?parent0 ?parent1 ?parent ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd) ;no parent-rtd
	(nongenerative	?non ...))
     (syntax-violation 'define-class
       "only one argument is needed in parent clause for class definition"
       (syntax->datum #'?input-form)))

    ;;If the  class definition used  the PARENT with a  single argument,
    ;;expand the definition using PARENT-RTD.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent		?parent-name)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd) ;no parent-rtd
	(nongenerative	?non ...))
     (%define-class/make-fields-vector
      ?input-form (?name ?constructor ?predicate)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-function ...)
      (predicate	?pre ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	(class-type-descriptor ?parent-name)
			(class-constructor-descriptor ?parent-name))
      (nongenerative	?non ...)))

;;; --------------------------------------------------------------------
;;; process PARENT-RTD clause

    ;;If  the class definition  used PARENT-RTD  with 2  arguments, pass
    ;;everything to the next step.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent) ;no parent
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?parent-rtd ?parent-cd)
	(nongenerative	?non ...))
     (%define-class/make-fields-vector
      ?input-form (?name ?constructor ?predicate)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-function ...)
      (predicate	?pre ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?parent-rtd ?parent-cd)
      (nongenerative	?non ...)))

    ;;If the class  definition used PARENT-RTD with more  or less than 2
    ;;arguments, raise an error.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(parent) ;no parent
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?parent-rtd ...)
	(nongenerative	?non ...))
     (syntax-violation 'define-class
       "wrong number of arguments in parent-rtd clause used in class definition"
       (syntax->datum #'?input-form)))

    ))


(define-syntax %define-class/make-fields-vector
  (syntax-rules (fields protocol sealed opaque parent-rtd nongenerative predicate)
    ((_ ?input-form (?name ?constructor ?predicate)
	((?keyword ?field ?accessor ...) ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	(predicate	?pre ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?parent-rtd ?parent-cd)
	(nongenerative	?non ...))
     (%define-class/normalise-predicate
      ?input-form (?name ?constructor ?predicate)
      ((?keyword ?field ?accessor ...) ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-function ...)
      #((?keyword ?field) ...)
      (predicate	?pre ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?parent-rtd ?parent-cd)
      (nongenerative	?non ...)))
    ))


(define-syntax %define-class/normalise-predicate
  (syntax-rules (fields protocol sealed opaque parent-rtd nongenerative)

    ;;No predicate was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	?fields-vector
	(predicate)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?parent-rtd ?parent-cd)
	(nongenerative	?non ...))
     (%define-class/normalise-protocol
      ?input-form (?name ?constructor ?predicate)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-function ...)
      ?fields-vector
      ?predicate
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?parent-rtd ?parent-cd)
      (nongenerative	?non ...)))

    ;;A predicate was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	?fields-vector
	(predicate	?predicate-function)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?parent-rtd ?parent-cd)
	(nongenerative	?non ...))
     (%define-class/normalise-protocol
      ?input-form (?name ?constructor ?predicate)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-function ...)
      ?fields-vector
      ?predicate-function
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?parent-rtd ?parent-cd)
      (nongenerative	?non ...)))

    ))


(define-syntax %define-class/normalise-protocol
  (syntax-rules (protocol sealed opaque parent-rtd nongenerative)

    ;;A PROTOCOL was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	?fields-vector
	?predicate-function
	(protocol	?protocol)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?parent-rtd ?parent-cd)
	(nongenerative	?non ...))
     (%define-class/normalise-sealed
      ?input-form (?name ?constructor ?predicate)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-function ...)
      ?fields-vector
      ?predicate-function
      ?protocol
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?parent-rtd ?parent-cd)
      (nongenerative	?non ...)))

    ;;No PROTOCOL was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	?fields-vector
	?predicate-function
	(protocol)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?parent-rtd ?parent-cd)
	(nongenerative	?non ...))
     (%define-class/normalise-sealed
      ?input-form (?name ?constructor ?predicate)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-function ...)
      ?fields-vector
      ?predicate-function
      #f
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?parent-rtd ?parent-cd)
      (nongenerative	?non ...)))

    ))


(define-syntax %define-class/normalise-sealed
  (syntax-rules (sealed opaque parent-rtd nongenerative)

    ;;A SEALED was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	?fields-vector
	?predicate-function
	?protocol
	(sealed		?sealed)
	(opaque		?opa ...)
	(parent-rtd	?parent-rtd ?parent-cd)
	(nongenerative	?non ...))
     (%define-class/normalise-opaque
      ?input-form (?name ?constructor ?predicate)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-function ...)
      ?fields-vector
      ?predicate-function
      ?protocol
      ?sealed
      (opaque		?opa ...)
      (parent-rtd	?parent-rtd ?parent-cd)
      (nongenerative	?non ...)))

    ;;No SEALED was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	?fields-vector
	?predicate-function
	?protocol
	(sealed)
	(opaque		?opa ...)
	(parent-rtd	?parent-rtd ?parent-cd)
	(nongenerative	?non ...))
     (%define-class/normalise-opaque
      ?input-form (?name ?constructor ?predicate)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-function ...)
      ?fields-vector
      ?predicate-function
      ?protocol
      #f
      (opaque		?opa ...)
      (parent-rtd	?parent-rtd ?parent-cd)
      (nongenerative	?non ...)))

    ))


(define-syntax %define-class/normalise-opaque
  (syntax-rules (opaque parent-rtd nongenerative)

    ;;An OPAQUE was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	?fields-vector
	?predicate-function
	?protocol
	?sealed
	(opaque		?opaque)
	(parent-rtd	?parent-rtd ?parent-cd)
	(nongenerative	?non ...))
     (%define-class/normalise-parent-rtd
      ?input-form (?name ?constructor ?predicate)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-function ...)
      ?fields-vector
      ?predicate-function
      ?protocol
      ?sealed
      ?opaque
      (parent-rtd	?parent-rtd ?parent-cd)
      (nongenerative	?non ...)))

    ;;No OPAQUE was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	?fields-vector
	?predicate-function
	?protocol
	?sealed
	(opaque)
	(parent-rtd	?parent-rtd ?parent-cd)
	(nongenerative	?non ...))
     (%define-class/normalise-parent-rtd
      ?input-form (?name ?constructor ?predicate)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-function ...)
      ?fields-vector
      ?predicate-function
      ?protocol
      ?sealed
      #f
      (parent-rtd	?parent-rtd ?parent-cd)
      (nongenerative	?non ...)))

    ))


(define-syntax %define-class/normalise-parent-rtd
  (syntax-rules (parent-rtd nongenerative)
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-function ...)
	?fields-vector
	?predicate-function
	?protocol
	?sealed
	?opaque
	(parent-rtd	?parent-rtd ?parent-cd)
	(nongenerative	?non ...))
     (%define-class/normalise-nongenerative
      ?input-form (?name ?constructor ?predicate)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-function ...)
      ?fields-vector
      ?predicate-function
      ?protocol
      ?sealed
      ?opaque
      ?parent-rtd
      ?parent-cd
      (nongenerative	?non ...)))))


(define-syntax %define-class/normalise-nongenerative
  (lambda (stx)
    (syntax-case stx (nongenerative)

      ;;A NONGENERATIVE was given.
      ((_ ?input-form (?name ?constructor ?predicate)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  ?fields-vector
	  ?predicate-function
	  ?protocol
	  ?sealed
	  ?opaque
	  ?parent-rtd
	  ?parent-cd
	  (nongenerative	?uid))
       #'(%define-class/output-forms
	  ?input-form (?name ?constructor ?predicate)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  ?fields-vector
	  ?predicate-function
	  ?protocol
	  ?sealed
	  ?opaque
	  ?parent-rtd
	  ?parent-cd
	  ?uid))

      ;;No NONGENERATIVE was given.
      ((_ ?input-form (?name ?constructor ?predicate)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  ?fields-vector
	  ?predicate-function
	  ?protocol
	  ?sealed
	  ?opaque
	  ?parent-rtd
	  ?parent-cd
	  (nongenerative))
       #`(%define-class/output-forms
	  ?input-form (?name ?constructor ?predicate)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-function ...)
	  ?fields-vector
	  ?predicate-function
	  ?protocol
	  ?sealed
	  ?opaque
	  ?parent-rtd
	  ?parent-cd
	  ;;We have to  do this to properly generated  a unique UID.  We
	  ;;cannot rely  on the  expander renaming a  raw symbol  we may
	  ;;introduce here.
	  #,@(generate-temporaries #'(?name))))

      )))


(define-syntax %define-class/output-forms
  (lambda (stx)

    (define (%make-name/rtd name)
      (string->symbol (string-append (symbol->string name) "-rtd")))

    (define (%make-name/cd name)
      (string->symbol (string-append (symbol->string name) "-cd")))

    (define (%make-name/with-fields name)
      (string->symbol (string-append (symbol->string name) "-with-class-fields-of")))

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

    (define generate-numbers
      (case-lambda

       ((context-stx list-of-syntaxes)
	(generate-numbers context-stx list-of-syntaxes 0 1))

       ((context-stx list-of-syntaxes start)
	(generate-numbers context-stx list-of-syntaxes start 1))

       ((context-stx list-of-syntaxes start step)
	(let ((count (length (syntax->datum list-of-syntaxes))))
	  (if (< count 0)
	      (assertion-violation 'generate-numbers
		"expected non-negative count argument" count)
	    (do ((count count (- count 1))
		 (val (+ start (* (- count 1) step)) (- val step))
		 (ret '() (cons val ret)))
		((<= count 0)
		 (datum->syntax context-stx ret))))))))

    (syntax-case stx ()

      ((_ ?input-form (?class-name ?constructor ?predicate)
	  ((?mutability ?field ?accessor ...) ...)
	  ((?virtual-mutability ?virtual-field ?virtual-accessor ...) ...)
	  ((?method ?method-function) ...)
	  (?collected-function ...)
	  ?fields-vector ?predicate-function
	  ?protocol ?sealed ?opaque ?parent-rtd ?parent-cd ?uid)
       (let ((id (duplicated-ids? #'(?field ... ?virtual-field ... ?method ...))))
	 (if id
	     (syntax-violation 'define-class
	       "duplicated field names in class definition"
	       (syntax->datum #'?input-form)
	       (syntax->datum id))
	   (let ((name (syntax->datum #'?class-name)))
	     (with-syntax
		 ((NAME-RTD	(datum->syntax #'?class-name (%make-name/rtd		name)))
		  (NAME-CD	(datum->syntax #'?class-name (%make-name/cd		name)))
		  (WITH-FIELDS	(datum->syntax #'?class-name (%make-name/with-fields	name)))
		  ((FIELD-INDEXES ...) (generate-numbers #'?class-name
							 #'((?mutability ?field ?accessor ...) ...))))
	       #'(begin
		   (define NAME-RTD
		     (make-record-type-descriptor (quote ?class-name)
						  ?parent-rtd (quote ?uid)
						  ?sealed ?opaque (quote ?fields-vector)))
		   (define NAME-CD
		     (make-record-constructor-descriptor NAME-RTD ?parent-cd ?protocol))

		   (define ?constructor		(record-constructor NAME-CD))
		   (define ?predicate		(record-predicate NAME-RTD))

		   (%define-class/output-forms/fields NAME-RTD (FIELD-INDEXES ...)
						      (?mutability ?field ?accessor ...) ...)

		   ?collected-function ...

		   (define-syntax ?class-name
		     (syntax-rules (class-type-descriptor
				    custom-predicate
				    default-constructor
				    default-constructor-descriptor
				    with-class-fields-of)
		       ((_ class-type-descriptor)
		   	(begin NAME-RTD))
		       ((_ default-constructor-descriptor)
		   	(begin NAME-CD))
		       ((_ default-constructor ?arg (... ...))
		   	(?constructor ?arg (... ...)))
		       ((_ custom-predicate ?arg (... ...))
		   	(?predicate-function ?arg (... ...)))
		       ((_ with-class-fields-of ?arg (... ...))
		   	(WITH-FIELDS ?arg (... ...)))
		       ))

		   (define-syntax WITH-FIELDS
		     (syntax-rules ()
		       ((_ ?variable-name ?body0 ?body (... ...))
		   	(%with-class-fields
		   	 ?variable-name
			 ((?mutability ?field ?accessor ...) ...
			  (?virtual-mutability ?virtual-field ?virtual-accessor ...) ...)
		   	 (%with-methods ?variable-name ((?method ?method-function) ...)
		   			?body0 ?body (... ...))
		   	 ))))
		   )))
	   )))
      )))


(define-syntax %define-class/output-forms/fields
  (syntax-rules ()

    ;;No fields.
    ((_ ?rtd ())
     (define dummy #f))

    ;;Process last field as mutable.
    ((_ ?rtd (?index) (mutable ?field ?accessor ?mutator))
     (begin
       (define ?accessor  (record-accessor ?rtd ?index))
       (define ?mutator   (record-mutator  ?rtd ?index))))

    ;;Process last field as immutable.
    ((_ ?rtd (?index) (immutable ?field ?accessor))
     (define ?accessor    (record-accessor ?rtd ?index)))

    ;;Process next field as mutable.
    ((_ ?rtd (?next-index ?index ...) (mutable ?field ?accessor ?mutator) ?clause ...)
     (begin
       (define ?accessor  (record-accessor ?rtd ?next-index))
       (define ?mutator   (record-mutator  ?rtd ?next-index))
       (%define-class/output-forms/fields ?rtd (?index ...) ?clause ...)))

    ;;Process next field as immutable.
    ((_ ?rtd (?next-index ?index ...) (immutable ?field ?accessor) ?clause ...)
     (begin
       (define ?accessor  (record-accessor ?rtd ?next-index))
       (%define-class/output-forms/fields ?rtd (?index ...) ?clause ...)))

    ))


(define-syntax %with-class-fields
  (lambda (stx)
    (define (%field name field)
      (string->symbol (string-append (symbol->string name) "." (symbol->string field))))
    (syntax-case stx ()

      ;;Process a field clause with both accessor and mutator.
      ((_ ?variable-name ((?mutability ?field ?accessor ?mutator) ?clause ...) ?body0 ?body ...)
       (with-syntax ((FIELD (datum->syntax #'?variable-name
					   (%field (syntax->datum #'?variable-name)
						   (syntax->datum #'?field)))))
	 #'(with-accessor-and-mutator ((FIELD ?variable-name ?accessor ?mutator))
				      (%with-class-fields ?variable-name
							  (?clause ...) ?body0 ?body ...))))

      ;;Process a field clause with accessor only.
      ((_ ?variable-name ((?mutability ?field ?accessor) ?clause ...) ?body0 ?body ...)
       (with-syntax ((FIELD (datum->syntax #'?variable-name
					   (%field (syntax->datum #'?variable-name)
						   (syntax->datum #'?field)))))
	 #'(with-accessor-and-mutator ((FIELD ?variable-name ?accessor))
				      (%with-class-fields ?variable-name
							  (?clause ...) ?body0 ?body ...))))

      ;;No more field clauses, output the body.
      ((_ ?variable-name () ?body0 ?body ...)
       #'(begin ?body0 ?body ...))
      )))

(define-syntax %with-methods
  (lambda (stx)
    (define (%field name field)
      (string->symbol (string-append (symbol->string name) "." (symbol->string field))))
    (syntax-case stx ()

      ((_ ?variable-name ((?field ?function-name) ?clause ...) ?body0 ?body ...)
       (with-syntax ((FIELD (datum->syntax #'?variable-name
					   (%field (syntax->datum #'?variable-name)
						   (syntax->datum #'?field)))))
	 #'(let-syntax ((FIELD (syntax-rules ()
				 ((_ ?arg (... ...))
				  (?function-name ?variable-name ?arg (... ...))))))
	     (%with-methods ?variable-name (?clause ...) ?body0 ?body ...))))

      ;;No more field clauses, output the body.
      ((_ ?variable-name () ?body0 ?body ...)
       #'(begin ?body0 ?body ...))
      )))


(define-syntax class-type-descriptor
  (lambda (stx)
    (syntax-case stx (class-type-descriptor)
      ((_ ?class-name)
       (free-identifier=? #'?class-name #'<top>)
       #'(record-type-descriptor ?class-name))

      ((_ ?class-name)
       #'(?class-name class-type-descriptor)))))

(define-syntax class-constructor-descriptor
  (lambda (stx)
    (syntax-case stx (default-constructor-descriptor)
      ((_ ?class-name)
       (free-identifier=? #'?class-name #'<top>)
       #'(record-constructor-descriptor ?class-name))
      ((_ ?class-name)
       #'(?class-name default-constructor-descriptor)))))


;;;; fields access syntaxes

(define-syntax with-fields
  ;;Just hand everything to  %WITH-FIELDS; this is because the recursive
  ;;%WITH-FIELDS needs  to allow  input forms which  we do not  want for
  ;;WITH-FIELDS.
  ;;
  ;;We  allow  an  empty list  of  clauses  because  it is  useful  when
  ;;expanding other macros into WITH-FIELDS uses.
  ;;
  (syntax-rules ()
    ((_ (?clause ...) ?body0 ?body ...)
     (%with-fields (?clause ...) ?body0 ?body ...))))

(define-syntax %with-fields
  (lambda (stx)
    (syntax-case stx ()

      ;;If the class is "<top>" skip it, because "<top>" has no fields.
      ((_ ((?var ?class0 ?class ...) ?clause ...) ?body0 ?body ...)
       (free-identifier=? #'?class0 #'<top>)
       #'(%with-fields ((?var ?class ...) ?clause ...) ?body0 ?body ...))

      ((_ ((?var ?class0 ?class ...) ?clause ...) ?body0 ?body ...)
       #'(?class0 with-class-fields-of ?var
		  (%with-fields ((?var ?class ...) ?clause ...) ?body0 ?body ...)))

      ((_ ((?var) ?clause ...) ?body0 ?body ...)
       #'(%with-fields (?clause ...) ?body0 ?body ...))

      ((_ () ?body0 ?body ...)
       #'(begin ?body0 ?body ...)))))

(define-syntax let-fields
  (syntax-rules ()
    ((_ (((?var ?class0 ?class ...) ?init) ...) ?body0 ?body ...)
     (let ((?var ?init) ...)
       (with-fields ((?var ?class0 ?class ...) ...) ?body0 ?body ...)))))

(define-syntax let*-fields
  (lambda (stx)
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

      ((let*-fields (((?var0 ?class0 ?class00 ...) ?init0)
		     ((?var1 ?class1 ?class11 ...) ?init1)
		     ...)
	 ?body0 ?body ...)
       (let ((id (duplicated-ids? #'(?var0 ?var1 ...))))
	 (if id
	     #`(syntax-violation 'let*-fields
		 "duplicated field names in let*-fields"
		 (quote (let*-fields (((?var0 ?class0 ?class00 ...) ?init0)
				      ((?var1 ?class1 ?class11 ...) ?init1)
				      ...)
			  ?body0 ?body ...))
		 (quote (#,id)))
	   #'(let ((?var0 ?init0))
	       (with-fields ((?var0 ?class0 ?class00 ...))
		 (let*-fields (((?var1 ?class1 ?class11 ...) ?init1) ...) ?body0 ?body ...))))))

      ((_ () ?body0 ?body ...)
       #'(begin ?body0 ?body ...)))))

(define-syntax letrec-fields
  (syntax-rules ()
    ((_ (((?var ?class0 ?class ...) ?init) ...) ?body0 ?body ...)
     (let ((?var #f) ...)
       (with-fields ((?var ?class0 ?class ...) ...)
	 (set! ?var ?init) ...
	 ?body0 ?body ...)))))

(define-syntax letrec*-fields
  ;;The  difference between  LETREC and  LETREC*  is only  the order  of
  ;;evaluation of ?INIT, which is enforced in LETREC*.
  ;;
  (syntax-rules ()
    ((_ (((?var ?class0 ?class ...) ?init) ...) ?body0 ?body ...)
     (let ((?var #f) ...)
       (with-fields ((?var ?class0 ?class ...) ...)
	 (set! ?var ?init) ...
	 ?body0 ?body ...)))))


(define-syntax define/with
  (syntax-rules ()
    ((_ (?variable . ?formals) . ?body)
     (define ?variable
       (lambda/with ?formals . ?body)))
    ((_ ?variable ?expression)
     (define ?variable ?expression))
    ((_ ?variable)
     (define ?variable))))

(define-syntax define/with*
  (syntax-rules ()
    ((_ (?variable . ?formals) . ?body)
     (define ?variable (lambda/with* ?formals . ?body)))
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
       (let ()
	 (with-fields ((?collected-arg ?collected-cls ...) ...) . ?body))))

    ;;Matches two cases: (1) when  all the arguments have been processed
    ;;and only  the rest argument is  there; (2) when the  formals is an
    ;;identifier (lambda args ---).
    ((_ #f ?rest ((?collected-cls ...) ...) (?collected-arg ...) . ?body)
     (lambda (?collected-arg ... . ?rest)
       (with-fields ((?collected-arg ?collected-cls ...) ...) . ?body)))
    ((_ #t ?rest ((?collected-cls ...) ...) (?collected-arg ...) . ?body)
     (lambda (?collected-arg ... . ?rest)
       (%add-assertions ((?collected-cls ...) ...) (?collected-arg ...))
       (let ()
	 (with-fields ((?collected-arg ?collected-cls ...) ...) . ?body))))
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
					  (let ()
					    (with-fields ((?collected-arg ?collected-cls ...)
							  ...)
					      . ?body))))
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
					  (let ()
					    (with-fields ((?collected-arg ?collected-cls ...)
							  ...)
					      . ?body))))
      ()
      ()
      ?case-lambda-clause ...))

    ;;Matches when all the CASE-LAMBDA clauses have been collected.
    ((_ ?add-assertions (?collected-case-lambda-clause ...) () ())
     (case-lambda ?collected-case-lambda-clause ...))

    ))


(define-record-type <top>
  (nongenerative nausicaa:builtin:<top>))

(define-class <builtin>
  (nongenerative nausicaa:builtin:<builtin>))

(define-syntax define-builtin-class
  (lambda (stx)
    (define (%uid name)
      (string->symbol (string-append "nausicaa:builtin:" (symbol->string name))))
    (syntax-case stx ()
      ((_ ?class-name ?clause ...)
       (with-syntax ((UID (datum->syntax #'?class-name (%uid (syntax->datum #'?class-name)))))
	 #'(define-class ?class-name
	     (parent <builtin>)
	     (nongenerative UID)
	     ?clause ...))))))

(define-builtin-class <pair>
  (predicate pair?)
  (virtual-fields (immutable car car)
		  (immutable cdr cdr)))

(define-builtin-class <list>
  (predicate list?)
  (virtual-fields (immutable car car)
		  (immutable cdr cdr)
		  (immutable length length)))

(define-builtin-class <char>
  (predicate char?)
  (virtual-fields (immutable upcase	char-upcase)
		  (immutable downcase	char-downcase)
		  (immutable titlecase	char-titlecase)
		  (immutable foldcase	char-foldcase)))

(define-builtin-class <string>
  (predicate string?)
  (virtual-fields (immutable length	string-length)
		  (immutable upcase	string-upcase)
		  (immutable downcase	string-downcase)
		  (immutable titlecase	string-titlecase)
		  (immutable foldcase	string-foldcase)))

(define-builtin-class <vector>
  (predicate vector?)
  (virtual-fields (immutable length vector-length)))

(define-builtin-class <bytevector>
  (predicate bytevector?)
  (virtual-fields (immutable length bytevector-length)))

(define-builtin-class <hashtable>
  (predicate hashtable?)
  (virtual-fields (immutable size hashtable-size)
		  (immutable keys hashtable-keys)
		  (immutable entries hashtable-entries)))

;;; --------------------------------------------------------------------

(define-builtin-class <record>
  (predicate record?))

(define-builtin-class <condition>
  (predicate condition?)
  (virtual-fields (immutable message	condition-message)
		  (immutable who	condition-who)
		  (immutable irritants	condition-irritants)))

;;; --------------------------------------------------------------------

(define-builtin-class <port>
  (predicate port?)
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
  (predicate input-port?)
  (nongenerative nausicaa:builtin:<input-port>))

(define-class <output-port>
  (parent <port>)
  (predicate output-port?)
  (nongenerative nausicaa:builtin:<output-port>))

(define-class <binary-port>
  (parent <port>)
  (predicate %binary-port?)
  (nongenerative nausicaa:builtin:<binary-port>))

;;This predicate is needed because in some implementations (Petite Chez)
;;BINARY-PORT? throws an exception if OBJ is not a port.
(define (%binary-port? obj)
  (and (port? obj) (binary-port? obj)))

(define-class <textual-port>
  (parent <port>)
  (predicate %textual-port?)
  (nongenerative nausicaa:builtin:<textual-port>))

;;This predicate is needed because in some implementations (Petite Chez)
;;TEXTUAL-PORT? throws an exception if OBJ is not a port.
(define (%textual-port? obj)
  (and (port? obj) (textual-port? obj)))

;;; --------------------------------------------------------------------

(define-builtin-class <number>
  (predicate number?)
  (virtual-fields (immutable exact	exact)
		  (immutable inexact	inexact)

		  (immutable exact?	exact?)
		  (immutable inexact?	inexact?)

		  (immutable zero?	zero?)

		  (immutable odd?	odd?)
		  (immutable even?	even?)

		  (immutable finite?	finite?)
		  (immutable infinite?	infinite?)
		  (immutable nan?	nan?)

		  (immutable numerator	numerator)
		  (immutable denominator denominator)

		  (immutable floor	floor)
		  (immutable ceiling	ceiling)
		  (immutable truncate	truncate)
		  (immutable round	round)))

(define-class <complex>
  (parent <number>)
  (predicate complex?)
  (virtual-fields (immutable real-part	real-part)
		  (immutable imag-part	imag-part)
		  (immutable magnitude	magnitude)
		  (immutable angle	angle))
  (nongenerative nausicaa:builtin:<complex>))

(define-class <real-valued>
  (parent <complex>)
  (predicate real-valued?)
  (virtual-fields (immutable positive?		positive?)
		  (immutable negative?		negative?)
		  (immutable non-positive?	non-positive?)
		  (immutable non-negative?	non-negative?))
  (nongenerative nausicaa:builtin:<real-valued>))

(define-class <real>
  (parent <real-valued>)
  (predicate real?)
  (nongenerative nausicaa:builtin:<real>)
  (virtual-fields (immutable abs)))

(define-class <rational-valued>
  (parent <real>)
  (predicate rational-valued?)
  (nongenerative nausicaa:builtin:<rational-valued>))

(define-class <flonum>
  (parent <real>)
  (predicate flonum?)
  (nongenerative nausicaa:builtin:<flonum>))

(define-class <rational>
  (parent <rational-valued>)
  (predicate rational?)
  (nongenerative nausicaa:builtin:<rational>))

(define-class <integer-valued>
  (parent <rational-valued>)
  (predicate integer-valued?)
  (nongenerative nausicaa:builtin:<integer-valued>))

(define-class <integer>
  (parent <integer-valued>)
  (predicate integer?)
  (nongenerative nausicaa:builtin:<integer>))

(define-class <fixnum>
  (parent <integer>)
  (predicate fixnum?)
  (nongenerative nausicaa:builtin:<fixnum>))


;;;; constructors

(define-syntax make
  (syntax-rules ()
    ((_ ?class-name ?arg ...)
     (?class-name default-constructor ?arg ...))))


;;;; predicates

(define (record-type-parent? rtd1 rtd2)
  (cond ((eq? (record-type-uid rtd1) (record-type-uid rtd2))	#t)
	((eq? (record-type-uid rtd1) (record-type-uid (record-type-descriptor <top>)))   #f)
   	((eq? (record-type-uid rtd2) (record-type-uid (record-type-descriptor <top>)))   #t)
	(else
	 (memq (record-type-uid rtd2) (map record-type-uid (record-parent-list rtd1))))))

(define-syntax is-a?
  (lambda (stx)
    (syntax-case stx ()

      ((_ ?obj ?class-name)
       (free-identifier=? #'?class-name #'<top>)
       (syntax (begin #t)))

      ((_ ?obj ?class-name)
       #'(?class-name custom-predicate ?obj))
      )))


;;;; inspection

(define-syntax class-parent-list
  (lambda (stx)
    (syntax-case stx ()

      ((_ ?record-name)
       (free-identifier=? #'?record-name #'<top>)
       (syntax (quote ())))

      ((_ ?record-name)
       #'(record-parent-list (class-type-descriptor ?record-name))))))

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
   ((hashtable?	obj)		(class-type-descriptor <hashtable>))

   ((record? obj)
    (record-rtd obj))

   ((number? obj)
    ;;Order does matter here!!!
    (cond ((fixnum?		obj)	(class-type-descriptor <fixnum>))
	  ((integer?		obj)	(class-type-descriptor <integer>))
	  ((rational?		obj)	(class-type-descriptor <rational>))
	  ((integer-valued?	obj)	(class-type-descriptor <integer-valued>))
	  ((rational-valued?	obj)	(class-type-descriptor <rational-valued>))
	  ((flonum?		obj)	(class-type-descriptor <flonum>))
	  ((real?		obj)	(class-type-descriptor <real>))
	  ((real-valued?	obj)	(class-type-descriptor <real-valued>))
	  ((complex?		obj)	(class-type-descriptor <complex>))
	  (else				(class-type-descriptor <number>))))
   ((char?		obj)		(class-type-descriptor <char>))
   ((string?		obj)		(class-type-descriptor <string>))
   ((vector?		obj)		(class-type-descriptor <vector>))
   ((bytevector?	obj)		(class-type-descriptor <bytevector>))
   ((port?		obj)
    ;;Order here is arbitrary.
    (cond ((input-port?		obj)	(class-type-descriptor <input-port>))
	  ((output-port?	obj)	(class-type-descriptor <output-port>))
	  ((binary-port?	obj)	(class-type-descriptor <binary-port>))
	  ((textual-port?	obj)	(class-type-descriptor <textual-port>))
	  (else				(class-type-descriptor <port>))))
   ((condition?		obj)		(class-type-descriptor <condition>))
   ((record?		obj)		(class-type-descriptor <record>))
   ((pair?		obj)
    ;;Order does matter  here!!!  Better leave these at  the end because
    ;;qualifying a long list can be time-consuming.
    (cond ((list?	obj)	(class-type-descriptor <list>))
	  (else			(class-type-descriptor <pair>))))
   (else (record-type-descriptor <top>))))


;;;; done

)

;;; end of file
