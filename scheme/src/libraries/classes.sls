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
;;;	Aaron Hsu  contributed the SYNTAX->LIST function  through a post
;;;	on comp.lang.scheme.
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
    class-record-descriptor		class-constructor-descriptor
    define/with-class			define/with-class*
    lambda/with-class			lambda/with-class*
    case-lambda/with-class		case-lambda/with-class*
    receive/with-class
    let/with-class			let*/with-class
    letrec/with-class			letrec*/with-class
    setf				getf
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
	  with-accessor-and-mutator)
    (rnrs mutable-strings))


;;;; helpers

(define-syntax make-list
  (syntax-rules ()
    ((_ ?len ?fill)
     (let ((len ?len))
       (do ((i 0 (+ 1 i))
	    (result '() (cons ?fill result)))
	   ((= i ?len)
	    result))))))


(define-record-type class-type-descriptor
  (fields (immutable rtd)
		;The record's RTD.
	  (immutable virtual-fields)
		;A vector of  virtual fields, in the same  format of the
		;one for concrete fields; empty vector if this class has
		;no virtual fields.
	  (immutable predicate)
		;A  symbol representing  the  class' predicate.   Always
		;present because it defaults to the record predicate.
	  (immutable setter)
		;A  symbol representing  the  class' setter  identifier;
		;false if  no setter was selected.  It  does not include
		;the implementation,  because a  method can be  either a
		;closure or a syntax.
	  (immutable getter)
		;A  symbol representing  the  class' getter  identifier;
		;false if  no getter was selected.  It  does not include
		;the implementation,  because a  getter can be  either a
		;closure or a syntax.
	  (immutable methods)
		;A  vector  of   symbols  listing  the  defined  methods
		;identifiers.  It  does not include  the implementation,
		;because a method can be either a closure or a syntax.
	  ))


(define-syntax define-virtual-class
  ;;A virtual class  is just a tag  we slap on any value  to use virtual
  ;;fields and methods with dot notation.
  ;;
  (syntax-rules ()
    ((_ ?name ?clause ...)
     (%define-virtual-class (define-class ?name ?clause ...) ?name () ?clause ...))))

(define-syntax %define-virtual-class
  ;;Raise an  error if the PROTOCOL  or PARENT clause is  present in the
  ;;body of the definition.  Finally define the class with DEFINE-CLASS.
  ;;
  (lambda (stx)
    (syntax-case stx (fields protocol)

      ;;no more clauses to collect
      ((_ ?input-form ?name (?collected-clause ...))
       #'(define-class ?name
	   (protocol (lambda (make-parent)
		       (lambda args
			 (syntax-violation #f "attempt to instantiate virtual class" (quote ?name)))))
	   ?collected-clause ...))

      ;;found PROTOCOL clause
      ((_ ?input-form ?name (?collected-clause ...) (protocol ?pro ...) ?clause ...)
       (syntax-violation 'define-class
	 "protocol clause used in definition of virtual class"
	 (syntax->datum #'?input-form)
	 (syntax->datum #'(protocol ?pro ...))))

      ;;found FIELDS clause
      ((_ ?input-form ?name (?collected-clause ...) (fields ?fie ...) ?clause ...)
       (syntax-violation 'define-class
	 "fields clause used in definition of virtual class"
	 (syntax->datum #'?input-form)
	 (syntax->datum #'(fields ?fie ...))))

      ;;other clauses
      ((_ ?input-form ?name (?collected-clause ...) ?clause0 ?clause ...)
       #'(%define-virtual-class ?input-form ?name (?collected-clause ... ?clause0) ?clause ...))

      )))


(define-syntax define-class
  (lambda (stx)
    (define (%constructor name)
      (string->symbol (string-append "make-" name)))
    (define (%predicate name)
      (string->symbol (string-append name "?")))
    (define (syntax->list x)
      (syntax-case x ()
	(()		'())
	((h . t)	(cons (syntax->list #'h) (syntax->list #'t)))
	(term		#'term)))

    (syntax-case stx (fields mutable immutable parent protocol sealed opaque parent-rtd nongenerative
			     virtual-fields methods method predicate setter getter)

      ((_ (?name ?constructor ?predicate) ?clause ...)
;;;*FIXME*  This  is a  workaround  for a  bug  in  Ikarus 1870:  Ikarus
;;;produces an  IMproper list  of syntax objects,  rather than  a proper
;;;list.
;;;
;;;(for-all identifier? #'(?name ?constructor ?predicate))
       (for-all identifier? (syntax->list #'(?name ?constructor ?predicate)))
       #'(%define-class/sort-clauses
	  (define-class (?name ?constructor ?predicate) ?clause ...)
	  (?name ?constructor ?predicate)
	  ()	;collected concrete fields
	  ()	;collected virtual fields
	  ()	;collected methods
	  ()	;collected functions
	  (predicate) (setter) (getter)
	  (parent) (protocol) (sealed) (opaque) (parent-rtd) (nongenerative)
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
	      (predicate) (setter) (getter)
	      (parent) (protocol) (sealed) (opaque) (parent-rtd) (nongenerative)
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
			     virtual-fields methods method predicate setter getter)

      ;;Gather the PARENT clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
		   (?collected-definition ...)
		   (predicate		?pre ...)
		   (setter		?set ...)
		   (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	. ?predicate-rest)
	(setter		?set ...)
	(getter		?get ...)
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
		   (?collected-definition ...)
		   (predicate		?function-name)
		   (setter		?set ...)
		   (getter		?get ...)
		   (parent		?par ...)
		   (protocol		?pro ...)
		   (sealed		?sea ...)
		   (opaque		?opa ...)
		   (parent-rtd		?pad ...)
		   (nongenerative	?non ...)
		   ?clause ...)))))

      ;;Gather the SETTER clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		. ?setter-rest)
	(getter		?get ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(setter ?setter) ?clause ...)
       (let ((form	(syntax ?input-form))
	     (subform	(syntax (setter ?setter))))
	 (cond ((not (null? (syntax->datum (syntax ?setter-rest))))
		(%sinner "setter clause given twice in class definition" form subform))
	       ((not (identifier? (syntax ?setter)))
		(%sinner "invalid setter clause in class definition" form subform))
	       (else
		#'(%define-class/sort-clauses
		   ?input-form (?name ?constructor ?predicate)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-definition ...)
		   (predicate		?pre ...)
		   (setter		?setter)
		   (getter		?get ...)
		   (parent		?par ...)
		   (protocol		?pro ...)
		   (sealed		?sea ...)
		   (opaque		?opa ...)
		   (parent-rtd		?pad ...)
		   (nongenerative	?non ...)
		   ?clause ...)))))

      ;;Gather the GETTER clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		. ?getter-rest)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(getter ?getter) ?clause ...)
       (let ((form	(syntax ?input-form))
	     (subform	(syntax (getter ?getter))))
	 (cond ((not (null? (syntax->datum (syntax ?getter-rest))))
		(%sinner "getter clause given twice in class definition" form subform))
	       ((not (identifier? (syntax ?getter)))
		(%sinner "invalid getter clause in class definition" form subform))
	       (else
		#'(%define-class/sort-clauses
		   ?input-form (?name ?constructor ?predicate)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-definition ...)
		   (predicate		?pre ...)
		   (setter		?set ...)
		   (getter		?getter)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
		   (?collected-definition ...)
		   (predicate		?pre ...)
		   (setter		?set ...)
		   (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
		   (?collected-definition ...)
		   (predicate		?pre ...)
		   (setter		?set ...)
		   (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
		   (?collected-definition ...)
		   (predicate		?pre ...)
		   (setter		?set ...)
		   (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
		   (?collected-definition ...)
		   (predicate		?pre ...)
		   (setter		?set ...)
		   (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
		   (?collected-definition ...)
		   (predicate		?pre ...)
		   (setter		?set ...)
		   (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
		   (?collected-definition ...)
		   (predicate		?pre ...)
		   (setter		?set ...)
		   (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	  (?collected-definition ... (define/with-class (function-name . ?args) . ?body))
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
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
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
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
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
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
			     virtual-fields methods method predicate setter getter)

      ;;Gather mutable FIELDS clause with explicit selection of accessor
      ;;and mutator names.
      ((%define-class/sort-clauses/fields
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	      (?collected-definition ...)
	      (predicate	?pre ...)
	      (setter		?set ...)
	      (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	      (?collected-definition ...)
	      (predicate	?pre ...)
	      (setter		?set ...)
	      (getter		?get ...)
	      (parent		?par ...)
	      (protocol		?pro ...)
	      (sealed		?sea ...)
	      (opaque		?opa ...)
	      (parent-rtd	?pad ...)
	      (nongenerative	?non ...)
	      (fields ?field-clause ...) ?clause ...))))

      ;;Gather   immutable  FIELDS   clause  declared   without  IMMUTABLE
      ;;auxiliary syntax.
      ((%define-class/sort-clauses/fields
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	      (?collected-definition ...)
	      (predicate	?pre ...)
	      (setter		?set ...)
	      (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
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
			     virtual-fields methods method predicate setter getter)

      ;;Gather mutable VIRTUAL-FIELDS  clause with explicit selection of
      ;;accessor and mutator names.
      ((%define-class/sort-clauses/virtual-fields
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	      (?collected-definition ...)
	      (predicate	?pre ...)
	      (setter		?set ...)
	      (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	      (?collected-definition ...)
	      (predicate	?pre ...)
	      (setter		?set ...)
	      (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	      (?collected-definition ...)
	      (predicate	?pre ...)
	      (setter		?set ...)
	      (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
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
			     virtual-fields methods method predicate setter getter)

      ;;Gather METHODS clause with explicit selection of function name.
      ((%define-class/sort-clauses/methods
	?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	      (?collected-definition ...)
	      (predicate	?pre ...)
	      (setter		?set ...)
	      (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	      (?collected-definition ...)
	      (predicate	?pre ...)
	      (setter		?set ...)
	      (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
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
  ;;%DEFINE-CLASS/NORMALISE-PREDICATE.
  ;;
  (syntax-rules (parent protocol sealed opaque parent-rtd nongenerative predicate setter getter)

    ;;If the class definition used both PARENT and PARENT-RTD, raise an error.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(parent) ;no parent
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd) ;no parent-rtd
	(nongenerative	?non ...))
     (%define-class/normalise-predicate
      ?input-form (?name ?constructor ?predicate)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      (predicate	?pre ...)
      (setter		?set ...)
      (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(parent		?parent-name)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd) ;no parent-rtd
	(nongenerative	?non ...))
     (%define-class/normalise-predicate
      ?input-form (?name ?constructor ?predicate)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      (predicate	?pre ...)
      (setter		?set ...)
      (getter		?get ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	(class-record-descriptor ?parent-name)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(parent) ;no parent
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?parent-rtd ?parent-cd)
	(nongenerative	?non ...))
     (%define-class/normalise-predicate
      ?input-form (?name ?constructor ?predicate)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      (predicate	?pre ...)
      (setter		?set ...)
      (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
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


(define-syntax %define-class/normalise-predicate
  (syntax-rules (fields protocol sealed opaque parent-rtd nongenerative predicate setter getter)

    ;;No predicate was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate)
	(setter		?set ...)
	(getter		?get ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?parent-rtd ?parent-cd)
	(nongenerative	?non ...))
     (%define-class/normalise-setter
      ?input-form (?name ?constructor ?predicate)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      ?predicate
      (setter		?set ...)
      (getter		?get ...)
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
	(?collected-definition ...)
	(predicate	?predicate-function)
	(setter		?set ...)
	(getter		?get ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?parent-rtd ?parent-cd)
	(nongenerative	?non ...))
     (%define-class/normalise-setter
      ?input-form (?name ?constructor ?predicate)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      ?predicate-function
      (setter		?set ...)
      (getter		?get ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?parent-rtd ?parent-cd)
      (nongenerative	?non ...)))
    ))


(define-syntax %define-class/normalise-setter
  (syntax-rules (fields protocol sealed opaque parent-rtd nongenerative setter getter)

    ;;No setter was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	?predicate-function
	(setter)
	(getter		?get ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?parent-rtd ?parent-cd)
	(nongenerative	?non ...))
     (%define-class/normalise-getter
      ?input-form (?name ?constructor ?predicate)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      ?predicate-function
      #f
      (getter		?get ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?parent-rtd ?parent-cd)
      (nongenerative	?non ...)))

    ;;A setter was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	?predicate-function
	(setter		?setter)
	(getter		?get ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?parent-rtd ?parent-cd)
	(nongenerative	?non ...))
     (%define-class/normalise-getter
      ?input-form (?name ?constructor ?predicate)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      ?predicate-function
      ?setter
      (getter		?get ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?parent-rtd ?parent-cd)
      (nongenerative	?non ...)))
    ))


(define-syntax %define-class/normalise-getter
  (syntax-rules (fields protocol sealed opaque parent-rtd nongenerative getter)

    ;;No getter was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	?predicate-function
	?setter
	(getter)
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
      (?collected-definition ...)
      ?predicate-function
      ?setter
      #f
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?parent-rtd ?parent-cd)
      (nongenerative	?non ...)))

    ;;A getter was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	?predicate-function
	?setter
	(getter		?getter)
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
      (?collected-definition ...)
      ?predicate-function
      ?setter
      ?getter
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
	(?collected-definition ...)
	?predicate-function
	?setter
	?getter
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
      (?collected-definition ...)
      ?predicate-function
      ?setter
      ?getter
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
	(?collected-definition ...)
	?predicate-function
	?setter
	?getter
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
      (?collected-definition ...)
      ?predicate-function
      ?setter
      ?getter
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
	(?collected-definition ...)
	?predicate-function
	?setter
	?getter
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
      (?collected-definition ...)
      ?predicate-function
      ?setter
      ?getter
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
	(?collected-definition ...)
	?predicate-function
	?setter
	?getter
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
      (?collected-definition ...)
      ?predicate-function
      ?setter
      ?getter
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
	(?collected-definition ...)
	?predicate-function
	?setter
	?getter
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
      (?collected-definition ...)
      ?predicate-function
      ?setter
      ?getter
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
	(?collected-definition ...)
	?predicate-function
	?setter
	?getter
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
      (?collected-definition ...)
      ?predicate-function
      ?setter
      ?getter
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
	(?collected-definition ...)
	?predicate-function
	?setter
	?getter
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
      (?collected-definition ...)
      ?predicate-function
      ?setter
      ?getter
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
	  (?collected-definition ...)
	  ?predicate-function
	  ?setter
	  ?getter
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
	  (?collected-definition ...)
	  ?predicate-function
	  ?setter
	  ?getter
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
	  (?collected-definition ...)
	  ?predicate-function
	  ?setter
	  ?getter
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
	  (?collected-definition ...)
	  ?predicate-function
	  ?setter
	  ?getter
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

    (define (%make-name/rtd name)	;name of record type descriptor
      (string->symbol (string-append (symbol->string name) "-rtd")))

    (define (%make-name/ctd name)	;name of class type descriptor
      (string->symbol (string-append (symbol->string name) "-ctd")))

    (define (%make-name/cd name)	;name of constructor descriptor
      (string->symbol (string-append (symbol->string name) "-cd")))

    (define (%make-name/with-fields name)	;name of class specific with-fields syntax
      (string->symbol (string-append (symbol->string name) "-with-class-fields-of")))

    (define (duplicated-ids? ell)
      ;;Search the list of  identifier syntax objects ELL for duplicated
      ;;identifiers;  return   false  of  a  syntax   object  holding  a
      ;;duplicated identifier.
      ;;
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
      ;;Derived  from   IOTA  from  (lists);  generate   a  sequence  of
      ;;non-negative exact integers to be  used as indexes in the vector
      ;;of concrete fields.
      ;;
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
	  (?collected-definition ...)
	  ?predicate-function ?setter ?getter
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
		  (NAME-CTD	(datum->syntax #'?class-name (%make-name/ctd		name)))
		  (NAME-CD	(datum->syntax #'?class-name (%make-name/cd		name)))
		  (WITH-CLASS-BINDINGS	(datum->syntax #'?class-name (%make-name/with-fields	name)))
		  ((FIELD-INDEXES ...) (generate-numbers #'?class-name
							 #'((?mutability ?field ?accessor ...) ...))))
	       #'(begin
		   (define NAME-RTD
		     (make-record-type-descriptor (quote ?class-name)
						  ?parent-rtd (quote ?uid)
						  ?sealed ?opaque
						  (quote #((?mutability ?field) ...))))

		   (define NAME-CTD
		     (make-class-type-descriptor
		      NAME-RTD
		      '#((?virtual-mutability ?virtual-field ?virtual-accessor ...) ...)
		      (quote ?predicate-function)
		      (quote ?setter) (quote ?getter)
		      '#((?method ?method-function) ...)))

		   (define NAME-CD
		     (make-record-constructor-descriptor NAME-RTD ?parent-cd ?protocol))

		   (define ?constructor		(record-constructor NAME-CD))
		   (define ?predicate		(record-predicate NAME-RTD))

		   (%define-class/output-forms/fields NAME-RTD (FIELD-INDEXES ...)
						      (?mutability ?field ?accessor ...) ...)

		   ;;These are the definitions of in-definition methods.
		   ?collected-definition ...

		   (define-syntax ?class-name
		     (lambda (stx)
		       (syntax-case stx (class-record-descriptor
					 custom-predicate
					 default-constructor
					 default-constructor-descriptor
					 with-class-fields-of
					 setter)

			 ((_ class-record-descriptor)
			  #'(begin NAME-RTD))

			 ((_ default-constructor-descriptor)
			  #'(begin NAME-CD))

			 ((_ default-constructor ?arg (... ...))
			  #'(?constructor ?arg (... ...)))

			 ((_ custom-predicate ?arg (... ...))
			  #'(?predicate-function ?arg (... ...)))

			 ((_ with-class-fields-of ?arg (... ...))
			  #'(WITH-CLASS-BINDINGS ?arg (... ...)))

			 ((_ ?keyword . ?rest)
			  (syntax-violation '?class-name
			    "invalid class internal keyword"
			    (syntax->datum #'(?class-name ?keyword . ?rest))
			    (syntax->datum #'?keyword)))
			 )))

		   (define-syntax WITH-CLASS-BINDINGS
		     (syntax-rules ()
		       ((_ ?variable-name ?body0 ?body (... ...))
		   	(%with-class-fields
		   	 ?variable-name
			 ((?mutability ?field ?accessor ...) ...
			  (?virtual-mutability ?virtual-field ?virtual-accessor ...) ...)
		   	 (%with-methods ?variable-name ((?method ?method-function) ...)
					(%with-setter-and-getter ?variable-name
								 ?setter ?getter
								 ?body0 ?body (... ...)))
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


(define-syntax %with-setter-and-getter
  ;;Wrap the body with the  LET-SYNTAX defining the setter binding; then
  ;;hand the  body to  %WITH-GETTER.  If there  is no setter:  leave the
  ;;identifier undefined, so that it does not shadow enclosing bindings.
  ;;
  (lambda (stx)
    (define (%setf name)
      (string->symbol (string-append (symbol->string name)
				     ".__nausicaa_private_setter_identifier_syntax")))
    (syntax-case stx ()
      ((_ ?variable-name ?setter ?getter . ?body)
       (identifier? #'?setter)
       (with-syntax ((SETF (datum->syntax #'?variable-name (%setf (syntax->datum #'?variable-name)))))
	 #'(let-syntax ((SETF (syntax-rules ()
				((_ ?key0 ?key (... ...) ?value)
				 (?setter ?variable-name ?key0 ?key (... ...) ?value)))))
	     (%with-getter ?variable-name ?getter . ?body))))

      ((_ ?variable-name ?setter ?getter . ?body)
       #'(%with-getter ?variable-name ?getter . ?body))
      )))

(define-syntax %with-getter
  ;;Wrap the body with the  LET-SYNTAX defining the getter binding; then
  ;;expand  the body.   If  there  is no  getter:  leave the  identifier
  ;;undefined, so that it does not shadow enclosing bindings.
  ;;
  (lambda (stx)
    (define (%getf name)
      (string->symbol (string-append name ".__nausicaa_private_getter_identifier_syntax")))
    (syntax-case stx ()
      ((_ ?variable-name ?getter . ?body)
       (identifier? #'?getter)
       (let ((name (symbol->string (syntax->datum #'?variable-name))))
	 (with-syntax ((GETF (datum->syntax #'?variable-name (%getf name))))
	   #'(let-syntax ((GETF (syntax-rules ()
				  ((_ ?key0 ?key (... ...))
				   (?getter ?variable-name ?key0 ?key (... ...))))))
	       . ?body))))
      ((_ ?variable-name ?getter . ?body)
       #'(begin . ?body))
      )))

(define-syntax setf
  (lambda (stx)
    (define (%setf name)
      (string->symbol (string-append (symbol->string name)
				     ".__nausicaa_private_setter_identifier_syntax")))
    (syntax-case stx (setter setter-multi-key set!)
      ((_ (?variable-name ?key0 ?key ...) ?value)
       (with-syntax ((SETF (datum->syntax #'?variable-name (%setf (syntax->datum #'?variable-name)))))
      	 #'(SETF ?key0 ?key ... ?value)))
       ((_ ?variable-name ?value)
	#'(set! ?variable-name ?value))
       )))

(define-syntax getf
  (lambda (stx)
    (define (%getf name)
      (string->symbol (string-append (symbol->string name)
				     ".__nausicaa_private_getter_identifier_syntax")))
    (syntax-case stx (setter setter-multi-key set!)
      ((_ (?variable-name ?key0 ?key ...))
       (with-syntax ((GETF (datum->syntax #'?variable-name (%getf (syntax->datum #'?variable-name)))))
      	 #'(GETF ?key0 ?key ...))))))


(define-syntax class-record-descriptor
  (lambda (stx)
    (syntax-case stx (class-record-descriptor)
      ((_ ?class-name)
       (free-identifier=? #'?class-name #'<top>)
       #'(record-type-descriptor ?class-name))

      ((_ ?class-name)
       #'(?class-name class-record-descriptor)))))

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


;;;; LET processing

(define no-loop #f)

(define-syntax %do-let/no-types
  ;;Process a LET form in which  all the bindings are untyped.  Hand the
  ;;rest to %DO-LET/ADD-TOP.
  ;;
  (lambda (stx)
    (define (syntax->list x)
      (syntax-case x ()
	(()		'())
	((h . t)	(cons (syntax->list #'h) (syntax->list #'t)))
	(term		#'term)))
    (syntax-case stx ()

      ;;No bindings.  Expand to ?LET  to allow <definition> forms in the
      ;;body.
      ((_ ?let ?let/with-class ?loop () ?body0 ?body ...)
       (if (free-identifier=? #'no-loop #'?loop)
	   #'(?let () ?body0 ?body ...)
	 #'(?let ?loop () ?body0 ?body ...)))

      ;;All bindings are without types.
      ((_ ?let ?let/with-class ?loop ((?var ?init) ...) ?body0 ?body ...)
;;;*FIXME*  This  is a  workaround  for a  bug  in  Ikarus 1870:  Ikarus
;;;produces an  IMproper list  of syntax objects,  rather than  a proper
;;;list.
;;;
;;;(for-all identifier? #'(?var ...))
       (for-all identifier? (syntax->list #'(?var ...)))
       (if (free-identifier=? #'no-loop #'?loop)
	   #'(?let ((?var ?init) ...) ?body0 ?body ...)
	 #'(?let ?loop ((?var ?init) ...) ?body0 ?body ...)))

      ;;At list one binding has types.
      ((_ ?let ?let/with-class ?loop ((?var ?init) ...) ?body0 ?body ...)
       #'(%do-let/add-top ?let ?let/with-class ?loop () ((?var ?init) ...) ?body0 ?body ...))
      )))

(define-syntax %do-let/add-top
  ;;Add <top>  to the  bindings with no  type.  Then hand  everything to
  ;;?LET-FIELD.
  ;;
  (lambda (stx)
    (syntax-case stx ()

    ;;No more bindings to collect.
    ((_ ?let ?let/with-class ?loop (?bind ...) () ?body0 ?body ...)
     (if (free-identifier=? #'let #'?let)
	 #'(?let/with-class ?loop (?bind ...) ?body0 ?body ...)
       #'(?let/with-class (?bind ...) ?body0 ?body ...)))

    ;;Add <top> as type to an untyped binding.
    ((_ ?let ?let/with-class ?loop (?bind ...) ((?var0 ?init0) (?var ?init) ...) ?body0 ?body ...)
     (identifier? #'?var0)
     #'(%do-let/add-top ?let ?let/with-class ?loop
		       (?bind ... ((?var0 <top>) ?init0))
		       ((?var ?init) ...)
		       ?body0 ?body ...))

    ;;Collect a typed binding.
    ((_ ?let ?let/with-class ?loop (?bind ...) ((?var0 ?init0) (?var ?init) ...) ?body0 ?body ...)
     #'(%do-let/add-top ?let ?let/with-class ?loop
		       (?bind ... (?var0 ?init0))
		       ((?var ?init) ...)
		       ?body0 ?body ...))
    )))

;;; --------------------------------------------------------------------

(define-syntax let/with-class
  (syntax-rules ()
    ((_ ?loop ((?var ?init) ...) ?body0 ?body ...)
     (%do-let/no-types let %let/with-class ?loop ((?var ?init) ...) ?body0 ?body ...))
    ((_ ((?var ?init) ...) ?body0 ?body ...)
     (%do-let/no-types let %let/with-class no-loop ((?var ?init) ...) ?body0 ?body ...))))

(define-syntax %let/with-class
  (lambda (stx)
    (syntax-case stx ()

      ((_ ?loop (((?var ?class0 ?class ...) ?init) ...) ?body0 ?body ...)
       (free-identifier=? #'no-loop #'?loop)
       #'(let ((?var ?init) ...)
	   (with-fields ((?var ?class0 ?class ...) ...) ?body0 ?body ...)))

      ((_ ?loop (((?var ?class0 ?class ...) ?init) ...) ?body0 ?body ...)
       #'(let ?loop ((?var ?init) ...)
	   (with-fields ((?var ?class0 ?class ...) ...) ?body0 ?body ...)))
      )))

;;; --------------------------------------------------------------------

(define-syntax let*/with-class
  (syntax-rules ()
    ((_ ((?var ?init) ...) ?body0 ?body ...)
     (%do-let/no-types let* %let*/with-class no-loop ((?var ?init) ...) ?body0 ?body ...))))

(define-syntax %let*/with-class
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

      ((let*/with-class (((?var0 ?class0 ?class00 ...) ?init0)
		     ((?var1 ?class1 ?class11 ...) ?init1)
		     ...)
	 ?body0 ?body ...)
       (let ((id (duplicated-ids? #'(?var0 ?var1 ...))))
	 (if id
	     #`(syntax-violation 'let*/with-class
		 "duplicated field names in let*/with-class"
		 (quote (let*/with-class (((?var0 ?class0 ?class00 ...) ?init0)
				      ((?var1 ?class1 ?class11 ...) ?init1)
				      ...)
			  ?body0 ?body ...))
		 (quote (#,id)))
	   #'(let ((?var0 ?init0))
	       (with-fields ((?var0 ?class0 ?class00 ...))
		 (let*/with-class (((?var1 ?class1 ?class11 ...) ?init1) ...) ?body0 ?body ...))))))

      ((_ () ?body0 ?body ...)
       #'(begin ?body0 ?body ...)))))

;;; --------------------------------------------------------------------

(define-syntax letrec/with-class
  (syntax-rules ()
    ((_ ((?var ?init) ...) ?body0 ?body ...)
     (%do-let/no-types letrec %letrec/with-class no-loop ((?var ?init) ...) ?body0 ?body ...))))

(define-syntax %letrec/with-class
  (syntax-rules ()
    ((_ (((?var ?class0 ?class ...) ?init) ...) ?body0 ?body ...)
     (let ((?var #f) ...)
       (with-fields ((?var ?class0 ?class ...) ...)
	 (set! ?var ?init) ...
	 ?body0 ?body ...)))))

;;; --------------------------------------------------------------------

(define-syntax letrec*/with-class
  (syntax-rules ()
    ((_ ((?var ?init) ...) ?body0 ?body ...)
     (%do-let/no-types letrec* %letrec*/with-class no-loop ((?var ?init) ...) ?body0 ?body ...))))

(define-syntax %letrec*/with-class
  ;;The  difference between  LETREC and  LETREC*  is only  the order  of
  ;;evaluation of ?INIT, which is enforced in LETREC*.
  ;;
  (syntax-rules ()
    ((_ (((?var ?class0 ?class ...) ?init) ...) ?body0 ?body ...)
     (let ((?var #f) ...)
       (with-fields ((?var ?class0 ?class ...) ...)
	 (set! ?var ?init) ...
	 ?body0 ?body ...)))))


(define-syntax define/with-class
  (syntax-rules ()
    ((_ (?variable . ?formals) . ?body)
     (define ?variable
       (lambda/with-class ?formals . ?body)))
    ((_ ?variable ?expression)
     (define ?variable ?expression))
    ((_ ?variable)
     (define ?variable))))

(define-syntax define/with-class*
  (syntax-rules ()
    ((_ (?variable . ?formals) . ?body)
     (define ?variable (lambda/with-class* ?formals . ?body)))
    ((_ ?variable ?expression)
     (define ?variable ?expression))
    ((_ ?variable)
     (define ?variable))))

(define-syntax lambda/with-class
  (syntax-rules ()
    ((_ ?formals . ?body)
     (%lambda/collect-classes-and-arguments #f ?formals
					    () ;collected classes
					    () ;collected args
					    . ?body))))

(define-syntax lambda/with-class*
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

(define-syntax receive/with-class
  (syntax-rules ()
    ((_ ?formals ?expression ?form0 ?form ...)
     (call-with-values
	 (lambda () ?expression)
       (lambda/with-class ?formals ?form0 ?form ...)))))


(define-syntax case-lambda/with-class
  (syntax-rules ()
    ((_ (?formals . ?body) ...)
     (%case-lambda/collect-classes-and-arguments
      #f
      ()	;collected CASE-LAMBDA clauses
      ()	;collected classes in current CASE-LAMBDA clause
      ()	;collected args in current CASE-LAMBDA clause
      (?formals . ?body)
      ...))))

(define-syntax case-lambda/with-class*
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

(define-virtual-class <builtin>
  (nongenerative nausicaa:builtin:<builtin>))

(define-syntax define-builtin-class
  (lambda (stx)
    (define (%uid name)
      (string->symbol (string-append "nausicaa:builtin:" (symbol->string name))))
    (syntax-case stx ()
      ((_ ?class-name ?clause ...)
       (with-syntax ((UID (datum->syntax #'?class-name (%uid (syntax->datum #'?class-name)))))
	 #'(define-virtual-class ?class-name
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
		  (immutable foldcase	string-foldcase))
  (setter string-set!)
  (getter string-ref))

(define-builtin-class <vector>
  (predicate vector?)
  (virtual-fields (immutable length vector-length))
  (setter vector-set!)
  (getter vector-ref))

(define-builtin-class <bytevector>
  (predicate bytevector?)
  (virtual-fields (immutable length bytevector-length))
  (setter <bytevector>-setf)
  (getter <bytevector>-getf))

(define-syntax <bytevector>-setf
  (syntax-rules (u8 s8 u16 s16 u16n s16n u32 s32 u32n s32n u64 s64 u64n s64n uint sint
		    single double singlen doublen big little)
    ((_ ?var ?idx ?value)
     (bytevector-u8-set! ?var ?idx ?value))

    ((_ ?var ?idx u8 ?value)
     (bytevector-u8-set! ?var ?idx ?value))
    ((_ ?var ?idx s8 ?value)
     (bytevector-s8-set! ?var ?idx ?value))
    ((_ ?var ?idx u16 ?endian ?value)
     (bytevector-u16-set! ?var ?idx ?value (endianness ?endian)))
    ((_ ?var ?idx s16 ?endian ?value)
     (bytevector-s16-set! ?var ?idx ?value (endianness ?endian)))
    ((_ ?var ?idx u32 ?endian ?value)
     (bytevector-u32-set! ?var ?idx ?value (endianness ?endian)))
    ((_ ?var ?idx s32 ?endian ?value)
     (bytevector-s32-set! ?var ?idx ?value (endianness ?endian)))
    ((_ ?var ?idx u64 ?endian ?value)
     (bytevector-u64-set! ?var ?idx ?value (endianness ?endian)))
    ((_ ?var ?idx s64 ?endian ?value)
     (bytevector-s64-set! ?var ?idx ?value (endianness ?endian)))
    ((_ ?var ?idx single ?endian ?value)
     (bytevector-ieee-single-set! ?var ?idx ?value (endianness ?endian)))
    ((_ ?var ?idx double ?endian ?value)
     (bytevector-ieee-double-set! ?var ?idx ?value (endianness ?endian)))

    ((_ ?var ?idx u16n ?value)		(bytevector-u16-native-set! ?var ?idx ?value))
    ((_ ?var ?idx s16n ?value)		(bytevector-s16-native-set! ?var ?idx ?value))
    ((_ ?var ?idx u32n ?value)		(bytevector-u32-native-set! ?var ?idx ?value))
    ((_ ?var ?idx s32n ?value)		(bytevector-s32-native-set! ?var ?idx ?value))
    ((_ ?var ?idx u64n ?value)		(bytevector-u64-native-set! ?var ?idx ?value))
    ((_ ?var ?idx s64n ?value)		(bytevector-s64-native-set! ?var ?idx ?value))
    ((_ ?var ?idx singlen ?value)	(bytevector-ieee-single-native-set! ?var ?idx ?value))
    ((_ ?var ?idx doublen ?value)	(bytevector-ieee-double-native-set! ?var ?idx ?value))
    ))

(define-syntax <bytevector>-getf
  (syntax-rules (u8 s8 u16 s16 u16n s16n u32 s32 u32n s32n u64 s64 u64n s64n uint sint
		    single double singlen doublen big little)

    ((_ ?var ?idx)		(bytevector-u8-ref ?var ?idx))

    ((_ ?var ?idx u8)
     (bytevector-u8-ref ?var ?idx))
    ((_ ?var ?idx s8)
     (bytevector-s8-ref ?var ?idx))
    ((_ ?var ?idx u16 ?endian)
     (bytevector-u16-ref ?var ?idx (endianness ?endian)))
    ((_ ?var ?idx s16 ?endian)
     (bytevector-s16-ref ?var ?idx (endianness ?endian)))
    ((_ ?var ?idx u32 ?endian)
     (bytevector-u32-ref ?var ?idx (endianness ?endian)))
    ((_ ?var ?idx s32 ?endian)
     (bytevector-s32-ref ?var ?idx (endianness ?endian)))
    ((_ ?var ?idx u64 ?endian)
     (bytevector-u64-ref ?var ?idx (endianness ?endian)))
    ((_ ?var ?idx s64 ?endian)
     (bytevector-s64-ref ?var ?idx (endianness ?endian)))
    ((_ ?var ?idx single ?endian)
     (bytevector-ieee-single-ref ?var ?idx (endianness ?endian)))
    ((_ ?var ?idx double ?endian)
     (bytevector-ieee-double-ref ?var ?idx (endianness ?endian)))

    ((_ ?var ?idx u16n)		(bytevector-u16-native-ref ?var ?idx))
    ((_ ?var ?idx s16n)		(bytevector-s16-native-ref ?var ?idx))
    ((_ ?var ?idx u32n)		(bytevector-u32-native-ref ?var ?idx))
    ((_ ?var ?idx s32n)		(bytevector-s32-native-ref ?var ?idx))
    ((_ ?var ?idx u64n)		(bytevector-u64-native-ref ?var ?idx))
    ((_ ?var ?idx s64n)		(bytevector-s64-native-ref ?var ?idx))
    ((_ ?var ?idx singlen)	(bytevector-ieee-single-native-ref ?var ?idx))
    ((_ ?var ?idx doublen)	(bytevector-ieee-double-native-ref ?var ?idx))
    ))

(define-builtin-class <hashtable>
  (predicate hashtable?)
  (virtual-fields (immutable size hashtable-size)
		  (immutable keys hashtable-keys)
		  (immutable entries hashtable-entries))
  (setter hashtable-set!)
  (getter <hashtable>-getf))

(define-syntax <hashtable>-getf
  (syntax-rules ()
    ((_ ?variable-name ?key)
     (hashtable-ref ?variable-name ?key #f))
    ((_ ?variable-name ?key ?default)
     (hashtable-ref ?variable-name ?key ?default))))

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

(define-virtual-class <input-port>
  (parent <port>)
  (predicate input-port?)
  (nongenerative nausicaa:builtin:<input-port>))

(define-virtual-class <output-port>
  (parent <port>)
  (predicate output-port?)
  (nongenerative nausicaa:builtin:<output-port>))

(define-virtual-class <binary-port>
  (parent <port>)
  (predicate %binary-port?)
  (nongenerative nausicaa:builtin:<binary-port>))

;;This predicate is needed because in some implementations (Petite Chez)
;;BINARY-PORT? throws an exception if OBJ is not a port.
(define (%binary-port? obj)
  (and (port? obj) (binary-port? obj)))

(define-virtual-class <textual-port>
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

(define-virtual-class <complex>
  (parent <number>)
  (predicate complex?)
  (virtual-fields (immutable real-part	real-part)
		  (immutable imag-part	imag-part)
		  (immutable magnitude	magnitude)
		  (immutable angle	angle))
  (nongenerative nausicaa:builtin:<complex>))

(define-virtual-class <real-valued>
  (parent <complex>)
  (predicate real-valued?)
  (virtual-fields (immutable positive?		positive?)
		  (immutable negative?		negative?)
		  (immutable non-positive?	non-positive?)
		  (immutable non-negative?	non-negative?))
  (nongenerative nausicaa:builtin:<real-valued>))

(define-virtual-class <real>
  (parent <real-valued>)
  (predicate real?)
  (nongenerative nausicaa:builtin:<real>)
  (virtual-fields (immutable abs)))

(define-virtual-class <rational-valued>
  (parent <real>)
  (predicate rational-valued?)
  (nongenerative nausicaa:builtin:<rational-valued>))

(define-virtual-class <flonum>
  (parent <real>)
  (predicate flonum?)
  (nongenerative nausicaa:builtin:<flonum>))

(define-virtual-class <rational>
  (parent <rational-valued>)
  (predicate rational?)
  (nongenerative nausicaa:builtin:<rational>))

(define-virtual-class <integer-valued>
  (parent <rational-valued>)
  (predicate integer-valued?)
  (nongenerative nausicaa:builtin:<integer-valued>))

(define-virtual-class <integer>
  (parent <integer-valued>)
  (predicate integer?)
  (nongenerative nausicaa:builtin:<integer>))

(define-virtual-class <fixnum>
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
       #'(record-parent-list (class-record-descriptor ?record-name))))))

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
   ((hashtable?	obj)		(class-record-descriptor <hashtable>))

   ((record? obj)
    (record-rtd obj))

   ((number? obj)
    ;;Order does matter here!!!
    (cond ((fixnum?		obj)	(class-record-descriptor <fixnum>))
	  ((integer?		obj)	(class-record-descriptor <integer>))
	  ((rational?		obj)	(class-record-descriptor <rational>))
	  ((integer-valued?	obj)	(class-record-descriptor <integer-valued>))
	  ((rational-valued?	obj)	(class-record-descriptor <rational-valued>))
	  ((flonum?		obj)	(class-record-descriptor <flonum>))
	  ((real?		obj)	(class-record-descriptor <real>))
	  ((real-valued?	obj)	(class-record-descriptor <real-valued>))
	  ((complex?		obj)	(class-record-descriptor <complex>))
	  (else				(class-record-descriptor <number>))))
   ((char?		obj)		(class-record-descriptor <char>))
   ((string?		obj)		(class-record-descriptor <string>))
   ((vector?		obj)		(class-record-descriptor <vector>))
   ((bytevector?	obj)		(class-record-descriptor <bytevector>))
   ((port?		obj)
    ;;Order here is arbitrary.
    (cond ((input-port?		obj)	(class-record-descriptor <input-port>))
	  ((output-port?	obj)	(class-record-descriptor <output-port>))
	  ((binary-port?	obj)	(class-record-descriptor <binary-port>))
	  ((textual-port?	obj)	(class-record-descriptor <textual-port>))
	  (else				(class-record-descriptor <port>))))
   ((condition?		obj)		(class-record-descriptor <condition>))
   ((record?		obj)		(class-record-descriptor <record>))
   ((pair?		obj)
    ;;Order does matter  here!!!  Better leave these at  the end because
    ;;qualifying a long list can be time-consuming.
    (cond ((list?	obj)	(class-record-descriptor <list>))
	  (else			(class-record-descriptor <pair>))))
   (else (record-type-descriptor <top>))))


;;;; done

)

;;; end of file
