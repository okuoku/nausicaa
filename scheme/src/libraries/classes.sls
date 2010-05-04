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

    ;; usage macros
    define-class			define-virtual-class
    define-label			is-a?
    make				make-from-fields

    ;; inspection macros
    class-record-type-descriptor
    class-public-constructor-descriptor	class-superclass-constructor-descriptor
    class-from-fields-constructor-descriptor
    class-type-uid			class-uid-list
    class-parent-rtd-list

    ;; inspection functions
    record-type-parent?
    class-uid-equal-or-parent?
    record-type-of
    record-parent-list
    class-uid-list-of

    ;; dot notation syntaxes
    with-class
    setf				getf
    define/with-class			define/with-class*
    lambda/with-class			lambda/with-class*
    case-lambda/with-class		case-lambda/with-class*
    receive/with-class
    let/with-class			let*/with-class
    letrec/with-class			letrec*/with-class

    ;; builtin classes
    <top> <builtin> <pair> <list>
    <char> <string> <vector> <bytevector> <hashtable> <record> <condition>
    <port> <binary-port> <input-port> <output-port> <textual-port>
    <fixnum> <flonum> <integer> <integer-valued> <rational> <rational-valued>
    <real> <real-valued> <complex> <number>)
  (import (rnrs)
    (rnrs mutable-strings)
    (gensym)
    (for (classes helpers) expand))


;;;; helpers

(define-syntax with-accessor-and-mutator
  (syntax-rules ()
    ((_ ((?name ?thing ?accessor ?mutator) ?spec ...) ?body0 ?body ...)
     (let-syntax ((?name (identifier-syntax
			  (_              (?accessor ?thing))
			  ((set! _ ?expr) (?mutator ?thing ?expr)))))
       (with-accessor-and-mutator (?spec ...) ?body0 ?body ...)))
    ((_ ((?name ?thing ?accessor) ?spec ...) ?body0 ?body ...)
     (let-syntax ((?name (identifier-syntax (?accessor ?thing))))
       (with-accessor-and-mutator (?spec ...) ?body0 ?body ...)))
    ((_ () ?body0 ?body ...)
     (begin ?body0 ?body ...))))


;;;; inspection procedures

(define (record-type-parent? maybe-parent-rtd maybe-child-rtd)
  (memq (record-type-uid maybe-parent-rtd)
	(map record-type-uid (record-parent-list maybe-child-rtd))))

(define (class-uid-equal-or-parent? maybe-parent-uid-list maybe-child-uid-list)
  ;;Given  two lists of  record type  UIDs representing  the inheritance
  ;;hierarchy  of two  classes, return  true if  the first  represents a
  ;;parent of the second.
  ;;
  (let ((maybe-parent-uid (car maybe-parent-uid-list))
	(maybe-child-uid  (car maybe-child-uid-list)))
    (cond
     ((eq? maybe-parent-uid maybe-child-uid)		#t) ;includes <top> and <top>
     ((eq? maybe-parent-uid 'nausicaa:builtin:<top>)	#t) ;<top> is parent of everything
     ((eq? maybe-child-uid  'nausicaa:builtin:<top>)	#f) ;<top> is child of nothing
     (else
      (memq maybe-parent-uid maybe-child-uid-list)))))

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
   ((hashtable?	obj)		(class-record-type-descriptor <hashtable>))

   ((record? obj)
    (record-rtd obj))

   ((number? obj)
    ;;Order does matter here!!!
    (cond ((fixnum?		obj)	(class-record-type-descriptor <fixnum>))
	  ((integer?		obj)	(class-record-type-descriptor <integer>))
	  ((rational?		obj)	(class-record-type-descriptor <rational>))
	  ((integer-valued?	obj)	(class-record-type-descriptor <integer-valued>))
	  ((rational-valued?	obj)	(class-record-type-descriptor <rational-valued>))
	  ((flonum?		obj)	(class-record-type-descriptor <flonum>))
	  ((real?		obj)	(class-record-type-descriptor <real>))
	  ((real-valued?	obj)	(class-record-type-descriptor <real-valued>))
	  ((complex?		obj)	(class-record-type-descriptor <complex>))
	  (else				(class-record-type-descriptor <number>))))
   ((char?		obj)		(class-record-type-descriptor <char>))
   ((string?		obj)		(class-record-type-descriptor <string>))
   ((vector?		obj)		(class-record-type-descriptor <vector>))
   ((bytevector?	obj)		(class-record-type-descriptor <bytevector>))
   ((port?		obj)
    ;;Order here is arbitrary.
    (cond ((input-port?		obj)	(class-record-type-descriptor <input-port>))
	  ((output-port?	obj)	(class-record-type-descriptor <output-port>))
	  ((binary-port?	obj)	(class-record-type-descriptor <binary-port>))
	  ((textual-port?	obj)	(class-record-type-descriptor <textual-port>))
	  (else				(class-record-type-descriptor <port>))))
   ((condition?		obj)		(class-record-type-descriptor <condition>))
   ((record?		obj)		(class-record-type-descriptor <record>))
   ((pair?		obj)
    ;;Order does matter  here!!!  Better leave these at  the end because
    ;;qualifying a long list can be time-consuming.
    (cond ((list?	obj)	(class-record-type-descriptor <list>))
	  (else			(class-record-type-descriptor <pair>))))
   (else (record-type-descriptor <top>))))

(define (class-uid-list-of obj)
  ;;Return the list of UIDs in the class hierarchy of OBJ.  The order of
  ;;the tests is important.  More specialised types must come first.
  ;;
  (cond

   ;;This  is  here  as  a  special exception  because  in  Larceny  the
   ;;hashtable  is a  record.  We  have  to process  it before  applying
   ;;RECORD?
   ((hashtable?	obj)			(class-uid-list <hashtable>))

   ((record? obj)			(map record-type-uid (record-parent-list (record-rtd obj))))

   ((number? obj)
    ;;Order does matter here!!!
    (cond ((fixnum?		obj)	(class-uid-list <fixnum>))
	  ((integer?		obj)	(class-uid-list <integer>))
	  ((rational?		obj)	(class-uid-list <rational>))
	  ((integer-valued?	obj)	(class-uid-list <integer-valued>))
	  ((rational-valued?	obj)	(class-uid-list <rational-valued>))
	  ((flonum?		obj)	(class-uid-list <flonum>))
	  ((real?		obj)	(class-uid-list <real>))
	  ((real-valued?	obj)	(class-uid-list <real-valued>))
	  ((complex?		obj)	(class-uid-list <complex>))
	  (else				(class-uid-list <number>))))
   ((char?		obj)		(class-uid-list <char>))
   ((string?		obj)		(class-uid-list <string>))
   ((vector?		obj)		(class-uid-list <vector>))
   ((bytevector?	obj)		(class-uid-list <bytevector>))
   ((port?		obj)
    ;;Order here is arbitrary.
    (cond ((input-port?		obj)	(class-uid-list <input-port>))
	  ((output-port?	obj)	(class-uid-list <output-port>))
	  ((binary-port?	obj)	(class-uid-list <binary-port>))
	  ((textual-port?	obj)	(class-uid-list <textual-port>))
	  (else				(class-uid-list <port>))))
   ((condition?		obj)		(class-uid-list <condition>))
   ((record?		obj)		(class-uid-list <record>))
   ((pair?		obj)
    ;;Order does matter  here!!!  Better leave these at  the end because
    ;;qualifying a long list can be time-consuming.
    (cond ((list?	obj)	(class-uid-list <list>))
	  (else			(class-uid-list <pair>))))
   (else '(nausicaa:builtin:<top>))))

(define (%make-from-fields-cd rtd)
  ;;Given  a  record  type  descriptor  build  and  return  its  default
  ;;constructor descriptor: the one accepting the raw field values.  The
  ;;returned descriptor is used by the MAKE-FROM-FIELDS syntax.
  ;;
  (make-record-constructor-descriptor rtd
				      (let ((parent-rtd (record-type-parent rtd)))
					(if parent-rtd
					    (%make-from-fields-cd parent-rtd)
					  #f))
				      #f))


;;;; inspection macros

(define-syntax class-record-type-descriptor
  ;;Expand into the record type  descriptor (RTD) record associated to a
  ;;class name.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?class-name)
       (free-identifier=? #'?class-name #'<top>)
       #'(record-type-descriptor <top>))
      ((_ ?class-name)
       #'(?class-name class-record-type-descriptor)))))

;;; --------------------------------------------------------------------

(define-syntax class-public-constructor-descriptor
  ;;Expand into the class' public constructor descriptor associated to a
  ;;class name.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?class-name)
       (free-identifier=? #'?class-name #'<top>)
       #'(record-constructor-descriptor ?class-name))
      ((_ ?class-name)
       #'(?class-name public-constructor-descriptor)))))

(define-syntax class-superclass-constructor-descriptor
  ;;Expand into the  class' superclass constructor descriptor associated
  ;;to a class name.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?class-name)
       (free-identifier=? #'?class-name #'<top>)
       #'(record-constructor-descriptor ?class-name))
      ((_ ?class-name)
       #'(?class-name superclass-constructor-descriptor)))))

(define-syntax class-from-fields-constructor-descriptor
  ;;Expand into the class' from-fields constructor descriptor associated
  ;;to a class name.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?class-name)
       (free-identifier=? #'?class-name #'<top>)
       #'(record-constructor-descriptor ?class-name))
      ((_ ?class-name)
       #'(?class-name from-fields-constructor-descriptor)))))

;;; --------------------------------------------------------------------

(define-syntax class-type-uid
  ;;Expand into the class type UID associated to a class name.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?class-name)
       (free-identifier=? #'?class-name #'<top>)
       #'(record-type-uid (record-type-descriptor <top>)))
      ((_ ?class-name)
       #'(?class-name class-type-uid)))))

(define-syntax class-uid-list
  ;;Expand into  the list of type UIDs  of the parents of  a class name.
  ;;The first element is the UID of the class itself, then comes the UID
  ;;of the parent, then the UID of the parent's parent, etc.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?class-name)
       (free-identifier=? #'?class-name #'<top>)
       #'(list (record-type-uid (record-type-descriptor <top>))))
      ((_ ?class-name)
       #'(?class-name class-uid-list)))))

;;; --------------------------------------------------------------------

(define-syntax class-parent-rtd-list
  (syntax-rules ()
    ((_ ?class-name)
     (?class-name parent-rtd-list))))


;;;; usage macros

(define-syntax make
  ;;Build a new class instance using the public constructor.
  ;;
  (syntax-rules ()
    ((_ ?class-name ?arg ...)
     (?class-name make ?arg ...))))

(define-syntax make-from-fields
  ;;Build a new class instance using the "from fields" constructor.
  ;;
  (syntax-rules ()
    ((_ ?class-name ?arg ...)
     (?class-name make-from-fields ?arg ...))))

(define-syntax is-a?
  ;;Test  if  a  given object  matches  a  class  type using  the  class
  ;;predicate selected in the DEFINE-CLASS form.
  ;;
  (lambda (stx)
    (syntax-case stx ()

      ((_ ?obj ?class-name)
       (free-identifier=? #'?class-name #'<top>)
       (syntax #t))

      ((_ ?obj ?class-name)
       (identifier? #'?class-name)
       #'(?class-name is-a? ?obj))

      (?input-form
       (syntax-violation 'is-a? "invalid syntax use" (syntax->datum #'?input-form))))))


(define-syntax define-virtual-class
  ;;A virtual class  is just a tag  we slap on any value  to use virtual
  ;;fields  and methods  with dot  notation,  but nevertheless  it is  a
  ;;proper record type.
  ;;
  (syntax-rules ()
    ((_ ?name ?clause ...)
     (%define-virtual-class (define-class ?name ?clause ...) ?name () ?clause ...))))

(define-syntax %define-virtual-class
  ;;Raise an  error if a PUBLIC-PROTOCOL  or FIELD clause  is present in
  ;;the body of the definition;  else define the class with DEFINE-CLASS
  ;;specifying a public protocol which raises an error when invoked.
  ;;
  (lambda (stx)
    (syntax-case stx (fields public-protocol)

      ;;no more clauses to collect
      ((_ ?input-form ?name (?collected-clause ...))
       #'(define-class ?name
	   (public-protocol (lambda (make-parent)
			      (lambda args
				(syntax-violation #f
				  "attempt to instantiate virtual class" (quote ?name)))))
	   ?collected-clause ...))

      ;;found PUBLIC-PROTOCOL clause
      ((_ ?input-form ?name (?collected-clause ...) (public-protocol ?pro ...) ?clause ...)
       (syntax-violation 'define-class
	 "public-protocol clause used in definition of virtual class"
	 (syntax->datum #'?input-form)
	 (syntax->datum #'(public-protocol ?pro ...))))

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
    (syntax-case stx (fields mutable immutable parent sealed opaque parent-rtd nongenerative
			     virtual-fields methods method predicate setter getter inherit
			     bindings)

      ((_ (?name ?constructor ?predicate) ?clause ...)
       (all-identifiers? #'(?name ?constructor ?predicate))
       #'(%define-class/sort-clauses
	  (define-class (?name ?constructor ?predicate) ?clause ...)
	  (?name ?constructor ?predicate)
	  (#f #f #f) ;common protocol, public protocol, superclass protocol
	  ()	     ;collected concrete fields
	  ()	     ;collected virtual fields
	  ()	     ;collected methods
	  ()	     ;collected definitions
	  (predicate) (setter) (getter) (bindings)
	  (parent) (inherit) (sealed) (opaque) (parent-rtd) (nongenerative)
	  ?clause ...))

      ((_ ?name ?clause ...)
       (identifier? (syntax ?name))
       #`(%define-class/sort-clauses
	  (define-class ?name ?clause ...)
	  (?name #,(syntax-prefix "make-" #'?name) #,(syntax-suffix #'?name "?"))
	  (#f #f #f) ;common protocol, public protocol, superclass protocol
	  ()	     ;collected concrete fields
	  ()	     ;collected virtual fields
	  ()	     ;collected methods
	  ()	     ;collected definitions
	  (predicate) (setter) (getter) (bindings)
	  (parent) (inherit) (sealed) (opaque) (parent-rtd) (nongenerative)
	  ?clause ...))

      ((_ ?name-spec . ?clauses)
       (syntax-violation 'define-class
	 "invalid name specification in class definition"
	 (syntax->datum (syntax ?input-form))
	 (syntax->datum (syntax ?name-spec))))
      )))


(define-syntax %define-class/sort-clauses
  ;;Sorts all  the auxiliary  syntaxes.  Collects the  specifications of
  ;;concrete and virtual fields; expands the field clauses given with no
  ;;accessor  and mutator  names to  clauses with  accessor  and mutator
  ;;names automatically built  from the record type name  (as defined by
  ;;R6RS).   Collects the  method clauses,  expanding the  ones  with no
  ;;function name to clauses with function name automatically built from
  ;;the record type name.
  ;;
  (lambda (stx)
    (define (%sinner msg input-form subform)
      (syntax-violation 'define-class msg (syntax->datum input-form) (syntax->datum subform)))

    (syntax-case stx (fields mutable immutable parent
			     protocol public-protocol superclass-protocol
			     sealed opaque parent-rtd nongenerative
			     virtual-fields methods method method-syntax
			     predicate setter getter inherit bindings)

      ;;Gather the INHERIT clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	. ?inherit-rest)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(inherit ?superclass-name . ?inherit-clauses) ?clause ...)
       (let ((form	(syntax ?input-form))
	     (subform	(syntax (inherit ?superclass-name))))
	 (cond ((not (null? (syntax->datum (syntax ?inherit-rest))))
		(%sinner "inherit clause given twice in class definition" form subform))
	       ((not (identifier? (syntax ?superclass-name)))
		(%sinner "invalid inherit clause in class definition" form subform))
	       (else
		#'(%define-class/sort-clauses
		   ?input-form (?name ?constructor ?predicate)
		   (?common-protocol ?public-protocol ?superclass-protocol)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-definition ...)
		   (predicate		?pre ...)
		   (setter		?set ...)
		   (getter		?get ...)
		   (bindings		?bin ...)
		   (parent		?par ...)
		   (inherit		?superclass-name . ?inherit-clauses)
		   (sealed		?sea ...)
		   (opaque		?opa ...)
		   (parent-rtd		?pad ...)
		   (nongenerative	?non ...)
		   ?clause ...)))))

      ;;Gather the PARENT clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		. ?parent-rest)
	(inherit	?inh ...)
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
		   (?common-protocol ?public-protocol ?superclass-protocol)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-definition ...)
		   (predicate		?pre ...)
		   (setter		?set ...)
		   (getter		?get ...)
		   (bindings		?bin ...)
		   (parent		?parent-name)
		   (inherit		?inh ...)
		   (sealed		?sea ...)
		   (opaque		?opa ...)
		   (parent-rtd		?pad ...)
		   (nongenerative	?non ...)
		   ?clause ...)))))

      ;;Gather the PREDICATE clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	. ?predicate-rest)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
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
		   (?common-protocol ?public-protocol ?superclass-protocol)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-definition ...)
		   (predicate		?function-name)
		   (setter		?set ...)
		   (getter		?get ...)
		   (bindings		?bin ...)
		   (parent		?par ...)
		   (inherit		?inh ...)
		   (sealed		?sea ...)
		   (opaque		?opa ...)
		   (parent-rtd		?pad ...)
		   (nongenerative	?non ...)
		   ?clause ...)))))

      ;;Gather the SETTER clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		. ?setter-rest)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
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
		   (?common-protocol ?public-protocol ?superclass-protocol)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-definition ...)
		   (predicate		?pre ...)
		   (setter		?setter)
		   (getter		?get ...)
		   (bindings		?bin ...)
		   (parent		?par ...)
		   (inherit		?inh ...)
		   (sealed		?sea ...)
		   (opaque		?opa ...)
		   (parent-rtd		?pad ...)
		   (nongenerative	?non ...)
		   ?clause ...)))))

      ;;Gather the GETTER clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		. ?getter-rest)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
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
		   (?common-protocol ?public-protocol ?superclass-protocol)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-definition ...)
		   (predicate		?pre ...)
		   (setter		?set ...)
		   (getter		?getter)
		   (bindings		?bin ...)
		   (parent		?par ...)
		   (inherit		?inh ...)
		   (sealed		?sea ...)
		   (opaque		?opa ...)
		   (parent-rtd		?pad ...)
		   (nongenerative	?non ...)
		   ?clause ...)))))

;;; --------------------------------------------------------------------

      ;;Gather the BINDINGS clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	. ?bindings-rest)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(bindings ?macro-name) ?clause ...)
       (let ((form	(syntax ?input-form))
	     (subform	(syntax (bindings ?macro-name))))
	 (cond ((not (null? (syntax->datum (syntax ?bindings-rest))))
		(%sinner "bindings clause given twice in class definition" form subform))
	       ((not (identifier? (syntax ?macro-name)))
		(%sinner "invalid bindings clause in class definition" form subform))
	       (else
		#'(%define-class/sort-clauses
		   ?input-form (?name ?constructor ?predicate)
		   (?common-protocol ?public-protocol ?superclass-protocol)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-definition ...)
		   (predicate		?pre ...)
		   (setter		?set ...)
		   (getter		?get ...)
		   (bindings		?macro-name)
		   (parent		?par ...)
		   (inherit		?inh ...)
		   (sealed		?sea ...)
		   (opaque		?opa ...)
		   (parent-rtd		?pad ...)
		   (nongenerative	?non ...)
		   ?clause ...)))))

;;; --------------------------------------------------------------------

      ;;Gather the PROTOCOL clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(protocol ?protocol-expression) ?clause ...)
       (if (syntax->datum #'?common-protocol)
	   (%sinner "protocol clause given twice in class definition"
		    (syntax ?input-form)
		    (syntax (protocol ?protocol-expression)))
	 #'(%define-class/sort-clauses
	    ?input-form (?name ?constructor ?predicate)
	    (?protocol-expression ?public-protocol ?superclass-protocol)
	    (?collected-concrete-field ...)
	    (?collected-virtual-field ...)
	    (?collected-method ...)
	    (?collected-definition ...)
	    (predicate		?pre ...)
	    (setter		?set ...)
	    (getter		?get ...)
	    (bindings		?bin ...)
	    (parent		?par ...)
	    (inherit		?inh ...)
	    (sealed		?sea ...)
	    (opaque		?opa ...)
	    (parent-rtd		?pad ...)
	    (nongenerative	?non ...)
	    ?clause ...)))

      ;;Gather the PUBLIC-PROTOCOL clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(public-protocol ?protocol-expression) ?clause ...)
       (if (syntax->datum #'?public-protocol)
	   (%sinner "public protocol clause given twice in class definition"
		    (syntax ?input-form)
		    (syntax (public-protocol ?protocol-expression)))
	 #'(%define-class/sort-clauses
	    ?input-form (?name ?constructor ?predicate)
	    (?common-protocol ?protocol-expression ?superclass-protocol)
	    (?collected-concrete-field ...)
	    (?collected-virtual-field ...)
	    (?collected-method ...)
	    (?collected-definition ...)
	    (predicate		?pre ...)
	    (setter		?set ...)
	    (getter		?get ...)
	    (bindings		?bin ...)
	    (parent		?par ...)
	    (inherit		?inh ...)
	    (sealed		?sea ...)
	    (opaque		?opa ...)
	    (parent-rtd		?pad ...)
	    (nongenerative	?non ...)
	    ?clause ...)))

      ;;Gather the SUPERCLASS-PROTOCOL clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(superclass-protocol ?protocol-expression) ?clause ...)
       (if (syntax->datum #'?superclass-protocol)
	   (%sinner "superclass protocol clause given twice in class definition"
		    (syntax ?input-form)
		    (syntax (superclass-protocol ?protocol-expression)))
	 #'(%define-class/sort-clauses
	    ?input-form (?name ?constructor ?predicate)
	    (?common-protocol ?public-protocol ?protocol-expression)
	    (?collected-concrete-field ...)
	    (?collected-virtual-field ...)
	    (?collected-method ...)
	    (?collected-definition ...)
	    (predicate		?pre ...)
	    (setter		?set ...)
	    (getter		?get ...)
	    (bindings		?bin ...)
	    (parent		?par ...)
	    (inherit		?inh ...)
	    (sealed		?sea ...)
	    (opaque		?opa ...)
	    (parent-rtd		?pad ...)
	    (nongenerative	?non ...)
	    ?clause ...)))

;;; --------------------------------------------------------------------

      ;;Gather the SEALED clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
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
		   (?common-protocol ?public-protocol ?superclass-protocol)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-definition ...)
		   (predicate		?pre ...)
		   (setter		?set ...)
		   (getter		?get ...)
		   (bindings		?bin ...)
		   (parent		?par ...)
		   (inherit		?inh ...)
		   (sealed		?sealed)
		   (opaque		?opa ...)
		   (parent-rtd		?pad ...)
		   (nongenerative	?non ...)
		   ?clause ...)))))

      ;;Gather the OPAQUE clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
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
		   (?common-protocol ?public-protocol ?superclass-protocol)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-definition ...)
		   (predicate		?pre ...)
		   (setter		?set ...)
		   (getter		?get ...)
		   (bindings		?bin ...)
		   (parent		?par ...)
		   (inherit		?inh ...)
		   (sealed		?sea ...)
		   (opaque		?opaque)
		   (parent-rtd		?pad ...)
		   (nongenerative	?non ...)
		   ?clause ...)))))

      ;;Gather the PARENT-RTD clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
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
		   (?common-protocol ?public-protocol ?superclass-protocol)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-definition ...)
		   (predicate		?pre ...)
		   (setter		?set ...)
		   (getter		?get ...)
		   (bindings		?bin ...)
		   (parent		?par ...)
		   (inherit		?inh ...)
		   (sealed		?sea ...)
		   (opaque		?opa ...)
		   (parent-rtd		?parent-rtd ?parent-cd)
		   (nongenerative	?non ...)
		   ?clause ...)))))

      ;;Gather the NONGENERATIVE empty clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
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
		   (?common-protocol ?public-protocol ?superclass-protocol)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-definition ...)
		   (predicate		?pre ...)
		   (setter		?set ...)
		   (getter		?get ...)
		   (bindings		?bin ...)
		   (parent		?par ...)
		   (inherit		?inh ...)
		   (sealed		?sea ...)
		   (opaque		?opa ...)
		   (parent-rtd		?pad ...)
		   ;;We have  to do this  to properly generate  a unique
		   ;;UID.  We cannot rely on the expander renaming a raw
		   ;;symbol we may introduce here.
		   (nongenerative	#,(datum->syntax #'?name (gensym)))
		   ?clause ...)))))

      ;;Gather the NONGENERATIVE non-empty clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
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
		   (?common-protocol ?public-protocol ?superclass-protocol)
		   (?collected-concrete-field ...)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-definition ...)
		   (predicate		?pre ...)
		   (setter		?set ...)
		   (getter		?get ...)
		   (bindings		?bin ...)
		   (parent		?par ...)
		   (inherit		?inh ...)
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
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields ?field-clause ...) ?clause ...)
       #'(%define-class/sort-clauses/fields
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
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
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields ?field-clause ...) ?clause ...)
       #'(%define-class/sort-clauses/virtual-fields
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ... )
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
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
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(methods ?method-clause ...) ?clause ...)
       #'(%define-class/sort-clauses/methods
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (methods ?method-clause ...) ?clause ...))

;;; --------------------------------------------------------------------

      ;;Gather a METHOD clause with define-like function definition.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(method (?method . ?args) . ?body) ?clause ...)
       (identifier? #'?method)
       #'(%define-class/sort-clauses
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ... (?method function-name))
	  (?collected-definition ... (define/with-class (function-name . ?args) . ?body))
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  ?clause ...))

      ;;Gather a METHOD clause with expression function definition.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(method ?method ?expression) ?clause ...)
       (identifier? #'?method)
       #'(%define-class/sort-clauses
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ... (?method function-name))
	  (?collected-definition ... (define function-name ?expression))
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  ?clause ...))

;;; --------------------------------------------------------------------

      ;;Gather a METHOD-SYNTAX clause.
      ((%define-class/sort-clauses
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(method-syntax ?method ?transformer) ?clause ...)
       (identifier? #'?method)
       #'(%define-class/sort-clauses
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ... (?method macro-name))
	  (?collected-definition ... (define-syntax macro-name ?transformer))
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  ?clause ...))

;;; --------------------------------------------------------------------

      ;;No    more   clauses    to   gather.     Hand    everything   to
      ;;%DEFINE-CLASS/NORMALISE-INHERITANCE.
      ;;
      ((_ ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings	?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...))
       #'(%define-class/normalise-inheritance
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
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
    (syntax-case stx (fields mutable immutable parent sealed opaque parent-rtd nongenerative
			     predicate setter getter inherit bindings)

      ;;Gather mutable FIELDS clause with explicit selection of accessor
      ;;and mutator names.
      ((%define-class/sort-clauses/fields
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields (mutable ?field ?accessor ?mutator) ?field-clause ...) ?clause ...)
       (all-identifiers? #'(?field ?accessor ?mutator))
       #'(%define-class/sort-clauses/fields
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ... (mutable ?field ?accessor ?mutator))
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (fields ?field-clause ...) ?clause ...))

      ;;Gather  mutable  FIELDS   clause  with  automatically  generated
      ;;accessor and mutator names.
      ((%define-class/sort-clauses/fields
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields (mutable ?field) ?field-clause ...) ?clause ...)
       (identifier? #'?field)
       #`(%define-class/sort-clauses/fields
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ... (mutable ?field
						  #,(syntax-accessor-name #'?name #'?field)
						  #,(syntax-mutator-name  #'?name #'?field)))
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (fields ?field-clause ...) ?clause ...))

      ;;Gather  immutable  FIELDS  clause  with  explicit  selection  of
      ;;accessor name.
      ((%define-class/sort-clauses/fields
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields (immutable ?field ?accessor) ?field-clause ...) ?clause ...)
       (all-identifiers? #'(?field ?accessor))
       #'(%define-class/sort-clauses/fields
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ... (immutable ?field ?accessor))
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (fields ?field-clause ...) ?clause ...))

      ;;Gather  immutable  FIELDS  clause with  automatically  generated
      ;;accessor name.
      ((%define-class/sort-clauses/fields
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields (immutable ?field) ?field-clause ...) ?clause ...)
       (identifier? #'?field)
       #`(%define-class/sort-clauses/fields
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ... (immutable ?field #,(syntax-accessor-name #'?name #'?field)))
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate	?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd	?pad ...)
	  (nongenerative	?non ...)
	  (fields ?field-clause ...) ?clause ...))

      ;;Gather   immutable  FIELDS   clause  declared   without  IMMUTABLE
      ;;auxiliary syntax.
      ((%define-class/sort-clauses/fields
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields ?field ?field-clause ...) ?clause ...)
       (identifier? #'?field)
       #`(%define-class/sort-clauses/fields
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ... (immutable ?field #,(syntax-accessor-name #'?name #'?field)))
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate	?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (fields ?field-clause ...) ?clause ...))

;;; --------------------------------------------------------------------

      ;;Remove empty, leftover, FIELDS clause.
      ((%define-class/sort-clauses/fields
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields) ?clause ...)
       #'(%define-class/sort-clauses
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  ?clause ...))

      ;;Error.
      ((%define-class/sort-clauses/fields
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(parent		?par ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(fields ?field-spec ?field-clause ...) ?clause ...)
       (syntax-violation 'define-class
	 "invalid field specification"
	 (syntax->datum #'?input-form)
	 (syntax->datum #'(fields ?field-spec ?field-clause ...))))

      )))


(define-syntax %define-class/sort-clauses/virtual-fields
  (lambda (stx)
    (syntax-case stx (mutable immutable parent sealed opaque parent-rtd nongenerative
			      virtual-fields predicate setter getter inherit bindings)

      ;;Gather mutable VIRTUAL-FIELDS  clause with explicit selection of
      ;;accessor and mutator names.
      ((%define-class/sort-clauses/virtual-fields
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields (mutable ?field ?accessor ?mutator) ?field-clause ...) ?clause ...)
       (all-identifiers? #'(?field ?accessor ?mutator))
       #'(%define-class/sort-clauses/virtual-fields
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...  (mutable ?field ?accessor ?mutator))
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (virtual-fields ?field-clause ...) ?clause ...))

      ;;Gather   mutable   VIRTUAL-FIELDS   clause  with   automatically
      ;;generated accessor and mutator names.
      ((%define-class/sort-clauses/virtual-fields
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields (mutable ?field) ?field-clause ...) ?clause ...)
       (identifier? #'?field)
       #`(%define-class/sort-clauses/virtual-fields
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...  (mutable ?field
						  #,(syntax-accessor-name #'?name #'?field)
						  #,(syntax-mutator-name  #'?name #'?field)))
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate	?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (virtual-fields ?field-clause ...) ?clause ...))

;;; --------------------------------------------------------------------

      ;;Gather immutable  VIRTUAL-FIELDS clause with  explicit selection
      ;;of accessor name.
      ((%define-class/sort-clauses/virtual-fields
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields (immutable ?field ?accessor) ?field-clause ...) ?clause ...)
       (all-identifiers? #'(?field ?accessor))
       #'(%define-class/sort-clauses/virtual-fields
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ... (immutable ?field ?accessor))
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (virtual-fields ?field-clause ...) ?clause ...))

      ;;Gather   immutable  VIRTUAL-FIELDS  clause   with  automatically
      ;;generated accessor name.
      ((%define-class/sort-clauses/virtual-fields
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields (immutable ?field) ?field-clause ...) ?clause ...)
       (identifier? #'?field)
       #`(%define-class/sort-clauses/virtual-fields
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ... (immutable ?field #,(syntax-accessor-name #'?name #'?field)))
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (virtual-fields ?field-clause ...) ?clause ...))

      ;;Gather immutable VIRTUAL-FIELDS  clause declared without IMMUTABLE
      ;;auxiliary syntax.
      ((%define-class/sort-clauses/virtual-fields
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields ?field ?field-clause ...) ?clause ...)
       (identifier? #'?field)
       #`(%define-class/sort-clauses/virtual-fields
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ... (immutable ?field #,(syntax-accessor-name #'?name #'?field)))
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (virtual-fields ?field-clause ...) ?clause ...))

;;; --------------------------------------------------------------------

      ;;Remove empty, leftover, VIRTUAL-FIELDS clause.
      ((%define-class/sort-clauses/virtual-fields
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields) ?clause ...)
       #'(%define-class/sort-clauses
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  ?clause ...))

      ;;Error.
      ((%define-class/sort-clauses/virtual-fields
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(virtual-fields ?field-spec ?field-clause ...) ?clause ...)
       (syntax-violation 'define-class
	 "invalid virtual field specification"
	 (syntax->datum #'?input-form)
	 (syntax->datum #'(virtual-fields ?field-spec  ?field-clause ...))))

      )))


(define-syntax %define-class/sort-clauses/methods
  (lambda (stx)
    (syntax-case stx (parent sealed opaque parent-rtd nongenerative
			     methods predicate setter getter inherit bindings)

      ;;Gather  METHODS   clause  with  explicit   selection  of  method
      ;;function/macro name.
      ((%define-class/sort-clauses/methods
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(methods (?method ?method-name) ?method-clause ...) ?clause ...)
       (and (identifier? #'?method) (identifier? #'?method-name))
       #'(%define-class/sort-clauses/methods
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ... (?method ?method-name))
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (methods ?method-clause ...) ?clause ...))

      ;;Gather  METHODS  clause  with automatically  generated  function
      ;;name.
      ((%define-class/sort-clauses/methods
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(methods (?method) ?method-clause ...) ?clause ...)
       (identifier? #'?method)
       #`(%define-class/sort-clauses/methods
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ... (?method #,(syntax-method-name #'?name #'?method)))
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (methods ?method-clause ...) ?clause ...))

      ;;Gather METHODS clause declared with only the symbol.
      ((%define-class/sort-clauses/methods
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(methods ?method ?method-clause ...) ?clause ...)
       (identifier? #'?method)
       #`(%define-class/sort-clauses/methods
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ... (?method #,(syntax-method-name #'?name #'?method)))
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  (methods ?method-clause ...) ?clause ...))

      ;;Remove empty, leftover, METHODS clause.
      ((%define-class/sort-clauses/methods
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(methods) ?clause ...)
       #'(%define-class/sort-clauses
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...)
	  ?clause ...))

      ;;Error.
      ((%define-class/sort-clauses/methods
	?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(parent		?par ...)
	(inherit	?inh ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...)
	(methods ?method-spec ?method-clause ...) ?clause ...)
       (syntax-violation 'define-class
	 "invalid methods clause"
	 (syntax->datum #'?input-form)
	 (syntax->datum #'(methods ?method-spec ?method-clause ...))))

      )))


(define-syntax %define-class/normalise-inheritance
  ;;Normalise   the  definition  by   processing  INHERIT,   PARENT  and
  ;;PARENT-RTD  clauses;   also  take  care  of   selecting  the  parent
  ;;constructor    descriptors.      Finally    hand    everything    to
  ;;%DEFINE-CLASS/NORMALISE-PREDICATE.
  ;;
  (lambda (stx)
    (syntax-case stx (parent sealed opaque parent-rtd nongenerative
			     predicate setter getter inherit bindings)

;;; --------------------------------------------------------------------
;;; process errors first

      ;;If the class  definition used both INHERIT and  PARENT, raise an
      ;;error.
      ((_ ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?parent0 ?parent ...)
	  (inherit		?inherit0 ?inherit ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?pad ...)
	  (nongenerative	?non ...))
       (syntax-violation 'define-class
	 "both inherit and parent used in class definition"
	 (syntax->datum #'?input-form)))

      ;;If the  class definition used both PARENT  and PARENT-RTD, raise
      ;;an error.
      ((_ ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?parent0 ?parent ...)
	  (inherit		?inh ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?parent-rtd0 ?parent-rtd ...)
	  (nongenerative	?non ...))
       (syntax-violation 'define-class
	 "both parent and parent-rtd used in class definition"
	 (syntax->datum #'?input-form)))

      ;;If the class definition  used both INHERIT and PARENT-RTD, raise
      ;;an error.
      ((_ ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?par ...)
	  (inherit		?inherit0 ?inherit ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?parent-rtd0 ?parent-rtd ...)
	  (nongenerative	?non ...))
       (syntax-violation 'define-class
	 "both inherit and parent-rtd used in class definition"
	 (syntax->datum #'?input-form)))

;;; --------------------------------------------------------------------

      ;;If the class definition used neither the INHERIT clause, nor the
      ;;PARENT clause, nor the  PARENT-RTD clause, make the type derived
      ;;by "<top>".
      ((_ ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent)  ;no parent
	  (inherit) ;no inherit
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd) ;no parent-rtd
	  (nongenerative	?non ...))
       #'(%define-class/normalise-predicate
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (<top>-superclass (record-type-descriptor <top>)
			    (record-constructor-descriptor <top>) ())
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (nongenerative	?non ...)))

;;; --------------------------------------------------------------------

      ;;Process INHERIT clause without inheritance options.
      ((_ ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent) ;no parent
	  (inherit		?superclass-name)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd) ;no parent-rtd
	  (nongenerative	?non ...))
       #`(%define-class/normalise-predicate
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  #,(if (free-identifier=? #'<top> #'?superclass-name)
		#'(<top>-superclass (record-type-descriptor <top>)
				    (record-constructor-descriptor <top>)
				    ())
	      #'(?superclass-name (?superclass-name class-record-type-descriptor)
				  (?superclass-name superclass-constructor-descriptor)
				  ()))
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (nongenerative	?non ...)))

      ;;Process INHERIT clause with inheritance options.
      ((_ ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent) ;no parent
	  (inherit		?superclass-name (?inherit-option ...))
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd) ;no parent-rtd
	  (nongenerative	?non ...))
       (if (all-identifiers? #'(?inherit-option ...))
	   #'(%define-class/normalise-predicate
	      ?input-form (?name ?constructor ?predicate)
	      (?common-protocol ?public-protocol ?superclass-protocol)
	      (?collected-concrete-field ...)
	      (?collected-virtual-field ...)
	      (?collected-method ...)
	      (?collected-definition ...)
	      (?superclass-name (?superclass-name class-record-type-descriptor)
				(?superclass-name superclass-constructor-descriptor)
				(?inherit-option ...))
	      (predicate	?pre ...)
	      (setter		?set ...)
	      (getter		?get ...)
	      (bindings		?bin ...)
	      (sealed		?sea ...)
	      (opaque		?opa ...)
	      (nongenerative	?non ...))
	 (syntax-violation 'define-class
	   "invalid inheritance options in inherit clause of class definition"
	   (syntax->datum #'?input-form)
	   (syntax->datum #'(inherit ?superclass-name (?inherit-option ...))))))

;;; --------------------------------------------------------------------

      ;;Process PARENT clause; use <top> as parent class type.
      ((_ ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent		?parent-name)
	  (inherit) ;no inherit
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd) ;no parent-rtd
	  (nongenerative	?non ...))
       #'(%define-class/normalise-predicate
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (<top>-superclass (record-type-descriptor ?parent-name)
			    (record-constructor-descriptor ?parent-name)
			    ())
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (nongenerative	?non ...)))

;;; --------------------------------------------------------------------

      ;;Process PARENT-RTD clause; use <top> as parent class descriptor.
      ((_ ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (parent)  ;no parent
	  (inherit) ;no inherit
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (parent-rtd		?parent-rtd ?parent-cd)
	  (nongenerative	?non ...))
       #'(%define-class/normalise-predicate
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (<top>-superclass ?parent-rtd ?parent-cd ())
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (nongenerative	?non ...)))

      )))


(define-syntax %define-class/normalise-predicate
  (syntax-rules (sealed opaque nongenerative predicate setter getter bindings)

    ;;No predicate was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	(predicate)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(nongenerative	?non ...))
     (%define-class/normalise-setter
      ?input-form (?name ?constructor ?predicate)
      (?common-protocol ?public-protocol ?superclass-protocol)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
      ?predicate
      (setter		?set ...)
      (getter		?get ...)
      (bindings		?bin ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (nongenerative	?non ...)))

    ;;A predicate was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	(predicate	?predicate-identifier)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(nongenerative	?non ...))
     (%define-class/normalise-setter
      ?input-form (?name ?constructor ?predicate)
      (?common-protocol ?public-protocol ?superclass-protocol)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
      ?predicate-identifier
      (setter		?set ...)
      (getter		?get ...)
      (bindings		?bin ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (nongenerative	?non ...)))
    ))


(define-syntax %define-class/normalise-setter
  (syntax-rules (sealed opaque nongenerative setter getter bindings)

    ;;No setter was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	?predicate-identifier
	(setter)
	(getter		?get ...)
	(bindings	?bin ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(nongenerative	?non ...))
     (%define-class/normalise-getter
      ?input-form (?name ?constructor ?predicate)
      (?common-protocol ?public-protocol ?superclass-protocol)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
      ?predicate-identifier
      #f
      (getter		?get ...)
      (bindings		?bin ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (nongenerative	?non ...)))

    ;;A setter was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	?predicate-identifier
	(setter		?setter)
	(getter		?get ...)
	(bindings	?bin ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(nongenerative	?non ...))
     (%define-class/normalise-getter
      ?input-form (?name ?constructor ?predicate)
      (?common-protocol ?public-protocol ?superclass-protocol)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
      ?predicate-identifier
      ?setter
      (getter		?get ...)
      (bindings		?bin ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (nongenerative	?non ...)))
    ))


(define-syntax %define-class/normalise-getter
  (syntax-rules (sealed opaque nongenerative getter bindings)

    ;;No getter was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	?predicate-identifier
	?setter
	(getter)
	(bindings	?bin ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(nongenerative	?non ...))
     (%define-class/normalise-bindings
      ?input-form (?name ?constructor ?predicate)
      (?common-protocol ?public-protocol ?superclass-protocol)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
      ?predicate-identifier
      ?setter
      #f
      (bindings		?bin ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (nongenerative	?non ...)))

    ;;A getter was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	?predicate-identifier
	?setter
	(getter		?getter)
	(bindings	?bin ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(nongenerative	?non ...))
     (%define-class/normalise-bindings
      ?input-form (?name ?constructor ?predicate)
      (?common-protocol ?public-protocol ?superclass-protocol)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
      ?predicate-identifier
      ?setter
      ?getter
      (bindings		?bin ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (nongenerative	?non ...)))

    ))


(define-syntax %define-class/normalise-bindings
  (lambda (stx)
    (syntax-case stx (sealed opaque nongenerative bindings)

      ;;No BINDINGS was given.
      ((_ ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	  ?predicate-identifier
	  ?setter
	  ?getter
	  (bindings)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (nongenerative	?non ...))
       #'(%define-class/normalise-sealed
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	  ?predicate-identifier
	  ?setter
	  ?getter
	  <top>-bindings
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (nongenerative	?non ...)))

      ;;A BINDINGS was given.
      ((_ ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	  ?predicate-identifier
	  ?setter
	  ?getter
	  (bindings		?bindings-name)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (nongenerative	?non ...))
       #'(%define-class/normalise-sealed
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	  ?predicate-identifier
	  ?setter
	  ?getter
	  ?bindings-name
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (nongenerative	?non ...)))

      ;;Error.
      ((_ ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	  ?predicate-identifier
	  ?setter
	  ?getter
	  (bindings		?bin ...)
	  (sealed		?sea ...)
	  (opaque		?opa ...)
	  (nongenerative	?non ...))
       (syntax-violation 'define-class
	 "invalid bindings clause in class definition"
	 (syntax->datum #'?input-form)
	 (syntax->datum #'(bindings ?bin ...))))

      )))


(define-syntax %define-class/normalise-sealed
  (syntax-rules (sealed opaque nongenerative)

    ;;A SEALED was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	?predicate-identifier
	?setter
	?getter
	?bindings-name
	(sealed		?sealed)
	(opaque		?opa ...)
	(nongenerative	?non ...))
     (%define-class/normalise-opaque
      ?input-form (?name ?constructor ?predicate)
      (?common-protocol ?public-protocol ?superclass-protocol)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
      ?predicate-identifier
      ?setter
      ?getter
      ?bindings-name
      ?sealed
      (opaque		?opa ...)
      (nongenerative	?non ...)))

    ;;No SEALED was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	?predicate-identifier
	?setter
	?getter
	?bindings-name
	(sealed)
	(opaque		?opa ...)
	(nongenerative	?non ...))
     (%define-class/normalise-opaque
      ?input-form (?name ?constructor ?predicate)
      (?common-protocol ?public-protocol ?superclass-protocol)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
      ?predicate-identifier
      ?setter
      ?getter
      ?bindings-name
      #f
      (opaque		?opa ...)
      (nongenerative	?non ...)))
    ))


(define-syntax %define-class/normalise-opaque
  (syntax-rules (opaque nongenerative)

    ;;An OPAQUE was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	?predicate-identifier
	?setter
	?getter
	?bindings-name
	?sealed
	(opaque		?opaque)
	(nongenerative	?non ...))
     (%define-class/normalise-nongenerative
      ?input-form (?name ?constructor ?predicate)
      (?common-protocol ?public-protocol ?superclass-protocol)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
      ?predicate-identifier
      ?setter
      ?getter
      ?bindings-name
      ?sealed
      ?opaque
      (nongenerative	?non ...)))

    ;;No OPAQUE was given.
    ((_ ?input-form (?name ?constructor ?predicate)
	(?common-protocol ?public-protocol ?superclass-protocol)
	(?collected-concrete-field ...)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	?predicate-identifier
	?setter
	?getter
	?bindings-name
	?sealed
	(opaque)
	(nongenerative	?non ...))
     (%define-class/normalise-nongenerative
      ?input-form (?name ?constructor ?predicate)
      (?common-protocol ?public-protocol ?superclass-protocol)
      (?collected-concrete-field ...)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
      ?predicate-identifier
      ?setter
      ?getter
      ?bindings-name
      ?sealed
      #f
      (nongenerative	?non ...)))

    ))


(define-syntax %define-class/normalise-nongenerative
  (lambda (stx)
    (syntax-case stx (nongenerative)

      ;;A NONGENERATIVE was given.
      ((_ ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	  ?predicate-identifier
	  ?setter
	  ?getter
	  ?bindings-name
	  ?sealed
	  ?opaque
	  (nongenerative	?uid))
       #'(%define-class/output-forms
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	  ?predicate-identifier
	  ?setter
	  ?getter
	  ?bindings-name
	  ?sealed
	  ?opaque
	  ?uid))

      ;;No NONGENERATIVE was given.
      ((_ ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	  ?predicate-identifier
	  ?setter
	  ?getter
	  ?bindings-name
	  ?sealed
	  ?opaque
	  (nongenerative))
       #`(%define-class/output-forms
	  ?input-form (?name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  (?collected-concrete-field ...)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	  ?predicate-identifier
	  ?setter
	  ?getter
	  ?bindings-name
	  ?sealed
	  ?opaque
	  ;;We have to  do this to properly generated  a unique UID.  We
	  ;;cannot rely  on the  expander renaming a  raw symbol  we may
	  ;;introduce here.
	  #,(datum->syntax #'?name (gensym))))

      )))


(define-syntax %define-class/output-forms
  ;;Generate the final output forms for class definition.
  ;;
  ;;Notice  that  for  the  class-specific  definitions  we  select  the
  ;;identifiers  THE-RTD,   THE-CD,  WITH-CLASS-BINDINGS  and   rely  on
  ;;automatic  renaming  of  introduced  bindings;  it  is  tempting  to
  ;;generate names having the class  name as substring (which would show
  ;;in debugging stack traces), but  it would pollute the environment of
  ;;the output form generating identifier collisions.
  ;;
  (lambda (stx)
    (define (generate-field-indexes list-of-fields/stx)
      ;;Derived  from   IOTA  from  (lists);  generate   a  sequence  of
      ;;non-negative exact integers to be  used as indexes in the vector
      ;;of concrete fields.
      ;;
      (let ((count (length (syntax->datum list-of-fields/stx))))
	(do ((count count (- count 1))
	     (val (- count 1) (- val 1))
	     (ret '() (cons val ret)))
	    ((<= count 0)
	     ret))))

    (syntax-case stx ()

      ((_ ?input-form
	  (?class-name ?constructor ?predicate)
	  (?common-protocol ?public-protocol ?superclass-protocol)
	  ((?mutability ?field ?accessor ...) ...)
	  ((?virtual-mutability ?virtual-field ?virtual-accessor ...) ...)
	  ((?method ?method-function) ...)
	  (?collected-definition ...)
	  (?superclass-name ?parent-rtd ?parent-cd ?inherit-options)
	  ?predicate-identifier ?setter ?getter ?bindings-name
	  ?sealed ?opaque ?uid)
       (let ((id (duplicated-identifiers? #'(?field ... ?virtual-field ... ?method ...))))
	 (if id
	     (syntax-violation 'define-class
	       "duplicated field names in class definition"
	       (syntax->datum #'?input-form)
	       (syntax->datum id))
	   (let ((name (syntax->datum #'?class-name)))
	     (with-syntax (((INHERIT-CONCRETE-FIELDS? INHERIT-VIRTUAL-FIELDS?
						      INHERIT-METHODS? INHERIT-SETTER-AND-GETTER?)
			    (datum->syntax #'?name
					   (%parse-inherit-options #'?inherit-options #'?input-form)))
			   ((FIELD-INDEXES ...)
			    (datum->syntax #'?class-name (generate-field-indexes #'(?field ...)))))
	       #'(begin
		   (define the-parent-rtd
		     ?parent-rtd)

		   (define the-rtd
		     (make-record-type-descriptor (quote ?class-name) the-parent-rtd
						  (quote ?uid) ?sealed ?opaque
						  (quote #((?mutability ?field) ...))))

		   (define the-common-protocol     ?common-protocol)
		   (define the-public-protocol     (or ?public-protocol     the-common-protocol))
		   (define the-superclass-protocol (or ?superclass-protocol the-common-protocol))

		   (define the-from-fields-cd
		     (%make-from-fields-cd the-rtd))

		   ;;Construction   protocol  used  when   invoking  the
		   ;;constructor explicitly through MAKE.
		   (define the-public-cd
		     (make-record-constructor-descriptor the-rtd ?parent-cd the-public-protocol))

		   ;;Construction   protocol  used  when   invoking  the
		   ;;constructor from a subclass constructor.
		   (define the-superclass-cd
		     (make-record-constructor-descriptor the-rtd ?parent-cd the-superclass-protocol))

		   (define ?constructor			(record-constructor the-public-cd))
		   (define superclass-constructor	(record-constructor the-superclass-cd))
		   (define from-fields-constructor	(record-constructor the-from-fields-cd))

		   (define ?predicate (record-predicate the-rtd))

		   (define the-parent-uid-list
		     (cons (quote ?uid) (if the-parent-rtd
					    (map record-type-uid (record-parent-list the-parent-rtd))
					  '())))

		   (define (the-parent-rtd-list)
		     (cons the-rtd (if the-parent-rtd
				       (record-parent-list the-parent-rtd)
				     '())))

		   (%define-class/output-forms/fields-accessors-and-mutators
		    the-rtd (FIELD-INDEXES ...) (?mutability ?field ?accessor ...) ...)

		   ;;These are the definitions of in-definition methods.
		   ?collected-definition ...

		   (define-syntax ?class-name
		     (lambda (stx)
		       (syntax-case stx (class-record-type-descriptor
					 class-type-uid
					 class-uid-list
					 public-constructor-descriptor
					 superclass-constructor-descriptor
					 from-fields-constructor-descriptor
					 parent-rtd-list
					 make make-from-fields is-a?
					 with-class-bindings-of)

			 ((_ class-record-type-descriptor)
			  #'(begin the-rtd))

			 ((_ class-type-uid)
			  #'(quote ?uid))

			 ((_ class-uid-list)
			  #'the-parent-uid-list)

			 ((_ public-constructor-descriptor)
			  #'(begin the-public-cd))

			 ((_ superclass-constructor-descriptor)
			  #'(begin the-superclass-cd))

			 ((_ from-fields-constructor-descriptor)
			  #'(begin the-from-fields-cd))

			 ((_ parent-rtd-list)
			  #'(the-parent-rtd-list))

			 ((_ make ?arg (... ...))
			  #'(?constructor ?arg (... ...)))

			 ((_ make-from-fields ?arg (... ...))
			  #'(from-fields-constructor ?arg (... ...)))

			 ((_ is-a? ?arg)
			  #'(?predicate-identifier ?arg))

			 ((_ with-class-bindings-of
			     (?inherit-concrete-fields
			      ?inherit-virtual-fields
			      ?inherit-methods
			      ?inherit-setter-and-getter)
			     ?arg (... ...))
			  (for-all boolean? (syntax->datum #'(?inherit-concrete-fields
							      ?inherit-virtual-fields
							      ?inherit-methods
							      ?inherit-setter-and-getter)))
			  #'(with-class-bindings
			     (?inherit-concrete-fields
			      ?inherit-virtual-fields
			      ?inherit-methods
			      ?inherit-setter-and-getter)
			     ?arg (... ...)))

			 ((_ ?keyword . ?rest)
			  (syntax-violation '?class-name
			    "invalid class internal keyword"
			    (syntax->datum #'(?class-name ?keyword . ?rest))
			    (syntax->datum #'?keyword)))
			 )))

		   (define-syntax with-class-bindings
		     (syntax-rules ()
		       ((_ (?inherit-concrete-fields
			    ?inherit-virtual-fields
			    ?inherit-methods
			    ?inherit-setter-and-getter)
			   ?variable-name ?body0 ?body (... ...))
			(?superclass-name
			 with-class-bindings-of (INHERIT-CONCRETE-FIELDS?
						 INHERIT-VIRTUAL-FIELDS?
						 INHERIT-METHODS?
						 INHERIT-SETTER-AND-GETTER?)
			 ?variable-name
			 (with-class-bindings/concrete-fields
			  ?inherit-concrete-fields ?variable-name
			  (with-class-bindings/virtual-fields
			   ?inherit-virtual-fields ?variable-name
			   (with-class-bindings/methods
			    ?inherit-methods ?variable-name
			    (with-class-bindings/setter-and-getter
			     ?inherit-setter-and-getter ?variable-name
			     (?bindings-name ?class-name ?variable-name
					     ?body0 ?body (... ...))))))))
		       ))

		   (define-syntax with-class-bindings/concrete-fields
		     (lambda (stx)
		       (syntax-case stx ()
			 ((_ ?inherit-concrete-fields ?variable-name . ?body)
			  (syntax->datum #'?inherit-concrete-fields)
			  #'(%with-class-fields
			     ?variable-name
			     ((?mutability ?field ?accessor ...) ...)
			     . ?body))
			 ((_ ?inherit-fields ?variable-name . ?body)
			  #'(begin . ?body))
			 )))

		   (define-syntax with-class-bindings/virtual-fields
		     (lambda (stx)
		       (syntax-case stx ()
			 ((_ ?inherit-virtual-fields ?variable-name . ?body)
			  (syntax->datum #'?inherit-virtual-fields)
			  #'(%with-class-fields
			     ?variable-name
			     ((?virtual-mutability ?virtual-field ?virtual-accessor ...) ...)
			     . ?body))
			 ((_ ?inherit-fields ?variable-name . ?body)
			  #'(begin . ?body))
			 )))

		   (define-syntax with-class-bindings/methods
		     (lambda (stx)
		       (syntax-case stx ()
			 ((_ ?inherit-methods ?variable-name . ?body)
			  (syntax->datum #'?inherit-methods)
			  #'(%with-class-methods ?variable-name ((?method ?method-function) ...)
						 . ?body))
			 ((_ ?inherit-methods ?variable-name . ?body)
			  #'(begin . ?body))
			 )))

		   (define-syntax with-class-bindings/setter-and-getter
		     (lambda (stx)
		       (syntax-case stx ()
			 ((_ ?inherit-setter-and-getter ?variable-name . ?body)
			  (syntax->datum #'?inherit-setter-and-getter)
			  #'(%with-class-setter-and-getter ?variable-name ?setter ?getter . ?body))
			 ((_ ?inherit-setter-and-getter ?variable-name . ?body)
			  #'(begin . ?body))
			 )))

		   )))
	   )))
      )))


(define-syntax %define-class/output-forms/fields-accessors-and-mutators
  ;;Subroutine  of  %DEFINE-CLASS/OUTPUT-FORMS   which  expands  to  the
  ;;definitions of the class' concrete field accessors and mutators.
  ;;
  (syntax-rules ()

    ;;No fields.
    ((_ ?rtd ())
     (define dummy #f))

    ;;Process last field as mutable.
    ((_ ?rtd (?field-index) (mutable ?field ?accessor ?mutator))
     (begin
       (define ?accessor  (record-accessor ?rtd ?field-index))
       (define ?mutator   (record-mutator  ?rtd ?field-index))))

    ;;Process last field as immutable.
    ((_ ?rtd (?field-index) (immutable ?field ?accessor))
     (define ?accessor    (record-accessor ?rtd ?field-index)))

    ;;Process next field as mutable.
    ((_ ?rtd (?next-field-index ?field-index ...) (mutable ?field ?accessor ?mutator) ?clause ...)
     (begin
       (define ?accessor  (record-accessor ?rtd ?next-field-index))
       (define ?mutator   (record-mutator  ?rtd ?next-field-index))
       (%define-class/output-forms/fields-accessors-and-mutators ?rtd (?field-index ...) ?clause ...)))

    ;;Process next field as immutable.
    ((_ ?rtd (?next-field-index ?field-index ...) (immutable ?field ?accessor) ?clause ...)
     (begin
       (define ?accessor  (record-accessor ?rtd ?next-field-index))
       (%define-class/output-forms/fields-accessors-and-mutators ?rtd (?field-index ...) ?clause ...)))

    ))


(define-syntax define-label
  ;;A label is just a tag we slap on any value to use virtual fields and
  ;;methods with dot notation, it  has NO record type.  Labels canNOT be
  ;;used in the inheritance hierarchy of classes.
  ;;
  (lambda (stx)
    (syntax-case stx (fields inherit predicate virtual-fields methods method setter getter bindings)

      ((_ (?name ?predicate) ?clause ...)
       (all-identifiers? #'(?name ?predicate))
       #'(%define-label/sort-clauses
	  (define-label (?name ?predicate) ?clause ...)
	  (?name ?predicate)
	  ()		;collected virtual fields
	  ()		;collected methods
	  ()		;collected definitions
	  (predicate) (setter) (getter) (bindings) (inherit)
	  ?clause ...))

      ((_ ?name ?clause ...)
       (identifier? (syntax ?name))
       #`(%define-label/sort-clauses
	  (define-label ?name ?clause ...)
	  (?name #,(syntax-suffix #'?name "?"))
	  ()		;collected virtual fields
	  ()		;collected methods
	  ()		;collected definitions
	  (predicate) (setter) (getter) (bindings) (inherit)
	  ?clause ...))

      ((_ ?name-spec . ?clauses)
       (syntax-violation 'define-label
	 "invalid name specification in label definition"
	 (syntax->datum (syntax ?input-form))
	 (syntax->datum (syntax ?name-spec))))
      )))


(define-syntax %define-label/sort-clauses
  ;;Sorts all  the auxiliary  syntaxes.  Collects the  specifications of
  ;;virtual fields; expands the field clauses given with no accessor and
  ;;mutator   names  to   clauses  with   accessor  and   mutator  names
  ;;automatically built from the record  type name (as defined by R6RS).
  ;;Collects  the method clauses,  expanding the  ones with  no function
  ;;name  to clauses  with function  name automatically  built  from the
  ;;record type name.
  ;;
  (lambda (stx)
    (define (%sinner msg input-form subform)
      (syntax-violation 'define-label msg (syntax->datum input-form) (syntax->datum subform)))

    (syntax-case stx (fields virtual-fields methods method method-syntax
			     predicate setter getter inherit bindings)

      ;;Gather the INHERIT clause.
      ((%define-label/sort-clauses
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	. ?inherit-rest)
	(inherit ?superlabel-name . ?inherit-clauses) ?clause ...)
       (let ((form	(syntax ?input-form))
	     (subform	(syntax (inherit ?superlabel-name))))
	 (cond ((not (null? (syntax->datum (syntax ?inherit-rest))))
		(%sinner "inherit clause given twice in label definition" form subform))
	       ((not (identifier? (syntax ?superlabel-name)))
		(%sinner "invalid inherit clause in label definition" form subform))
	       (else
		#'(%define-label/sort-clauses
		   ?input-form (?name ?predicate)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-definition ...)
		   (predicate	?pre ...)
		   (setter	?set ...)
		   (getter	?get ...)
		   (bindings	?bin ...)
		   (inherit	?superlabel-name . ?inherit-clauses)
		   ?clause ...)))))

      ;;Gather the PREDICATE clause.
      ((%define-label/sort-clauses
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	. ?predicate-rest)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(predicate ?function-name) ?clause ...)
       (let ((form	(syntax ?input-form))
	     (subform	(syntax (predicate ?function-name))))
	 (cond ((not (null? (syntax->datum (syntax ?predicate-rest))))
		(%sinner "predicate clause given twice in label definition" form subform))
	       ((not (identifier? (syntax ?function-name)))
		(%sinner "invalid predicate clause in label definition" form subform))
	       (else
		#'(%define-label/sort-clauses
		   ?input-form (?name ?predicate)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-definition ...)
		   (predicate	?function-name)
		   (setter	?set ...)
		   (getter	?get ...)
		   (bindings	?bin ...)
		   (inherit	?inh ...)
		   ?clause ...)))))

      ;;Gather the SETTER clause.
      ((%define-label/sort-clauses
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		. ?setter-rest)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(setter ?setter) ?clause ...)
       (let ((form	(syntax ?input-form))
	     (subform	(syntax (setter ?setter))))
	 (cond ((not (null? (syntax->datum (syntax ?setter-rest))))
		(%sinner "setter clause given twice in label definition" form subform))
	       ((not (identifier? (syntax ?setter)))
		(%sinner "invalid setter clause in label definition" form subform))
	       (else
		#'(%define-label/sort-clauses
		   ?input-form (?name ?predicate)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-definition ...)
		   (predicate	?pre ...)
		   (setter	?setter)
		   (getter	?get ...)
		   (bindings	?bin ...)
		   (inherit	?inh ...)
		   ?clause ...)))))

      ;;Gather the GETTER clause.
      ((%define-label/sort-clauses
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		. ?getter-rest)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(getter ?getter) ?clause ...)
       (let ((form	(syntax ?input-form))
	     (subform	(syntax (getter ?getter))))
	 (cond ((not (null? (syntax->datum (syntax ?getter-rest))))
		(%sinner "getter clause given twice in label definition" form subform))
	       ((not (identifier? (syntax ?getter)))
		(%sinner "invalid getter clause in label definition" form subform))
	       (else
		#'(%define-label/sort-clauses
		   ?input-form (?name ?predicate)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-definition ...)
		   (predicate	?pre ...)
		   (setter	?set ...)
		   (getter	?getter)
		   (bindings	?bin ...)
		   (inherit	?inh ...)
		   ?clause ...)))))

;;; --------------------------------------------------------------------

      ;;Gather the BINDINGS clause.
      ((%define-label/sort-clauses
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	. ?bindings-rest)
	(inherit	?inh ...)
	(bindings ?macro-name) ?clause ...)
       (let ((form	(syntax ?input-form))
	     (subform	(syntax (bindings ?macro-name))))
	 (cond ((not (null? (syntax->datum (syntax ?bindings-rest))))
		(%sinner "bindings clause given twice in label definition" form subform))
	       ((not (identifier? (syntax ?macro-name)))
		(%sinner "invalid bindings clause in label definition" form subform))
	       (else
		#'(%define-label/sort-clauses
		   ?input-form (?name ?predicate)
		   (?collected-virtual-field ...)
		   (?collected-method ...)
		   (?collected-definition ...)
		   (predicate	?pre ...)
		   (setter	?set ...)
		   (getter	?get ...)
		   (bindings	?macro-name)
		   (inherit	?inh ...)
		   ?clause ...)))))

;;; --------------------------------------------------------------------

      ;;Gather   the    VIRTUAL-FIELDS   clause.    Notice    that   the
      ;;VIRTUAL-FIELDS clause can be used multiple times.
      ((%define-label/sort-clauses
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(virtual-fields ?field-clause ...) ?clause ...)
       #'(%define-label/sort-clauses/virtual-fields
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate	?pre ...)
	  (setter	?set ...)
	  (getter	?get ...)
	  (bindings	?bin ...)
	  (inherit	?inh ...)
	  (virtual-fields ?field-clause ...) ?clause ...))

;;; --------------------------------------------------------------------

      ;;Gather METHODS  clause.  Notice that  the METHODS clause  can be
      ;;used multiple times.
      ((%define-label/sort-clauses
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(methods ?method-clause ...) ?clause ...)
       #'(%define-label/sort-clauses/methods
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate	?pre ...)
	  (setter	?set ...)
	  (getter	?get ...)
	  (bindings	?bin ...)
	  (inherit	?inh ...)
	  (methods ?method-clause ...) ?clause ...))

;;; --------------------------------------------------------------------

      ;;Gather a METHOD clause with define-like function definition.
      ((%define-label/sort-clauses
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(method (?method . ?args) . ?body) ?clause ...)
       (identifier? #'?method)
       #'(%define-label/sort-clauses
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ... (?method function-name))
	  (?collected-definition ... (define/with-class (function-name . ?args) . ?body))
	  (predicate	?pre ...)
	  (setter	?set ...)
	  (getter	?get ...)
	  (bindings	?bin ...)
	  (inherit	?inh ...)
	  ?clause ...))

      ;;Gather a METHOD clause with expression function definition.
      ((%define-label/sort-clauses
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(method ?method ?expression) ?clause ...)
       (identifier? #'?method)
       #'(%define-label/sort-clauses
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ... (?method function-name))
	  (?collected-definition ... (define function-name ?expression))
	  (predicate	?pre ...)
	  (setter	?set ...)
	  (getter	?get ...)
	  (bindings	?bin ...)
	  (inherit	?inh ...)
	  ?clause ...))

;;; --------------------------------------------------------------------

      ;;Gather a METHOD-SYNTAX clause.
      ((%define-label/sort-clauses
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(method-syntax ?method ?transformer) ?clause ...)
       (identifier? #'?method)
       #'(%define-label/sort-clauses
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ... (?method macro-name))
	  (?collected-definition ... (define-syntax macro-name ?transformer))
	  (predicate	?pre ...)
	  (setter	?set ...)
	  (getter	?get ...)
	  (bindings	?bin ...)
	  (inherit	?inh ...)
	  ?clause ...))

;;; --------------------------------------------------------------------

      ;;No    more   clauses    to   gather.     Hand    everything   to
      ;;%DEFINE-LABEL/NORMALISE-INHERITANCE.
      ;;
      ((_ ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate	?pre ...)
	  (setter	?set ...)
	  (getter	?get ...)
	  (bindings	?bin ...)
	  (inherit	?inh ...))
       #'(%define-label/normalise-inheritance
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate	?pre ...)
	  (setter	?set ...)
	  (getter	?get ...)
	  (bindings	?bin ...)
	  (inherit	?inh ...)))

      ((_ ?input-form . ?stuff)
       (syntax-violation 'define-label
	 "invalid label definition"
	 (syntax->datum (syntax ?input-form))))

      )))


(define-syntax %define-label/sort-clauses/virtual-fields
  (lambda (stx)
    (syntax-case stx (mutable immutable virtual-fields predicate setter getter inherit bindings)

      ;;Gather mutable VIRTUAL-FIELDS  clause with explicit selection of
      ;;accessor and mutator names.
      ((%define-label/sort-clauses/virtual-fields
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(virtual-fields (mutable ?field ?accessor ?mutator) ?field-clause ...) ?clause ...)
       (all-identifiers? #'(?field ?accessor ?mutator))
       #'(%define-label/sort-clauses/virtual-fields
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ...  (mutable ?field ?accessor ?mutator))
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate	?pre ...)
	  (setter	?set ...)
	  (getter	?get ...)
	  (bindings	?bin ...)
	  (inherit	?inh ...)
	  (virtual-fields ?field-clause ...) ?clause ...))

      ;;Gather   mutable   VIRTUAL-FIELDS   clause  with   automatically
      ;;generated accessor and mutator names.
      ((%define-label/sort-clauses/virtual-fields
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(virtual-fields (mutable ?field) ?field-clause ...) ?clause ...)
       (identifier? #'?field)
       #`(%define-label/sort-clauses/virtual-fields
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ...  (mutable ?field
						  #,(syntax-accessor-name #'?name #'?field)
						  #,(syntax-mutator-name  #'?name #'?field)))
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate	?pre ...)
	  (setter	?set ...)
	  (getter	?get ...)
	  (bindings	?bin ...)
	  (inherit	?inh ...)
	  (virtual-fields ?field-clause ...) ?clause ...))

;;; --------------------------------------------------------------------

      ;;Gather immutable  VIRTUAL-FIELDS clause with  explicit selection
      ;;of accessor name.
      ((%define-label/sort-clauses/virtual-fields
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(virtual-fields (immutable ?field ?accessor) ?field-clause ...) ?clause ...)
       (all-identifiers? #'(?field ?accessor))
       #'(%define-label/sort-clauses/virtual-fields
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ... (immutable ?field ?accessor))
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate	?pre ...)
	  (setter	?set ...)
	  (getter	?get ...)
	  (bindings	?bin ...)
	  (inherit	?inh ...)
	  (virtual-fields ?field-clause ...) ?clause ...))

      ;;Gather   immutable  VIRTUAL-FIELDS  clause   with  automatically
      ;;generated accessor name.
      ((%define-label/sort-clauses/virtual-fields
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(virtual-fields (immutable ?field) ?field-clause ...) ?clause ...)
       (identifier? #'?field)
       #`(%define-label/sort-clauses/virtual-fields
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ... (immutable ?field #,(syntax-accessor-name #'?name #'?field)))
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate	?pre ...)
	  (setter	?set ...)
	  (getter	?get ...)
	  (bindings	?bin ...)
	  (inherit	?inh ...)
	  (virtual-fields ?field-clause ...) ?clause ...))

      ;;Gather immutable VIRTUAL-FIELDS  clause declared without IMMUTABLE
      ;;auxiliary syntax.
      ((%define-label/sort-clauses/virtual-fields
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(virtual-fields ?field ?field-clause ...) ?clause ...)
       (identifier? #'?field)
       #`(%define-label/sort-clauses/virtual-fields
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ... (immutable ?field #,(syntax-accessor-name #'?name #'?field)))
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate	?pre ...)
	  (setter	?set ...)
	  (getter	?get ...)
	  (bindings	?bin ...)
	  (inherit	?inh ...)
	  (virtual-fields ?field-clause ...) ?clause ...))

;;; --------------------------------------------------------------------

      ;;Remove empty, leftover, VIRTUAL-FIELDS clause.
      ((%define-label/sort-clauses/virtual-fields
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(virtual-fields) ?clause ...)
       #'(%define-label/sort-clauses
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate	?pre ...)
	  (setter	?set ...)
	  (getter	?get ...)
	  (bindings	?bin ...)
	  (inherit	?inh ...)
	  ?clause ...))

      ;;Error.
      ((%define-label/sort-clauses/virtual-fields
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(virtual-fields ?field-spec ?field-clause ...) ?clause ...)
       (syntax-violation 'define-label
	 "invalid virtual field specification"
	 (syntax->datum #'?input-form)
	 (syntax->datum #'(virtual-fields ?field-spec  ?field-clause ...))))

      )))


(define-syntax %define-label/sort-clauses/methods
  (lambda (stx)
    (syntax-case stx (mutable immutable methods predicate setter getter inherit bindings)

      ;;Gather  METHODS   clause  with  explicit   selection  of  method
      ;;function/macro name.
      ((%define-label/sort-clauses/methods
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(methods (?method ?method-name) ?method-clause ...) ?clause ...)
       (and (identifier? #'?method) (identifier? #'?method-name))
       #'(%define-label/sort-clauses/methods
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ... (?method ?method-name))
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (inherit		?inh ...)
	  (methods ?method-clause ...) ?clause ...))

      ;;Gather  METHODS  clause  with automatically  generated  function
      ;;name.
      ((%define-label/sort-clauses/methods
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(methods (?method) ?method-clause ...) ?clause ...)
       (identifier? #'?method)
       #`(%define-label/sort-clauses/methods
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ... (?method #,(syntax-method-name #'?name #'?method)))
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (inherit		?inh ...)
	  (methods ?method-clause ...) ?clause ...))

      ;;Gather METHODS clause declared with only the symbol.
      ((%define-label/sort-clauses/methods
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(methods ?method ?method-clause ...) ?clause ...)
       (identifier? #'?method)
       #`(%define-label/sort-clauses/methods
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ... (?method #,(syntax-method-name #'?name #'?method)))
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (inherit		?inh ...)
	  (methods ?method-clause ...) ?clause ...))

      ;;Remove empty, leftover, METHODS clause.
      ((%define-label/sort-clauses/methods
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(methods) ?clause ...)
       #'(%define-label/sort-clauses
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (inherit		?inh ...)
	  ?clause ...))

      ;;Error.
      ((%define-label/sort-clauses/methods
	?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(predicate	?pre ...)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...)
	(inherit	?inh ...)
	(methods ?method-spec ?method-clause ...) ?clause ...)
       (syntax-violation 'define-label
	 "invalid methods clause"
	 (syntax->datum #'?input-form)
	 (syntax->datum #'(methods ?method-spec ?method-clause ...))))

      )))


(define-syntax %define-label/normalise-inheritance
  ;;Normalise  the  definition  by  processing  INHERIT.   Finally  hand
  ;;everything to %DEFINE-LABEL/NORMALISE-PREDICATE.
  ;;
  (lambda (stx)
    (syntax-case stx (predicate setter getter inherit bindings)

      ;;If the  label definition used  no INHERIT clause, make  the type
      ;;derived by "<top>-label".
      ((_ ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate	?pre ...)
	  (setter	?set ...)
	  (getter	?get ...)
	  (bindings	?bin ...)
	  (inherit)) ;no inherit
       #'(%define-label/normalise-predicate
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (<top>-label ())
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)))

;;; --------------------------------------------------------------------

      ;;Process INHERIT clause without inheritance options.
      ((_ ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate		?pre ...)
	  (setter		?set ...)
	  (getter		?get ...)
	  (bindings		?bin ...)
	  (inherit		?superlabel-name))
       #`(%define-label/normalise-predicate
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  #,(if (free-identifier=? #'<top> #'?superlabel-name)
		#'(<top>-label ())
	      #'(?superlabel-name ()))
	  (predicate	?pre ...)
	  (setter	?set ...)
	  (getter	?get ...)
	  (bindings	?bin ...)))

      ;;Process INHERIT clause with inheritance options.
      ((_ ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (predicate	?pre ...)
	  (setter	?set ...)
	  (getter	?get ...)
	  (bindings	?bin ...)
	  (inherit	?superlabel-name (?inherit-option ...)))
       (if (all-identifiers? #'(?inherit-option ...))
	   #'(%define-label/normalise-predicate
	      ?input-form (?name ?predicate)
	      (?collected-virtual-field ...)
	      (?collected-method ...)
	      (?collected-definition ...)
	      #,(if (free-identifier=? #'<top> #'?superlabel-name)
		    #'(<top>-label (?inherit-option ...))
		  #'(?superlabel-name (?inherit-option ...)))
	      (predicate	?pre ...)
	      (setter		?set ...)
	      (getter		?get ...)
	      (bindings		?bin ...))
	 (syntax-violation 'define-label
	   "invalid inheritance options in inherit clause of label definition"
	   (syntax->datum #'?input-form)
	   (syntax->datum #'(inherit ?superlabel-name (?inherit-option ...))))))

      )))


(define-syntax %define-label/normalise-predicate
  (syntax-rules (predicate setter getter bindings)

    ;;No predicate was given.
    ((_ ?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(?superlabel-name ?inherit-options)
	(predicate)
	(setter	?set ...)
	(getter	?get ...)
	(bindings	?bin ...))
     (%define-label/normalise-setter
      ?input-form (?name ?predicate)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      (?superlabel-name ?inherit-options)
      #f
      (setter		?set ...)
      (getter		?get ...)
      (bindings		?bin ...)))

    ;;A predicate was given.
    ((_ ?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(?superlabel-name ?inherit-options)
	(predicate	?predicate-identifier)
	(setter		?set ...)
	(getter		?get ...)
	(bindings	?bin ...))
     (%define-label/normalise-setter
      ?input-form (?name ?predicate)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      (?superlabel-name ?inherit-options)
      ?predicate-identifier
      (setter		?set ...)
      (getter		?get ...)
      (bindings		?bin ...)))

    ))


(define-syntax %define-label/normalise-setter
  (syntax-rules (setter getter bindings)

    ;;No setter was given.
    ((_ ?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(?superlabel-name ?inherit-options)
	?predicate-identifier
	(setter)
	(getter		?get ...)
	(bindings	?bin ...))
     (%define-label/normalise-getter
      ?input-form (?name ?predicate)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      (?superlabel-name ?inherit-options)
      ?predicate-identifier
      #f
      (getter		?get ...)
      (bindings		?bin ...)))

    ;;A setter was given.
    ((_ ?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(?superlabel-name ?inherit-options)
	?predicate-identifier
	(setter		?setter)
	(getter		?get ...)
	(bindings	?bin ...))
     (%define-label/normalise-getter
      ?input-form (?name ?predicate)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      (?superlabel-name ?inherit-options)
      ?predicate-identifier
      ?setter
      (getter		?get ...)
      (bindings		?bin ...)))

    ))


(define-syntax %define-label/normalise-getter
  (syntax-rules (getter bindings)

    ;;No getter was given.
    ((_ ?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(?superlabel-name ?inherit-options)
	?predicate-identifier
	?setter
	(getter)
	(bindings	?bin ...))
     (%define-label/normalise-bindings
      ?input-form (?name ?predicate)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      (?superlabel-name ?inherit-options)
      ?predicate-identifier
      ?setter
      #f
      (bindings		?bin ...)))

    ;;A getter was given.
    ((_ ?input-form (?name ?predicate)
	(?collected-virtual-field ...)
	(?collected-method ...)
	(?collected-definition ...)
	(?superlabel-name ?inherit-options)
	?predicate-identifier
	?setter
	(getter		?getter)
	(bindings	?bin ...))
     (%define-label/normalise-bindings
      ?input-form (?name ?predicate)
      (?collected-virtual-field ...)
      (?collected-method ...)
      (?collected-definition ...)
      (?superlabel-name ?inherit-options)
      ?predicate-identifier
      ?setter
      ?getter
      (bindings		?bin ...)))

    ))


(define-syntax %define-label/normalise-bindings
  (lambda (stx)
    (syntax-case stx (bindings)

      ;;No BINDINGS was given.
      ((_ ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (?superlabel-name ?inherit-options)
	  ?predicate-identifier
	  ?setter
	  ?getter
	  (bindings))
       #'(%define-label/output-forms
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (?superlabel-name ?inherit-options)
	  ?predicate-identifier
	  ?setter
	  ?getter
	  <top>-bindings))

      ;;A BINDINGS was given.
      ((_ ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (?superlabel-name ?inherit-options)
	  ?predicate-identifier
	  ?setter
	  ?getter
	  (bindings	?bindings-name))
       #'(%define-label/output-forms
	  ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (?superlabel-name ?inherit-options)
	  ?predicate-identifier
	  ?setter
	  ?getter
	  ?bindings-name))

      ;;Error.
      ((_ ?input-form (?name ?predicate)
	  (?collected-virtual-field ...)
	  (?collected-method ...)
	  (?collected-definition ...)
	  (?superlabel-name ?inherit-options)
	  ?predicate-identifier
	  ?setter
	  ?getter
	  (bindings	?bin ...))
       (syntax-violation 'define-label
	 "invalid bindings clause in label definition"
	 (syntax->datum #'?input-form)
	 (syntax->datum #'(bindings ?bin ...))))

      )))


(define-syntax %define-label/output-forms
  ;;Generate the final output forms for label definition.
  ;;
  (lambda (stx)
    (syntax-case stx ()

      ((_ ?input-form (?label-name ?predicate)
	  ((?virtual-mutability ?virtual-field ?virtual-accessor ...) ...)
	  ((?method ?method-function) ...)
	  (?collected-definition ...)
	  (?superlabel-name ?inherit-options)
	  ?predicate-identifier ?setter ?getter ?bindings-name)
       (let ((id (duplicated-identifiers? #'(?virtual-field ... ?method ...))))
	 (if id
	     (syntax-violation 'define-label
	       "duplicated field names in label definition"
	       (syntax->datum #'?input-form)
	       (syntax->datum id))
	   (let ((name (syntax->datum #'?label-name)))
	     (with-syntax (((INHERIT-VIRTUAL-FIELDS? INHERIT-METHODS? INHERIT-SETTER-AND-GETTER?)
			    (datum->syntax #'?name
					   (%parse-label-inherit-options #'?inherit-options
									 #'?input-form))))
	       #'(begin
		   (define ?predicate
		     (let ((p ?predicate-identifier))
		       (or p (lambda (x)
			       (assertion-violation (quote ?predicate)
				 "no predicate definition for label"
				 (quote ?input-form))))))

		   ;;These are the definitions of in-definition methods.
		   ?collected-definition ...

		   (define-syntax ?label-name
		     (lambda (stx)
		       (syntax-case stx (is-a? with-class-bindings-of)

			 ((_ is-a? ?arg)
			  #'(?predicate-identifier ?arg))

			 ((_ with-class-bindings-of
			     (?inherit-concrete-fields	;this comes from WITH-CLASS
			      ?inherit-virtual-fields
			      ?inherit-methods
			      ?inherit-setter-and-getter)
			     ?arg (... ...))
			  (for-all boolean? (syntax->datum #'(?inherit-concrete-fields
							      ?inherit-virtual-fields
							      ?inherit-methods
							      ?inherit-setter-and-getter)))
			  #'(with-label-bindings
			     (?inherit-virtual-fields
			      ?inherit-methods
			      ?inherit-setter-and-getter)
			     ?arg (... ...)))

			 ((_ ?keyword . ?rest)
			  (syntax-violation '?label-name
			    "invalid label internal keyword"
			    (syntax->datum #'(?label-name ?keyword . ?rest))
			    (syntax->datum #'?keyword)))
			 )))

		   (define-syntax with-label-bindings
		     (syntax-rules ()
		       ((_ (?inherit-virtual-fields ?inherit-methods ?inherit-setter-and-getter)
			   ?variable-name ?body0 ?body (... ...))
			(?superlabel-name
			 with-class-bindings-of (#f
						 INHERIT-VIRTUAL-FIELDS?
						 INHERIT-METHODS?
						 INHERIT-SETTER-AND-GETTER?)
			 ?variable-name
			 (with-label-bindings/virtual-fields
			  ?inherit-virtual-fields ?variable-name
			  (with-label-bindings/methods
			   ?inherit-methods ?variable-name
			   (with-label-bindings/setter-and-getter
			    ?inherit-setter-and-getter ?variable-name
			    (?bindings-name ?label-name ?variable-name
					    ?body0 ?body (... ...)))))))
		       ))

		   (define-syntax with-label-bindings/virtual-fields
		     (lambda (stx)
		       (syntax-case stx ()
			 ((_ ?inherit-virtual-fields ?variable-name . ?body)
			  (syntax->datum #'?inherit-virtual-fields)
			  #'(%with-class-fields
			     ?variable-name
			     ((?virtual-mutability ?virtual-field ?virtual-accessor ...) ...)
			     . ?body))
			 ((_ ?inherit-fields ?variable-name . ?body)
			  #'(begin . ?body))
			 )))

		   (define-syntax with-label-bindings/methods
		     (lambda (stx)
		       (syntax-case stx ()
			 ((_ ?inherit-methods ?variable-name . ?body)
			  (syntax->datum #'?inherit-methods)
			  #'(%with-class-methods ?variable-name ((?method ?method-function) ...)
						 . ?body))
			 ((_ ?inherit-methods ?variable-name . ?body)
			  #'(begin . ?body))
			 )))

		   (define-syntax with-label-bindings/setter-and-getter
		     (lambda (stx)
		       (syntax-case stx ()
			 ((_ ?inherit-setter-and-getter ?variable-name . ?body)
			  (syntax->datum #'?inherit-setter-and-getter)
			  #'(%with-class-setter-and-getter ?variable-name ?setter ?getter . ?body))
			 ((_ ?inherit-setter-and-getter ?variable-name . ?body)
			  #'(begin . ?body))
			 )))

		   )))
	   )))
      )))


(define-syntax %with-class-fields
  ;;Handle  access to  fields, both  concrete and  virtual;  expand into
  ;;nested uses of WITH-ACCESSOR-AND-MUTATOR from (language-extensions).
  ;;
  (lambda (stx)
    (syntax-case stx (mutable immutable)

      ;;Process a field clause with both accessor and mutator.
      ((_ ?variable-name ((mutable ?field ?accessor ?mutator) ?clause ...) . ?body)
       (and (identifier? #'?variable-name)
	    (identifier? #'?field)
	    (identifier? #'?accessor)
	    (identifier? #'?mutator))
       #`(with-accessor-and-mutator ((#,(syntax-dot-notation-name #'?variable-name #'?field)
				      ?variable-name ?accessor ?mutator))
				    (%with-class-fields ?variable-name (?clause ...) . ?body)))

      ;;Process a field clause with accessor only.
      ((_ ?variable-name ((immutable ?field ?accessor) ?clause ...) . ?body)
       (and (identifier? #'?variable-name)
	    (identifier? #'?field)
	    (identifier? #'?accessor))
       #`(with-accessor-and-mutator ((#,(syntax-dot-notation-name #'?variable-name #'?field)
				      ?variable-name ?accessor))
				    (%with-class-fields ?variable-name (?clause ...) . ?body)))

      ;;No more field clauses, output the body.
      ((_ ?variable-name () . ?body)
       (identifier? #'?variable-name)
       #'(begin . ?body))

      )))


(define-syntax %with-class-setter-and-getter
  ;;Wrap the body with the  LET-SYNTAX defining the setter binding; then
  ;;hand the body  to %WITH-CLASS-GETTER.  If there is  no setter: leave
  ;;the  identifier undefined,  so  that it  does  not shadow  enclosing
  ;;bindings.
  ;;
  (lambda (stx)
    (syntax-case stx ()

      ((_ ?variable-name ?setter ?getter . ?body)
       (and (identifier? #'?variable-name) (identifier? #'?setter))
       #`(let-syntax
	     ((#,(%variable-name->Setter-name #'?variable-name)
	       (syntax-rules ()
		 ((_ ?key0 ?key (... ...) ?value)
		  (?setter ?variable-name ?key0 ?key (... ...) ?value)))))
	   (%with-class-getter ?variable-name ?getter . ?body)))

      ((_ ?variable-name #f ?getter . ?body)
       (identifier? #'?variable-name)
       #'(%with-class-getter ?variable-name ?getter . ?body))

      )))

(define-syntax %with-class-getter
  ;;Subroutine of %WITH-CLASS-SETTER-AND-GETTER:  wrap the body with the
  ;;LET-SYNTAX defining  the getter binding;  then expand the  body.  If
  ;;there is no getter: leave  the identifier undefined, so that it does
  ;;not shadow enclosing bindings.
  ;;
  (lambda (stx)
    (syntax-case stx ()

      ((_ ?variable-name ?getter . ?body)
       (and (identifier? #'?variable-name) (identifier? #'?getter))
       #`(let-syntax ((#,(%variable-name->Getter-name #'?variable-name)
		       (syntax-rules ()
			 ((_ ?key0 ?key (... ...))
			  (?getter ?variable-name ?key0 ?key (... ...))))))
	   . ?body))

      ((_ ?variable-name #f . ?body)
       (identifier? #'?variable-name)
       #'(begin . ?body))

      )))


(define-syntax %with-class-methods
  ;;Expand into a LET-SYNTAX form,  wrapping the body, which defines the
  ;;methods' syntaxes.
  ;;
  (lambda (stx)
    (syntax-case stx ()

      ;;Generate the methods' syntaxes.
      ((_ ?variable-name ((?method ?function-name) ...) . ?body)
       (all-identifiers? #'(?variable-name ?method ... ?function-name ...))
       (with-syntax (((METHOD ...) (map (lambda (method/stx)
					  (syntax-dot-notation-name #'?variable-name method/stx))
				     #'(?method ...))))
	 #'(let-syntax ((METHOD (syntax-rules ()
				  ((_ ?arg (... ...))
				   (?function-name ?variable-name ?arg (... ...)))))
			...)
	     . ?body)))

      ;;No methods, output the body.
      ((_ ?variable-name () . ?body)
       (identifier? #'?variable-name)
       #'(begin . ?body))

      )))


;;;; core public syntaxes to access fields, methods, setters and getters

(define-syntax with-class
  ;;This  is the  public entry  point  for fields,  methods, setter  and
  ;;getter access;  the name WITH-CLASS is  a bit misleading  but it is
  ;;cute.  Just  hand everything to  %WITH-CLASS-BINDINGS: the recursive
  ;;%WITH-CLASS-BINDINGS needs to allow input forms which we do not want
  ;;for WITH-CLASS.
  ;;
  ;;We  allow  an  empty list  of  clauses  because  it is  useful  when
  ;;expanding other macros into WITH-CLASS uses.
  ;;
  (syntax-rules ()
    ((_ (?clause ...) ?body0 ?body ...)
     (%with-class-bindings (?clause ...) ?body0 ?body ...))))

(define-syntax %with-class-bindings
  ;;The  core syntax  for  fields, methods,  setter  and getter  access.
  ;;Expand into nested uses of:
  ;;
  ;;  (<class> with-class-bindings-of ...)
  ;;
  ;;which is the syntax having knowledge of the context of <class>.
  ;;
  (lambda (stx)
    (syntax-case stx ()

      ;;If the  class is "<top>" skip  it, because "<top>"  has no class
      ;;bindings.
      ((_ ((?var ?class0 ?class ...) ?clause ...) ?body0 ?body ...)
       (and (identifier? #'?var) (free-identifier=? #'?class0 #'<top>))
       #'(%with-class-bindings ((?var ?class ...) ?clause ...) ?body0 ?body ...))

      ;;Process the next class in the clause.
      ((_ ((?var ?class0 ?class ...) ?clause ...) ?body0 ?body ...)
       (and (identifier? #'?var) (identifier? #'?class0))
       #'(?class0 with-class-bindings-of (#t #t #t #t) ;enable everything
		  ?var
		  (%with-class-bindings ((?var ?class ...) ?clause ...) ?body0 ?body ...)))

      ;;No more classes, move to the next clause.
      ((_ ((?var) ?clause ...) ?body0 ?body ...)
       #'(%with-class-bindings (?clause ...) ?body0 ?body ...))

      ;;No more clauses, expand the body.
      ((_ () ?body0 ?body ...)
       #'(begin ?body0 ?body ...)))))

;;; --------------------------------------------------------------------

(define-syntax setf
  (lambda (stx)
    (syntax-case stx (setter setter-multi-key set!)
      ((_ (?variable-name ?key0 ?key ...) ?value)
       (identifier? #'?variable-name)
       #`(#,(%variable-name->Setter-name #'?variable-name)
	  ?key0 ?key ... ?value))
       ((_ ?variable-name ?value)
       (identifier? #'?variable-name)
	#'(set! ?variable-name ?value))
       )))

(define-syntax getf
  (lambda (stx)
    (syntax-case stx (setter setter-multi-key set!)
      ((_ (?variable-name ?key0 ?key ...))
       (identifier? #'?variable-name)
       #`(#,(%variable-name->Getter-name #'?variable-name)
	  ?key0 ?key ...)))))


;;;; LET wrappers

(define no-loop #f)

(define-syntax %do-let/no-types
  ;;Process a LET form in which  all the bindings are untyped.  Hand the
  ;;rest to %DO-LET/ADD-TOP.
  ;;
  (lambda (stx)
    (syntax-case stx ()

      ;;No bindings.  Expand to ?LET  to allow <definition> forms in the
      ;;body.
      ((_ ?let ?let/with-class ?loop () ?body0 ?body ...)
       (if (free-identifier=? #'no-loop #'?loop)
	   #'(?let () ?body0 ?body ...)
	 #'(?let ?loop () ?body0 ?body ...)))

      ;;All bindings are without types.
      ((_ ?let ?let/with-class ?loop ((?var ?init) ...) ?body0 ?body ...)
       (all-identifiers? #'(?var ...))
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
	   (with-class ((?var ?class0 ?class ...) ...) ?body0 ?body ...)))

      ((_ ?loop (((?var ?class0 ?class ...) ?init) ...) ?body0 ?body ...)
       #'(let ?loop ((?var ?init) ...)
	   (with-class ((?var ?class0 ?class ...) ...) ?body0 ?body ...)))
      )))

;;; --------------------------------------------------------------------

(define-syntax let*/with-class
  (syntax-rules ()
    ((_ ((?var ?init) ...) ?body0 ?body ...)
     (%do-let/no-types let* %let*/with-class no-loop ((?var ?init) ...) ?body0 ?body ...))))

(define-syntax %let*/with-class
  (lambda (stx)
    (syntax-case stx ()

      ((let*/with-class (((?var0 ?class0 ?class00 ...) ?init0)
			 ((?var1 ?class1 ?class11 ...) ?init1)
			 ...)
	 ?body0 ?body ...)
       (let ((id (duplicated-identifiers? #'(?var0 ?var1 ...))))
	 (if id
	     #`(syntax-violation 'let*/with-class
		 "duplicated field names in let*/with-class"
		 (quote (let*/with-class (((?var0 ?class0 ?class00 ...) ?init0)
					  ((?var1 ?class1 ?class11 ...) ?init1)
					  ...)
			  ?body0 ?body ...))
		 (quote (#,id)))
	   #'(let ((?var0 ?init0))
	       (with-class ((?var0 ?class0 ?class00 ...))
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
       (with-class ((?var ?class0 ?class ...) ...)
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
       (with-class ((?var ?class0 ?class ...) ...)
	 (set! ?var ?init) ...
	 ?body0 ?body ...)))))


;;;; DEFINE and LAMBDA wrappers

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
       (with-class ((?collected-arg ?collected-cls ...) ...) . ?body)))
    ((_ #t () ((?collected-cls ...) ...) (?collected-arg ...) . ?body)
     (lambda (?collected-arg ...)
       (%add-assertions ((?collected-cls ...) ...) (?collected-arg ...))
       (let ()
	 (with-class ((?collected-arg ?collected-cls ...) ...) . ?body))))

    ;;Matches two cases: (1) when  all the arguments have been processed
    ;;and only  the rest argument is  there; (2) when the  formals is an
    ;;identifier (lambda args ---).
    ((_ #f ?rest ((?collected-cls ...) ...) (?collected-arg ...) . ?body)
     (lambda (?collected-arg ... . ?rest)
       (with-class ((?collected-arg ?collected-cls ...) ...) . ?body)))
    ((_ #t ?rest ((?collected-cls ...) ...) (?collected-arg ...) . ?body)
     (lambda (?collected-arg ... . ?rest)
       (%add-assertions ((?collected-cls ...) ...) (?collected-arg ...))
       (let ()
	 (with-class ((?collected-arg ?collected-cls ...) ...) . ?body))))
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
					  (with-class ((?collected-arg ?collected-cls ...)
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
					    (with-class ((?collected-arg ?collected-cls ...)
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
					  (with-class ((?collected-arg ?collected-cls ...)
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
					    (with-class ((?collected-arg ?collected-cls ...)
							 ...)
					      . ?body))))
      ()
      ()
      ?case-lambda-clause ...))

    ;;Matches when all the CASE-LAMBDA clauses have been collected.
    ((_ ?add-assertions (?collected-case-lambda-clause ...) () ())
     (case-lambda ?collected-case-lambda-clause ...))

    ))


;;;; builtin classes

(define-record-type <top>
  (nongenerative nausicaa:builtin:<top>))

(define-syntax <top>-superclass
  (syntax-rules (class-record-type-descriptor
		 class-type-uid
		 class-uid-list
		 public-constructor-descriptor
		 superclass-constructor-descriptor
		 from-fields-constructor-descriptor
		 parent-rtd-list
		 make make-from-fields is-a?
		 with-class-bindings-of)

    ((_ class-record-type-descriptor)
     (record-type-descriptor <top>))

    ((_ class-type-uid)
     (quote nausicaa:builtin:<top>))

    ((_ class-uid-list)
     '(nausicaa:builtin:<top>))

    ((_ public-constructor-descriptor)
     (record-constructor-descriptor <top>))

    ((_ superclass-constructor-descriptor)
     (record-constructor-descriptor <top>))

    ((_ from-fields-constructor-descriptor)
     (record-constructor-descriptor <top>))

    ((_ parent-rtd-list)
     (list (record-type-descriptor <top>)))

    ((_ is-a? ?arg)
     (<top>? ?arg))

    ((_ with-class-bindings-of ?inherit-options . ?body)
     (begin . ?body))

    ((_ ?keyword . ?rest)
     (syntax-violation '<top>
       "invalid class internal keyword"
       (syntax->datum #'(<top> ?keyword . ?rest))
       (syntax->datum #'?keyword)))))

(define-syntax <top>-label
  (syntax-rules (is-a? with-class-bindings-of)

    ((_ is-a? ?arg)
     #t)

    ((_ with-class-bindings-of ?inherit-options . ?body)
     (begin . ?body))

    ((_ ?keyword . ?rest)
     (syntax-violation '<top>
       "invalid class internal keyword"
       (syntax->datum #'(<top> ?keyword . ?rest))
       (syntax->datum #'?keyword)))))

(define-syntax <top>-bindings
  (syntax-rules ()
    ((_ ?class-name ?identifier . ?body)
     (begin . ?body))))



(define-virtual-class <builtin>
  (nongenerative nausicaa:builtin:<builtin>))

(define-syntax define-builtin-class
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?class-name ?clause ...)
       #`(define-virtual-class ?class-name
	   (inherit <builtin>)
	   (nongenerative #,(syntax-prefix "nausicaa:builtin:" #'?class-name))
	   ?clause ...)))))

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
  (inherit <port>)
  (predicate input-port?)
  (nongenerative nausicaa:builtin:<input-port>))

(define-virtual-class <output-port>
  (inherit <port>)
  (predicate output-port?)
  (nongenerative nausicaa:builtin:<output-port>))

;;This predicate is needed because in some implementations (Petite Chez)
;;BINARY-PORT? throws an exception if  OBJ is not a port.
;;
;;*NOTE* The  predicate definition goes  before the class  definition to
;;make some Scheme implementations happy.
(define (%binary-port? obj)
  (and (port? obj) (binary-port? obj)))

(define-virtual-class <binary-port>
  (inherit <port>)
  (predicate %binary-port?)
  (nongenerative nausicaa:builtin:<binary-port>))

;;This predicate is needed because in some implementations (Petite Chez)
;;TEXTUAL-PORT? throws an exception if OBJ is not a port.
;;
;;*NOTE* The  predicate definition goes  before the class  definition to
;;make some Scheme implementations happy.
(define (%textual-port? obj)
  (and (port? obj) (textual-port? obj)))

(define-virtual-class <textual-port>
  (inherit <port>)
  (predicate %textual-port?)
  (nongenerative nausicaa:builtin:<textual-port>))

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
  (inherit <number>)
  (predicate complex?)
  (virtual-fields (immutable real-part	real-part)
		  (immutable imag-part	imag-part)
		  (immutable magnitude	magnitude)
		  (immutable angle	angle))
  (nongenerative nausicaa:builtin:<complex>))

(define-virtual-class <real-valued>
  (inherit <complex>)
  (predicate real-valued?)
  (virtual-fields (immutable positive?		positive?)
		  (immutable negative?		negative?)
		  (immutable non-positive?	non-positive?)
		  (immutable non-negative?	non-negative?))
  (nongenerative nausicaa:builtin:<real-valued>))

(define-virtual-class <real>
  (inherit <real-valued>)
  (predicate real?)
  (nongenerative nausicaa:builtin:<real>)
  (virtual-fields (immutable abs)))

(define-virtual-class <rational-valued>
  (inherit <real>)
  (predicate rational-valued?)
  (nongenerative nausicaa:builtin:<rational-valued>))

(define-virtual-class <flonum>
  (inherit <real>)
  (predicate flonum?)
  (nongenerative nausicaa:builtin:<flonum>))

(define-virtual-class <rational>
  (inherit <rational-valued>)
  (predicate rational?)
  (nongenerative nausicaa:builtin:<rational>))

(define-virtual-class <integer-valued>
  (inherit <rational-valued>)
  (predicate integer-valued?)
  (nongenerative nausicaa:builtin:<integer-valued>))

(define-virtual-class <integer>
  (inherit <integer-valued>)
  (predicate integer?)
  (nongenerative nausicaa:builtin:<integer>))

(define-virtual-class <fixnum>
  (inherit <integer>)
  (predicate fixnum?)
  (nongenerative nausicaa:builtin:<fixnum>))


;;;; done

)

;;; end of file
