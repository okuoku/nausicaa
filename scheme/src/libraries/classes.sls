;;; -*- coding: utf-8 -*-
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

;;;Copyright (C) Michael Sperber (2005). All Rights Reserved.
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT. IN  NO EVENT SHALL THE AUTHORS  OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


#!r6rs
(library (classes)
  (export

    ;; usage macros
    define-class			define-foreign-class
    define-label			is-a?
    make				make-from-fields
    make*
    define-virtual-method
    defmethod				defmethod-virtual

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
    define/with-class			lambda/with-class
    case-lambda/with-class		receive/with-class
    let/with-class			let*/with-class
    letrec/with-class			letrec*/with-class
    do/with-class			do*/with-class

    ;; auxiliary syntaxes
    parent sealed opaque parent-rtd nongenerative
    protocol fields mutable immutable
    inherit predicate maker setter getter bindings
    public-protocol maker-protocol superclass-protocol
    virtual-fields methods method method-syntax

    ;; builtin classes
    <top> <builtin> <pair> <list>
    <char> <string> <vector> <bytevector> <hashtable> <record> <condition>
    <port> <binary-port> <input-port> <output-port> <textual-port>
    <fixnum> <flonum> <integer> <integer-valued> <rational> <rational-valued>
    <real> <real-valued> <complex> <number>)
  (import (rnrs)
    (rnrs mutable-strings)
    (for (syntax-utilities)	expand)
    (for (gensym)		expand)
    (for (classes helpers)	expand)
    (for (prefix (sentinel) sentinel.) expand)
    (makers)
    (contracts)
    (auxiliary-syntaxes)
    (only (language-extensions)
	  define-values
	  define-syntax*
	  with-accessor-and-mutator)
    (classes internal-auxiliary-syntaxes)
    (classes top))


(define (%make-default-protocol rtd)
  ;;Given  a   record  type  descriptor  build  and   return  a  default
  ;;constructor: a function accepting the raw field values and returning
  ;;a record instance.
  ;;
  (define (split-at l n)
    (if (zero? n)
	(values '() l)
      (let-values (((a b) (split-at (cdr l) (- n 1))))
	(values (cons (car l) a) b))))
  (define (field-count rtd count)
    ;;We know that <top> has no fields and no parent.
    (if (or (not rtd) (eq? 'nausicaa:builtin:<top> (record-type-uid rtd)))
	count
      (field-count (record-type-parent rtd)
		   (+ count (vector-length (record-type-field-names rtd))))))
  (let ((parent (record-type-parent rtd)))
    (if (not parent)
	(lambda (make-record)
	  (lambda field-values
	    (apply make-record field-values)))
      (let ((parent-field-count (field-count parent 0)))
	(lambda (make-parent)
	  (lambda all-field-values
	    (call-with-values
		(lambda ()
		  (split-at all-field-values parent-field-count))
	      (lambda (parent-field-values this-field-values)
		(apply (apply make-parent parent-field-values) this-field-values)))))))))

(define (%make-from-fields-cd rtd protocol)
  ;;Given a  record type  descriptor and a  protocol function  build and
  ;;return a  default constructor descriptor: the one  accepting the raw
  ;;field   values.    The   returned   descriptor  is   used   by   the
  ;;MAKE-FROM-FIELDS syntax.
  ;;
  (make-record-constructor-descriptor
   rtd
   (let ((parent-rtd (record-type-parent rtd)))
     (if parent-rtd
	 (if (eq? 'nausicaa:builtin:<top> (record-type-uid parent-rtd))
	     (record-constructor-descriptor <top>)
	   (%make-from-fields-cd parent-rtd (%make-default-protocol parent-rtd)))
       #f))
   protocol))


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


;;;; inspection macros

(define-syntax class-record-type-descriptor
  ;;Expand into the record type  descriptor (RTD) record associated to a
  ;;class name.
  ;;
  (syntax-rules (<top>)
    ((_ <top>)
     (record-type-descriptor <top>))
    ((_ ?class-name)
     (?class-name :class-record-type-descriptor))))

;;; --------------------------------------------------------------------

(define-syntax class-public-constructor-descriptor
  ;;Expand into the class' public constructor descriptor associated to a
  ;;class name.
  ;;
  (syntax-rules (<top>)
    ((_ <top>)
     (record-constructor-descriptor <top>))
    ((_ ?class-name)
     (?class-name :public-constructor-descriptor))))

(define-syntax class-superclass-constructor-descriptor
  ;;Expand into the  class' superclass constructor descriptor associated
  ;;to a class name.
  ;;
  (syntax-rules (<top>)
    ((_ <top>)
     (record-constructor-descriptor <top>))
    ((_ ?class-name)
     (?class-name :superclass-constructor-descriptor))))

(define-syntax class-from-fields-constructor-descriptor
  ;;Expand into the class' from-fields constructor descriptor associated
  ;;to a class name.
  ;;
  (syntax-rules (<top>)
    ((_ <top>)
     (record-constructor-descriptor <top>))
    ((_ ?class-name)
     (?class-name :from-fields-constructor-descriptor))))

;;; --------------------------------------------------------------------

(define-syntax class-type-uid
  ;;Expand into the class type UID associated to a class name.
  ;;
  (syntax-rules (<top>)
    ((_ <top>)
     (record-type-uid (record-type-descriptor <top>)))
    ((_ ?class-name)
     (?class-name :class-type-uid))))

(define-syntax class-uid-list
  ;;Expand into  the list of type UIDs  of the parents of  a class name.
  ;;The first element is the UID of the class itself, then comes the UID
  ;;of the parent, then the UID of the parent's parent, etc.
  ;;
  (syntax-rules (<top>)
    ((_ <top>)
     (list (record-type-uid (record-type-descriptor <top>))))
    ((_ ?class-name)
     (?class-name :class-uid-list))))

;;; --------------------------------------------------------------------

(define-syntax class-parent-rtd-list
  (syntax-rules ()
    ((_ ?class-name)
     (?class-name :parent-rtd-list))))


;;;; usage macros

(define-syntax make
  ;;Build a new class instance using the public constructor.
  ;;
  (syntax-rules ()
    ((_ ?class-name ?arg ...)
     (?class-name :make ?arg ...))))

(define-syntax make*
  ;;Build a new class instance using the maker constructor.
  ;;
  (syntax-rules ()
    ((_ ?class-name ?arg ...)
     (?class-name :make* ?arg ...))))

(define-syntax make-from-fields
  ;;Build a new class instance using the "from fields" constructor.
  ;;
  (syntax-rules ()
    ((_ ?class-name ?arg ...)
     (?class-name :make-from-fields ?arg ...))))

(define-syntax* (is-a? stx)
  ;;Test  if  a  given object  matches  a  class  type using  the  class
  ;;predicate selected in the DEFINE-CLASS form.
  ;;
  (syntax-case stx (<top>)

    ((_ ?obj <top>)
     (syntax #t))

    ((_ ?obj ?class-name)
     (identifier? #'?class-name)
     #'(?class-name :is-a? ?obj))

    (_
     (synner "invalid syntax use"))))


(define-syntax define-foreign-class
  ;;A foreign class  is just a tag  we slap on any value  to use virtual
  ;;fields  and methods  with dot  notation,  but nevertheless  it is  a
  ;;proper record type.
  ;;
  (syntax-rules ()
    ((_ ?name ?clause ...)
     (%define-foreign-class (define-class ?name ?clause ...) ?name () ?clause ...))))

(define-syntax %define-foreign-class
  (lambda (stx)
    ;;Raise  an error  if  a PUBLIC-PROTOCOL,  MAKER-PROTOCOL or  FIELDS
    ;;clause is present  in the body of the  definition; else define the
    ;;class with DEFINE-CLASS specifying  a public protocol which raises
    ;;an error when invoked.
    ;;
    (define (%synner message form subform)
      (syntax-violation 'define-foreign-class message (syntax->datum form) (syntax->datum subform)))
    (syntax-case stx (public-protocol maker-protocol fields)

      ;;no more clauses to collect
      ((_ ?input-form ?name (?collected-clause ...))
       #'(define-class ?name
	   (public-protocol (lambda (make-parent)
			      (lambda args
				(syntax-violation #f
				  "attempt to instantiate foreign class" (quote ?name)))))
	   ?collected-clause ...))

      ;;found PUBLIC-PROTOCOL clause
      ((_ ?input-form ?name (?collected-clause ...) (public-protocol ?pro ...) ?clause ...)
       (%synner "public-protocol clause used in definition of foreign class"
	       #'?input-form #'(public-protocol ?pro ...)))

      ;;found MAKER-PROTOCOL clause
      ((_ ?input-form ?name (?collected-clause ...) (maker-protocol ?pro ...) ?clause ...)
       (%synner "maker-protocol clause used in definition of foreign class"
	       #'?input-form #'(public-protocol ?pro ...)))

      ;;found FIELDS clause
      ((_ ?input-form ?name (?collected-clause ...) (fields ?fie ...) ?clause ...)
       (%synner "fields clause used in definition of foreign class"
		#'?input-form
		#'(fields ?fie ...)))

      ;;other clauses
      ((_ ?input-form ?name (?collected-clause ...) ?clause0 ?clause ...)
       #'(%define-foreign-class ?input-form ?name (?collected-clause ... ?clause0) ?clause ...))

      )))


(define-syntax* (define-class stx)
  (define (main)
    (define-values (class-identifier constructor-identifier predicate-identifier clauses)
      (syntax-case stx ()
	((_ (?name ?constructor ?predicate) ?clause ...)
	 (all-identifiers? #'(?name ?constructor ?predicate))
	 (values #'?name #'?constructor #'?predicate (unwrap-syntax-object #'(?clause ...))))

	((_ ?name ?clause ...)
	 (identifier? #'?name)
	 (values #'?name
		 (syntax-maker-identifier #'?name)
		 (syntax-predicate-identifier #'?name)
		 (unwrap-syntax-object #'(?clause ...))))

	((_ ?name-spec . ?clauses)
	 (%synner "invalid name specification in class definition" #'?name-spec))))

    (validate-definition-clauses
     ;; mandatory keywords
     '()
     ;; optional keywords
     (list #'parent #'sealed #'opaque #'parent-rtd #'nongenerative #'fields #'protocol
	   #'inherit #'predicate #'maker #'setter #'getter #'bindings
	   #'public-protocol #'maker-protocol #'superclass-protocol
	   #'virtual-fields #'methods #'method #'method-syntax)
     ;; at most once keywords
     (list #'parent #'sealed #'opaque #'parent-rtd #'nongenerative
	   #'inherit #'predicate #'maker #'setter #'getter #'bindings
	   #'protocol #'public-protocol #'maker-protocol #'superclass-protocol)
     ;; mutually exclusive keywords sets
     (list (list #'inherit #'parent #'parent-rtd))
     clauses %synner)

    (let-values
	;;The superclass identifier or false; the inherit options: all
	;;boolean values.
	(((superclass-identifier inherit-concrete-fields? inherit-virtual-fields?
				 inherit-methods? inherit-setter-and-getter?)
	  (%collect-clause/class/inherit clauses %synner))

	 ;;An identifier representing the UID of the class.
	 ((uid-symbol)
	  (%collect-clause/nongenerative class-identifier clauses %synner))

	 ;;A boolean establishing if the class type is sealed.
	 ((sealed)
	  (%collect-clause/sealed clauses %synner))

	 ;;A boolean establishing if the class type is opaque.
	 ((opaque)
	  (%collect-clause/opaque clauses %synner))

	 ;;A syntax object holding  an expression which evaluates to the
	 ;;class' common  protocol function; it is false  if no protocol
	 ;;clause was present.
	 ((common-protocol)
	  (%collect-clause/protocol clauses %synner))

	 ;;A syntax  object holding  an expression which  evaluates to
	 ;;the class' public protocol function.
	 ((public-protocol)
	  (%collect-clause/public-protocol clauses %synner))

	 ;;A syntax  object holding  an expression which  evaluates to
	 ;;the class' maker protocol function.
	 ((maker-protocol)
	  (%collect-clause/maker-protocol clauses %synner))

	 ;;False  or  a  syntax  object holding  an  expression  which
	 ;;evaluates to the class' superclass protocol function.
	 ((superclass-protocol)
	  (%collect-clause/superclass-protocol clauses %synner))

	 ;;False/false or: a syntax object holding a list of identifiers
	 ;;representing the positional arguments  to the maker; a syntax
	 ;;object holding a list  of maker clauses representing optional
	 ;;arguments.
	 ;;
	 ;;*NOTE*:  when   no  maker  clause  is   present,  the  values
	 ;;false/false *cannot* be matched by WITH-SYNTAX patterns like:
	 ;;
	 ;;    ((POS-ARG ...) maker-positional-args)
	 ;;    ((OPT-ARG ...) maker-optional-args)
	 ;;
	 ;;so we must test the values first.
	 ((maker-positional-args maker-optional-args)
	  (%collect-clause/maker clauses %synner))

	 ;;False or  the identifier of  the parent *record*  type (not
	 ;;class type).
	 ((parent-name)
	  (%collect-clause/parent clauses %synner))

	 ;;False/false  or  an  expression  evaluating to  the  parent
	 ;;record type descriptor and  an expression evaluating to the
	 ;;parent constructor descriptor.
	 ((parent-rtd parent-cd)
	  (%collect-clause/parent-rtd clauses %synner))

	 ;;If a predicate clause  is present: an identifier representing
	 ;;the custom  predicate for the class.  If  no predicate clause
	 ;;is present:  set to the  PREDICATE-IDENTIFIER argument, which
	 ;;is the result of parsing he class name above.
	 ((custom-predicate)
	  (%collect-clause/predicate predicate-identifier clauses %synner))

	 ;;False or  an identifier  representing the setter  for the
	 ;;class.
	 ((setter)
	  (%collect-clause/setter clauses %synner))

	 ;;False or  an identifier  representing the getter  for the
	 ;;class.
	 ((getter)
	  (%collect-clause/getter clauses %synner))

	 ;;An identifier representing  the custom bindings macro for
	 ;;the class.
	 ((bindings-macro)
	  (%collect-clause/bindings clauses %synner))

	 ;;Null or a validated list of concrete fields having elements
	 ;;with format:
	 ;;
	 ;;    (immutable <field name> <field accessor>)
	 ;;    (mutable   <field name> <field accessor> <field mutator>)
	 ;;
	 ;;where  IMMUTABLE and  MUTABLE are  symbols and  the other
	 ;;elements are identifiers.
	 ((fields)
	  (%collect-clause/fields class-identifier clauses %synner))

	 ;;Null  or  a  validated  list  of  virtual  fields  having
	 ;;elements with format:
	 ;;
	 ;;    (immutable <field name> <field accessor>)
	 ;;    (mutable   <field name> <field accessor> <field mutator>)
	 ;;
	 ;;where  IMMUTABLE and  MUTABLE are  symbols and  the other
	 ;;elements are identifiers.
	 ((virtual-fields)
	  (%collect-clause/virtual-fields class-identifier clauses %synner))

	 ;;Null or a validated  list of method specifications from the
	 ;;METHODS clauses, having elements with format:
	 ;;
	 ;;	(<method name> <function or macro identifier>)
	 ;;
	 ((methods-from-methods)
	  (%collect-clause/methods class-identifier clauses %synner))

	 ;;Null/null or a validated list of method specifications from
	 ;;the METHOD clauses, having elements with format:
	 ;;
	 ;;    (<method name> <function name>)
	 ;;
	 ;;and a list of definitions with the format:
	 ;;
	 ;;    (<definition> ...)
	 ;;
	 ;;in which each definition has one of the formats:
	 ;;
	 ;;    (define (<function name> . <args>) . <body>)
	 ;;    (define <function name> <expression>)
	 ;;
	 ((methods method-definitions)
	  (%collect-clause/method class-identifier clauses %synner #'define/with-class))

	 ;;Null/null   or   a   validated   list  of   method   syntax
	 ;;specifications  from   the  METHOD-SYNTAX  clauses,  having
	 ;;elements with format:
	 ;;
	 ;;    (<method name> <macro identifier>)
	 ;;
	 ;;and a list of definitions with the format:
	 ;;
	 ;;    (<definition> ...)
	 ;;
	 ;;in which each definition has the format:
	 ;;
	 ;;    (define-syntax <macro identifier> <expression>)
	 ;;
	 ((syntax-methods syntax-definitions)
	  (%collect-clause/method-syntax class-identifier clauses %synner)))

      (define the-parent-is-a-class? (identifier? superclass-identifier))
      (set! methods		(append methods methods-from-methods syntax-methods))
      (set! method-definitions	(append method-definitions syntax-definitions))

      (let ((id (duplicated-identifiers? (append (map cadr fields)
						 (map cadr virtual-fields)
						 (map car  methods)))))
	(when id
	  (%synner "duplicated field names" id)))

      ;;Normalise the  inheritance for this  class.  We must  end with
      ;;sound  values bound  to SUPERCLASS-IDENTIFIER,  PARENT-RTD and
      ;;PARENT-CD.  The  parse procedure above  have left us  with the
      ;;following situation:
      ;;
      ;;* If  the INHERIT clause is  present: SUPERCLASS-IDENTIFIER is
      ;;set to  the identifier of  a superclass macro;  PARENT-RTD and
      ;;PARENT-CD set to false.
      ;;
      ;;* If the  PARENT clause is present: PARENT-NAME  is set to the
      ;;identifier of  the parent record  type; SUPERCLASS-IDENTIFIER,
      ;;PARENT and PARENT-CD are set to false.
      ;;
      ;;* If the PARENT-RTD clause  is present: PARENT-RTD is set to a
      ;;syntax object  evaluating to the parent RTD;  PARENT-CD is set
      ;;to  a  syntax  object  evaluating to  the  parent  constructor
      ;;descriptor; SUPERCLASS-IDENTIFIER is set to false.
      (cond

       ;;The INHERIT clause is present with "<top>" as superclass.
       ((and superclass-identifier
	     (free-identifier=? superclass-identifier #'<top>-superclass)
	     (not parent-name)
	     (not parent-rtd)
	     (not parent-cd))
	(set! parent-rtd #'(record-type-descriptor <top>))
	(set! parent-cd  #'(record-constructor-descriptor <top>)))

       ;;The  INHERIT clause  is present  with a  superclass different
       ;;from "<top>".
       ((and superclass-identifier
	     (identifier? superclass-identifier)
	     (not parent-name)
	     (not parent-rtd)
	     (not parent-cd))
	(set! parent-rtd #`(#,superclass-identifier :class-record-type-descriptor))
	(set! parent-cd  #`(#,superclass-identifier :superclass-constructor-descriptor)))

       ;;The PARENT clause is present.
       ((and parent-name
	     (not superclass-identifier)
	     (not parent-rtd)
	     (not parent-cd))
	(set! superclass-identifier #'<top>-superclass)
	(set! parent-rtd #`(record-type-descriptor        #,parent-name))
	(set! parent-cd  #`(record-constructor-descriptor #,parent-name)))

       ;;The PARENT-RTD clause is present.
       ((and (not superclass-identifier)
	     (not parent-name)
	     parent-rtd
	     parent-cd)
	(set! superclass-identifier #'<top>-superclass))

       ;;No inheritance clauses are present.
       ((and (not superclass-identifier)
	     (not parent-name)
	     (not parent-rtd)
	     (not parent-cd))
	(set! superclass-identifier #'<top>-superclass)
	(set! parent-rtd #'(record-type-descriptor <top>))
	(set! parent-cd  #'(record-constructor-descriptor <top>)))

       (else
	(%synner "invalid selection of superclass" #f)))

      (with-syntax
	  ((THE-CLASS			class-identifier)
	   (THE-SUPERCLASS		superclass-identifier)
	   (THE-RTD			(identifier-prefix "rtd-of-" class-identifier))
	   (THE-PARENT-CD		(identifier-prefix "parent-cd-of-" class-identifier))
	   (THE-PUBLIC-CD		(identifier-prefix "public-cd-of-" class-identifier))
	   (THE-MAKER			(identifier-prefix "maker-of-" class-identifier))
	   (THE-MAKER-CONSTRUCTOR	(identifier-prefix "maker-constructor-of-" class-identifier))
	   (THE-FROM-FIELDS-PROTOCOL	(identifier-prefix "from-fields-protocol-of-" class-identifier))
	   (THE-COMMON-PROTOCOL		(identifier-prefix "common-protocol-of-" class-identifier))
	   (THE-MAKER-PROTOCOL		(identifier-prefix "maker-protocol-of-" class-identifier))
	   (THE-PREDICATE		predicate-identifier)

	   (PARENT-RTD-FORM		parent-rtd)
	   (PARENT-CD-FORM		parent-cd)
	   (UID				uid-symbol)
	   (SEALED			sealed)
	   (OPAQUE			opaque)
	   (COMMON-PROTOCOL		common-protocol)
	   (PUBLIC-PROTOCOL		public-protocol)
	   (MAKER-PROTOCOL		maker-protocol)
	   (SUPERCLASS-PROTOCOL		superclass-protocol)
	   (CONSTRUCTOR-IDENTIFIER	constructor-identifier)
	   (CUSTOM-PREDICATE		custom-predicate)
	   ((METHOD-DEFINITION ...)	method-definitions)
	   (INHERIT-CONCRETE-FIELDS?	inherit-concrete-fields?)
	   (INHERIT-VIRTUAL-FIELDS?	inherit-virtual-fields?)
	   (INHERIT-METHODS?		inherit-methods?)
	   (INHERIT-SETTER-AND-GETTER?	inherit-setter-and-getter?)
	   (SETTER			setter)
	   (GETTER			getter)
	   (BINDINGS-MACRO		bindings-macro)
	   (((METHOD METHOD-IDENTIFIER) ...)	methods)
	   (((MUTABILITY FIELD ACCESSOR/MUTATOR ...) ...) fields)
	   (((VIRTUAL-MUTABILITY VIRTUAL-FIELD VIRTUAL-ACCESSOR/MUTATOR ...) ...) virtual-fields))
	(with-syntax
	    ;;Here we  try to  build and select  at expand time  what is
	    ;;possible.
	    (((FIELD-DEFINITION ...)	(%make-fields-accessors-and-mutators #'THE-RTD fields))
	     (MAKER-DEFINITION		(%make-maker-definition #'THE-MAKER
								#'THE-MAKER-CONSTRUCTOR
								maker-positional-args
								maker-optional-args
								constructor-identifier))
	     (FROM-FIELDS-CD-FORM	(%make-from-fields-cd-form the-parent-is-a-class?
								   #'THE-RTD
								   #'THE-SUPERCLASS
								   #'THE-FROM-FIELDS-PROTOCOL))
	     (COMMON-PROTOCOL-FORM	(or common-protocol	#'THE-FROM-FIELDS-PROTOCOL))
	     (PUBLIC-PROTOCOL-FORM	(or public-protocol	#'THE-COMMON-PROTOCOL))
	     (SUPERCLASS-PROTOCOL-FORM	(or superclass-protocol	#'THE-COMMON-PROTOCOL))
	     (MAKER-CD-FORM		(if maker-protocol
					    #'(make-record-constructor-descriptor
					       THE-RTD THE-PARENT-CD THE-MAKER-PROTOCOL)
					  #'THE-PUBLIC-CD))
	     )
	  #'(begin
	      (define the-parent-rtd	PARENT-RTD-FORM)
	      (define THE-PARENT-CD	PARENT-CD-FORM)
	      (define THE-RTD
		(make-record-type-descriptor (quote THE-CLASS) the-parent-rtd
					     (quote UID) SEALED OPAQUE
					     (quote #((MUTABILITY FIELD) ...))))

	      (define THE-PREDICATE (record-predicate THE-RTD))

	      FIELD-DEFINITION ...
	      METHOD-DEFINITION ...

	      ;;The protocol to use with the MAKE-FROM-FIELDS macro.
	      (define THE-FROM-FIELDS-PROTOCOL	(%make-default-protocol THE-RTD))
	      (define the-from-fields-cd	FROM-FIELDS-CD-FORM)
	      (define from-fields-constructor	(record-constructor the-from-fields-cd))

	      ;;The default protocol to use when the other protocols are
	      ;;not defined.
	      (define THE-COMMON-PROTOCOL	COMMON-PROTOCOL-FORM)

	      ;;The protocol to use with the MAKE macro.
	      (define the-public-protocol	PUBLIC-PROTOCOL-FORM)
	      (define THE-PUBLIC-CD		(make-record-constructor-descriptor
						 THE-RTD THE-PARENT-CD the-public-protocol))
	      (define CONSTRUCTOR-IDENTIFIER	(record-constructor THE-PUBLIC-CD))

	      ;;The protocol to use with the MAKE* macro.
	      (define THE-MAKER-PROTOCOL	MAKER-PROTOCOL)
	      (define the-maker-cd		MAKER-CD-FORM)
	      (define THE-MAKER-CONSTRUCTOR	(record-constructor the-maker-cd))

	      ;;The  protocol to use  when the  instantiated class  is a
	      ;;subclass of this one.
	      (define the-superclass-protocol	SUPERCLASS-PROTOCOL-FORM)
	      (define the-superclass-cd		(make-record-constructor-descriptor
						 THE-RTD THE-PARENT-CD the-superclass-protocol))
	      (define superclass-constructor	(record-constructor the-superclass-cd))

	      ;;This is memoized  because it is slow to  compute and not
	      ;;always needed.
	      (define the-uid-list
		(let ((the-list #f))
		  (lambda ()
		    (or the-list
			(begin
			  (set! the-list
				(cons (quote UID)
				      (if the-parent-rtd
					  (map record-type-uid (record-parent-list the-parent-rtd))
					'())))
			  the-list)))))

	      ;;This is memoized  because it is slow to  compute and not
	      ;;always needed.
	      (define the-parent-rtd-list
		(let ((the-list #f))
		  (lambda ()
		    (or the-list
			(begin
			  (set! the-list (cons THE-RTD (if the-parent-rtd
							   (record-parent-list the-parent-rtd)
							 '())))
			  the-list)))))

	      (define-syntax THE-CLASS
		(lambda (stx)
		  (syntax-case stx (:class-record-type-descriptor
				    :class-type-uid
				    :class-uid-list
				    :from-fields-constructor-descriptor
				    :is-a?
				    :list-of-concrete-fields
				    :list-of-methods
				    :list-of-virtual-fields
				    :make
				    :make*
				    :make-from-fields
				    :parent-rtd-list
				    :public-constructor-descriptor
				    :superclass-constructor-descriptor
				    :superclass-protocol
				    :with-class-bindings-of)

		    ((_ :class-record-type-descriptor)
		     #'THE-RTD)

		    ((_ :class-type-uid)
		     #'(quote UID))

		    ((_ :class-uid-list)
		     #'(the-uid-list))

		    ((_ :parent-rtd-list)
		     #'(the-parent-rtd-list))

		    ;; --------------------------------------------------

		    ((_ :public-constructor-descriptor)
		     #'THE-PUBLIC-CD)

		    ((_ :superclass-constructor-descriptor)
		     #'the-superclass-cd)

		    ((_ :from-fields-constructor-descriptor)
		     #'the-from-fields-cd)

		    ((_ :superclass-protocol)
		     #'the-superclass-protocol)

		    ;; --------------------------------------------------

		    ((_ :list-of-concrete-fields)
		     #'((MUTABILITY FIELD ACCESSOR/MUTATOR ...) ...))

		    ((_ :list-of-virtual-fields)
		     #'((VIRTUAL-MUTABILITY VIRTUAL-FIELD VIRTUAL-ACCESSOR/MUTATOR ...) ...))

		    ((_ :list-of-methods)
		     #'((METHOD METHOD-IDENTIFIER) ...))

		    ;; --------------------------------------------------

		    ((_ :make ?arg (... ...))
		     #'(CONSTRUCTOR-IDENTIFIER ?arg (... ...)))

		    ((_ :make* ?arg (... ...))
		     #'(THE-MAKER ?arg (... ...)))

		    ((_ :make-from-fields ?arg (... ...))
		     #'(from-fields-constructor ?arg (... ...)))

		    ((_ :is-a? ?arg)
		     #'(CUSTOM-PREDICATE ?arg))

		    ((_ :with-class-bindings-of
			(?use-dot-notation
			 ?inherit-concrete-fields
			 ?inherit-virtual-fields
			 ?inherit-methods
			 ?inherit-setter-and-getter)
			?variable-name ?arg (... ...))
		     (for-all boolean? (syntax->datum #'(?use-dot-notation
							 ?inherit-concrete-fields
							 ?inherit-virtual-fields
							 ?inherit-methods
							 ?inherit-setter-and-getter)))
		     #'(with-class-bindings
			(?use-dot-notation
			 ?inherit-concrete-fields
			 ?inherit-virtual-fields
			 ?inherit-methods
			 ?inherit-setter-and-getter)
			?variable-name ?arg (... ...)))

		    ((_ ?keyword . ?rest)
		     (syntax-violation 'THE-CLASS
		       "invalid class internal keyword"
		       (syntax->datum stx)
		       (syntax->datum #'?keyword)))
		    )))

	      MAKER-DEFINITION

	      (define-syntax with-class-bindings
		(syntax-rules ()
		  ((_ (?use-dot-notation
		       ?inherit-concrete-fields
		       ?inherit-virtual-fields
		       ?inherit-methods
		       ?inherit-setter-and-getter)
		      ?variable-name ?body0 ?body (... ...))
		   (THE-SUPERCLASS :with-class-bindings-of (?use-dot-notation
							    INHERIT-CONCRETE-FIELDS?
							    INHERIT-VIRTUAL-FIELDS?
							    INHERIT-METHODS?
							    INHERIT-SETTER-AND-GETTER?)
				   ?variable-name
				   (with-class-bindings/concrete-fields
				    ?inherit-concrete-fields ?use-dot-notation ?variable-name
				    (with-class-bindings/virtual-fields
				     ?inherit-virtual-fields ?use-dot-notation ?variable-name
				     (with-class-bindings/methods
				      ?inherit-methods ?use-dot-notation ?variable-name
				      (with-class-bindings/setter-and-getter
				       ?inherit-setter-and-getter ?variable-name
				       (BINDINGS-MACRO THE-CLASS ?variable-name
						       ?body0 ?body (... ...))))))))
		  ))

	      (define-syntax with-class-bindings/concrete-fields
		(syntax-rules ()
		  ((_ #t ?use-dot-notation ?variable-name . ?body)
		   (%with-class-fields ?use-dot-notation ?variable-name
				       ((MUTABILITY FIELD ACCESSOR/MUTATOR ...) ...)
				       . ?body))
		  ((_ #f ?use-dot-notation ?variable-name . ?body)
		   (begin . ?body))
		  ))

	      (define-syntax with-class-bindings/virtual-fields
		(syntax-rules ()
		  ((_ #t ?use-dot-notation ?variable-name . ?body)
		   (%with-class-fields ?use-dot-notation ?variable-name
				       ((VIRTUAL-MUTABILITY VIRTUAL-FIELD
							    VIRTUAL-ACCESSOR/MUTATOR ...) ...)
				       . ?body))
		  ((_ #f ?use-dot-notation ?variable-name . ?body)
		   (begin . ?body))
		  ))

	      (define-syntax with-class-bindings/methods
		(syntax-rules ()
		  ((_ #t ?use-dot-notation ?variable-name . ?body)
		   (%with-class-methods ?use-dot-notation ?variable-name
					((METHOD METHOD-IDENTIFIER) ...) . ?body))
		  ((_ #f ?use-dot-notation ?variable-name . ?body)
		   (begin . ?body))
		  ))

	      (define-syntax with-class-bindings/setter-and-getter
		(syntax-rules ()
		  ((_ #t ?variable-name . ?body)
		   (%with-class-setter-and-getter ?variable-name SETTER GETTER . ?body))
		  ((_ #f ?variable-name . ?body)
		   (begin . ?body))
		  ))

	      )))))

  (define (%make-fields-accessors-and-mutators rtd-name fields)
    ;;Build and return a syntax object holding a list of the definitions
    ;;of the class's concrete field accessors and mutators.
    ;;
    ;;RTD-NAME must be an identifier bound to the record type descriptor
    ;;for this class.
    ;;
    ;;FIELDS  must  be   a  syntax  object  holding  a   list  of  field
    ;;specifications in the following format:
    ;;
    ;;    (mutable   ?field ?accessor ?mutator)
    ;;    (immutable ?field ?accessor)
    ;;
    ;;the  order of  the field  specifications must  match the  order of
    ;;fields in the RTD definition.
    ;;
    (let loop ((definitions	'())
	       (field-index	0)
	       (fields		fields))
      (syntax-case fields (mutable immutable)
	(()
	 definitions)

	(((mutable ?field ?accessor ?mutator) . ?clauses)
	 (loop (cons* #`(define ?accessor  (record-accessor #,rtd-name #,field-index))
		      #`(define ?mutator   (record-mutator  #,rtd-name #,field-index))
		      definitions)
	       (+ 1 field-index)
	       #'?clauses))

	(((immutable ?field ?accessor) . ?clauses)
	 (loop (cons* #`(define ?accessor  (record-accessor #,rtd-name #,field-index))
		      definitions)
	       (+ 1 field-index)
	       #'?clauses))

	((?spec . ?clauses)
	 (%synner "invalid field specification while building field accessor and mutators" #'?spec)))))

  (define (%make-from-fields-cd-form the-parent-is-a-class? the-rtd the-superclass
				     the-from-fields-protocol)
    ;;We do this  to allow inheritance from non-class  record types.  We
    ;;select the appropriate clause  to make the construction efficient:
    ;;the constructor for non-class record types is slow compared to the
    ;;one of class types.
    (if the-parent-is-a-class?
	#`(make-record-constructor-descriptor #,the-rtd
					      (#,the-superclass :from-fields-constructor-descriptor)
					      #,the-from-fields-protocol)
      #`(%make-from-fields-cd #,the-rtd #,the-from-fields-protocol)))

  (define (%make-maker-definition the-maker the-maker-constructor
				  maker-positional-args maker-optional-args
				  public-constructor)
    ;;Build and return a syntax  object holding the maker definition for
    ;;this class; if  a maker was not declared for  this class: a syntax
    ;;is  defined to  expand a  call to  the maker  into a  call  to the
    ;;constructor.
    ;;
    ;;THE-MAKER must be an identifier representing the name of the maker
    ;;macro for this class;  THE-MAKER-CONSTRUCTOR must be an identifier
    ;;bound to the constructor of the maker protocol.
    ;;
    ;;MAKER-POSITIONAL-ARGS should be a  syntax object holding a list of
    ;;identifiers  representing the  mandatory arguments  for  the maker
    ;;macro.  It must be false when no maker was declared.
    ;;
    ;;MAKER-OPTIONAL-ARGS should  be a syntax  object holding a  list of
    ;;maker  optional clauses.   It  must  be false  when  no maker  was
    ;;declared.
    ;;
    ;;PUBLIC-CONSTRUCTOR  must  be an  identifier  bound  to the  public
    ;;constructor for the class, to be used when no maker was declared.
    ;;
    (if (or maker-positional-args maker-optional-args)
	#`(define-maker (#,the-maker #,@maker-positional-args)
	    #,the-maker-constructor #,maker-optional-args)
      #`(define-syntax #,the-maker
	  (syntax-rules ()
	    ((_ . ?args)
	     (#,public-constructor . ?args))))))

  (define (%synner msg subform)
    (syntax-violation 'define-class
      (string-append msg " in class definition")
      (syntax->datum stx)
      (syntax->datum subform)))

  (main))


(define-syntax* (define-label stx)
  ;;A label is just a tag we slap on any value to use virtual fields and
  ;;methods with dot notation, it  has NO record type.  Labels canNOT be
  ;;used in the inheritance hierarchy of classes.
  ;;
  (define (main)
    (define-values (label-identifier predicate-identifier clauses)
      (syntax-case stx ()
	((_ (?name ?predicate) ?clause ...)
	 (all-identifiers? #'(?name ?predicate))
	 (values #'?name #'?predicate (unwrap-syntax-object #'(?clause ...))))

	((_ ?name ?clause ...)
	 (identifier? #'?name)
	 (values #'?name (syntax-predicate-identifier #'?name) (unwrap-syntax-object #'(?clause ...))))

	((_ ?name-spec . ?clauses)
	 (%synner "invalid name specification in label definition" #'?name-spec))))

    (validate-definition-clauses
     ;; mandatory keywords
     '()
     ;; optional keywords
     (list #'inherit #'predicate #'setter #'getter #'bindings
	   #'virtual-fields #'methods #'method #'method-syntax)
     ;; at most once keywords
     (list #'inherit #'predicate #'setter #'getter #'bindings)
     ;; mutually exclusive keywords sets
     '()
     clauses %synner)

    (let-values
	;;The superlabel  identifier or false;  the inherit options:
	;;all boolean values.
	(((superlabel-identifier inherit-virtual-fields? inherit-methods? inherit-setter-and-getter?)
	  (%collect-clause/label/inherit clauses %synner))

	 ;;False or an  identifier representing the custom predicate
	 ;;for the label.
	 ((custom-predicate)
	  (%collect-clause/label/predicate clauses %synner))

	 ;;False or  an identifier  representing the setter  for the
	 ;;label.
	 ((setter)
	  (%collect-clause/setter clauses %synner))

	 ;;False or  an identifier  representing the getter  for the
	 ;;label.
	 ((getter)
	  (%collect-clause/getter clauses %synner))

	 ;;An identifier representing  the custom bindings macro for
	 ;;the label.
	 ((bindings-macro)
	  (%collect-clause/bindings clauses %synner))

	 ;;Null  or  a  validated  list  of  virtual  fields  having
	 ;;elements with format:
	 ;;
	 ;;    (immutable <field name> <field accessor>)
	 ;;    (mutable   <field name> <field accessor> <field mutator>)
	 ;;
	 ;;where  IMMUTABLE and  MUTABLE are  symbols and  the other
	 ;;elements are identifiers.
	 ((virtual-fields)
	  (%collect-clause/virtual-fields label-identifier clauses %synner))

	 ;;Null or a validated  list of method specifications having
	 ;;elements with format:
	 ;;
	 ;;	(<field identifier> <method identifier>)
	 ;;
	 ((methods-from-methods)
	  (%collect-clause/methods label-identifier clauses %synner))

	 ;;Null/null  or a validated  list of  method specifications
	 ;;having elements with format:
	 ;;
	 ;;    (<method name> <function name>)
	 ;;
	 ;;and a list of definitions with the format:
	 ;;
	 ;;    (<definition> ...)
	 ;;
	 ;;in which each definition has one of the formats:
	 ;;
	 ;;    (define (<function name> . <args>) . <body>)
	 ;;    (define <function name> <expression>)
	 ;;
	 ((methods definitions)
	  (%collect-clause/method label-identifier clauses %synner #'define/with-class))

	 ;;Null/null   or  a   validated  list   of   method  syntax
	 ;;specifications having elements with format:
	 ;;
	 ;;    (<method name> <macro identifier>)
	 ;;
	 ;;and a list of definitions with the format:
	 ;;
	 ;;    (<definition> ...)
	 ;;
	 ;;in which each definition has the format:
	 ;;
	 ;;    (define-syntax <macro identifier> <expression>)
	 ;;
	 ((syntax-methods syntax-definitions)
	  (%collect-clause/method-syntax label-identifier clauses %synner)))

      (set! methods	(append methods methods-from-methods syntax-methods))
      (set! definitions	(append definitions syntax-definitions))

      (let ((id (duplicated-identifiers? (append (map cadr virtual-fields)
						 (map car  methods)))))
	(when id
	  (%synner "duplicated field names" id)))

      (with-syntax ((LABEL-NAME				label-identifier)
		    (SUPERLABEL-IDENTIFIER		superlabel-identifier)
		    (PREDICATE				predicate-identifier)
		    (CUSTOM-PREDICATE			custom-predicate)
		    ((DEFINITION ...)			definitions)
		    (INHERIT-VIRTUAL-FIELDS?		inherit-virtual-fields?)
		    (INHERIT-METHODS?			inherit-methods?)
		    (INHERIT-SETTER-AND-GETTER?		inherit-setter-and-getter?)
		    (SETTER				setter)
		    (GETTER				getter)
		    (BINDINGS-MACRO			bindings-macro)
		    (((METHOD METHOD-FUNCTION) ...)	methods)
		    (((VIRTUAL-MUTABILITY VIRTUAL-FIELD VIRTUAL-ACCESSOR/MUTATOR ...)
		      ...)
		     virtual-fields))
	#'(begin
	    (define PREDICATE
	      (let ((p CUSTOM-PREDICATE))
		(or p (lambda (x) #t))))

	    DEFINITION ...

	    (define-syntax LABEL-NAME
	      (lambda (stx)
		(syntax-case stx (:is-a? :with-class-bindings-of)

		  ((_ is-a? ?arg)
		   #'(PREDICATE ?arg))

		  ((_ with-class-bindings-of
		      (?use-dot-notation ;this comes from WITH-CLASS
		       ?inherit-concrete-fields
		       ?inherit-virtual-fields
		       ?inherit-methods
		       ?inherit-setter-and-getter)
		      ?variable-name ?arg (... ...))
		   (for-all boolean? (syntax->datum #'(?use-dot-notation
						       ?inherit-concrete-fields
						       ?inherit-virtual-fields
						       ?inherit-methods
						       ?inherit-setter-and-getter)))
		   #'(with-label-bindings
		      (?use-dot-notation
		       ?inherit-virtual-fields
		       ?inherit-methods
		       ?inherit-setter-and-getter)
		      ?variable-name ?arg (... ...)))

		  ((_ ?keyword . ?rest)
		   (syntax-violation 'LABEL-NAME
		     "invalid label internal keyword"
		     (syntax->datum #'(LABEL-NAME ?keyword . ?rest))
		     (syntax->datum #'?keyword)))
		  )))

	    (define-syntax with-label-bindings
	      (syntax-rules ()
		((_ (?use-dot-notation
		     ?inherit-virtual-fields ?inherit-methods ?inherit-setter-and-getter)
		    ?variable-name ?body0 ?body (... ...))
		 (SUPERLABEL-IDENTIFIER
		  :with-class-bindings-of (?use-dot-notation
					   #f
					   INHERIT-VIRTUAL-FIELDS?
					   INHERIT-METHODS?
					   INHERIT-SETTER-AND-GETTER?)
		  ?variable-name
		  (with-label-bindings/virtual-fields
		   ?inherit-virtual-fields ?use-dot-notation ?variable-name
		   (with-label-bindings/methods
		    ?inherit-methods ?use-dot-notation ?variable-name
		    (with-label-bindings/setter-and-getter
		     ?inherit-setter-and-getter ?variable-name
		     (BINDINGS-MACRO LABEL-NAME ?variable-name
				     ?body0 ?body (... ...)))))))
		))

	    (define-syntax with-label-bindings/virtual-fields
	      (syntax-rules ()
		((_ #t ?use-dot-notation ?variable-name . ?body)
		 (%with-class-fields ?use-dot-notation  ?variable-name
				     ((VIRTUAL-MUTABILITY VIRTUAL-FIELD VIRTUAL-ACCESSOR/MUTATOR ...)
				      ...)
				     . ?body))
		((_ #f ?use-dot-notation ?variable-name . ?body)
		 (begin . ?body))
		))

	    (define-syntax with-label-bindings/methods
	      (syntax-rules ()
		((_ #t ?use-dot-notation ?variable-name . ?body)
		 (%with-class-methods ?use-dot-notation ?variable-name ((METHOD METHOD-FUNCTION) ...)
				      . ?body))
		((_ #f ?use-dot-notation ?variable-name . ?body)
		 (begin . ?body))
		))

	    (define-syntax with-label-bindings/setter-and-getter
	      (syntax-rules ()
		((_ #t ?variable-name . ?body)
		 (%with-class-setter-and-getter ?variable-name SETTER GETTER . ?body))
		((_ #f ?variable-name . ?body)
		 (begin . ?body))
		))
	    ))))

  (define (%synner msg subform)
    (syntax-violation 'define-label
      (string-append msg " in label definition")
      (syntax->datum stx)
      (syntax->datum subform)))

  (main))


;;;; virtual methods
;;
;;Each virtual method has a name (method name) and a class (the class it
;;belongs to).   There is a hash  table ($VIRTUAL-METHODS-TABLE) mapping
;;virtual method names to hash  tables; the nested hash tables map class
;;UIDs to method implementation procedures.
;;

(define $virtual-methods-table
  (make-eq-hashtable))

(define-syntax define-virtual-method
  (lambda (stx)
    (define (%synner message subform)
      (syntax-violation 'define-virtual-method message (syntax->datum stx) (syntax->datum subform)))

    (syntax-case stx ()

      ;;Define a method without implementation.
      ((_ ?class ?name)
       (all-identifiers? #'(?class ?name))
       #'(define-virtual-method ?class ?name #f))

      ;;Define a method with implementation.
      ((_ ?class (?name ?this . ?args) ?body0 ?body ...)
       #'(define-virtual-method ?class ?name (lambda/with-class (?this . ?args) ?body0 ?body ...)))

      ;;Define a method with an expression as implementation.
      ((_ ?class ?name ?lambda)
       (begin
	 (unless (identifier? #'?class)
	   (%synner "expected class identifier as first argument" #'?class))
	 (unless (identifier? #'?name)
	   (%synner "expected method name identifier as second argument" #'?name))
	 #`(begin
	     (define the-table
	       (or (hashtable-ref $virtual-methods-table '?name #f)
		   (let ((table (make-eq-hashtable)))
		     (hashtable-set! $virtual-methods-table '?name table)
		     table)))
	     (define the-implementation
	       (let ((f ?lambda))
		 (when f
		   (hashtable-set! the-table (class-type-uid ?class) f))
		 f))
	     (define-syntax #,(syntax-method-identifier #'?class #'?name)
	       (syntax-rules ()
		 ;;This  is  a method,  so  we  know  that ?SELF  is  an
		 ;;identifier bound to the class instance: we can safely
		 ;;use it multiple times.
		 ((_ ?self . ?args)
		  ((%retrieve-virtual-method-implementation the-table (record-type-of ?self) '?name)
		   ?self . ?args)))))
	 ))
      )))

(define (%retrieve-virtual-method-implementation method-table rtd method-symbol-name)
  (define (%error-missing-implementation)
    (syntax-violation 'retrieve-virtual-method-implementation
      (string-append "missing virtual method implementation for "
		     (symbol->string method-symbol-name)
		     " for class "  (symbol->string (record-type-name rtd))
		     " having uid " (symbol->string (record-type-uid  rtd)))
      #f))
  (let next-parent-rtd ((rtd rtd))
    (if rtd
	(let ((uid (record-type-uid rtd)))
	  (if (eq? uid 'nausicaa:builtin:<top>)
	      (%error-missing-implementation)
	    (or (hashtable-ref method-table uid #f)
		(next-parent-rtd (record-type-parent rtd)))))
      (%error-missing-implementation))))


(define-syntax* (%with-class-fields stx)
  ;;Handle  access to  fields, both  concrete and  virtual;  expand into
  ;;nested uses of WITH-ACCESSOR-AND-MUTATOR from (language-extensions).
  ;;
  (define (main)
    (syntax-case stx ()
      ((_ ?use-dot-notation ?variable-name (?clause ...) . ?body)
       (with-syntax
	   (((BINDING ...) (make-field-bindings (syntax->datum #'?use-dot-notation)
						#'?variable-name #'(?clause ...) synner)))
	 #`(let-syntax (BINDING ...) . ?body)))
      (_
       (synner "invalid syntax in with-class-fields"))))

  (define (make-field-bindings use-dot-notation? variable-stx clauses-stx synner)
    ;;Build and return a  list of lists representing LET-SYNTAX bindings
    ;;to be used to access the fields of a class or label.
    ;;
    ;;USE-DOT-NOTATION? must  be a boolean  value: true if  dot notation
    ;;must be  used, false  if the field  name identifiers must  be used
    ;;directly.
    ;;
    ;;VARIABLE-STX must  be the identifier  bound to the class  or label
    ;;instance;  if  USE-DOT-NOTATION?  is  false, VARIABLE-STX  is  the
    ;;identifier of the "this" method argument.
    ;;
    ;;CLAUSES-STX must be the list  of clauses defining the fields; each
    ;;clause must be in one of the forms:
    ;;
    ;;   (mutable   ?field ?accessor ?mutator)
    ;;   (immutable ?field ?accessor)
    ;;
    ;;SYNNER must  be a function  used to raise syntax  violation errors
    ;;with the context of the caller.
    ;;
    (define (main)
      (assert (boolean? use-dot-notation?))
      (assert (identifier? variable-stx))
      (map (lambda (clause-stx)
	     (make-single-field-binding clause-stx synner))
	(unwrap-syntax-object clauses-stx)))

    (define (make-single-field-binding clause-stx synner)
      (define (make-keyword field-stx)
	(if use-dot-notation?
	    (syntax-dot-notation-identifier variable-stx field-stx)
	  ;;If dot  notation is off,  VARIABLE-STX is the  identifier of
	  ;;the "this" method argument.
	  ;;
	  ;;Notice that FIELD-STX was not introduced in the same context
	  ;;of VARIABLE-STX, so we have  to create a new identifier with
	  ;;the  same  name  of   FIELD-STX  and  the  same  context  of
	  ;;VARIABLE-STX.
	  (datum->syntax variable-stx (syntax->datum field-stx))))
      (syntax-case clause-stx (mutable immutable)
	((mutable ?field ?accessor ?mutator)
	 #`(#,(make-keyword #'?field)
	    (identifier-syntax
	     (_              (?accessor #,variable-stx))
	     ((set! _ ?expr) (?mutator  #,variable-stx ?expr)))))
	((immutable ?field ?accessor)
	 #`(#,(make-keyword #'?field)
	    (identifier-syntax
	     (?accessor #,variable-stx))))
	(_
	 (synner "invalid syntax in field clause" clause-stx))))

    (main))

  (main))


(define-syntax* (%with-class-methods stx)
  ;;Expand into a LET-SYNTAX form,  wrapping the body, which defines the
  ;;methods' syntaxes.
  ;;
  (define (main)
    (syntax-case stx ()
      ((_ ?use-dot-notation ?variable-name ((?method ?function-name) ...) . ?body)
       (with-syntax
	   (((BINDING ...) (make-method-bindings (syntax->datum #'?use-dot-notation)
						 #'?variable-name
						 #'((?method ?function-name) ...)
						 synner)))
	 #'(let-syntax (BINDING ...) . ?body)))
      (_
       (synner "invalid syntax in with-class-methods"))))

  (define (make-method-bindings use-dot-notation variable-stx clauses-stx synner)
    ;;Build and return a  list of lists representing LET-SYNTAX bindings
    ;;to be used to call the methods of a class or label.
    ;;
    ;;USE-DOT-NOTATION? must  be a boolean  value: true if  dot notation
    ;;must be  used, false if the  method name identifiers  must be used
    ;;directly.
    ;;
    ;;VARIABLE-STX must  be the identifier  bound to the class  or label
    ;;instance;  if  USE-DOT-NOTATION?  is  false, VARIABLE-STX  is  the
    ;;identifier of the "this" method argument.
    ;;
    ;;CLAUSE-STX must be a syntax  object holding the list of clauses in
    ;;the form:
    ;;
    ;;   ((?method ?function-name) ...)
    ;;
    ;;SYNNER must  be a function  used to raise syntax  violation errors
    ;;with the context of the caller.
    ;;
    (define (main)
      (map (lambda (clause-stx)
	     (syntax-case clause-stx ()
	       ((?method ?function-name)
		(make-single-method-binding use-dot-notation variable-stx
					    #'?method #'?function-name))
	       (_
		(synner "invalid method specification clause" clause-stx))))
	(unwrap-syntax-object clauses-stx)))

    (define (make-single-method-binding use-dot-notation? variable-stx method-stx function-name-stx)
      #`(#,(if use-dot-notation?
	       (syntax-dot-notation-identifier variable-stx method-stx)
	     ;;If dot notation is off, VARIABLE-STX is the identifier of
	     ;;the "this" method argument.
	     ;;
	     ;;Notice  that METHOD-STX  was not  introduced in  the same
	     ;;context  of VARIABLE-STX,  so  we have  to  create a  new
	     ;;identifier with the same  name of METHOD-STX and the same
	     ;;context of VARIABLE-STX.
	     (datum->syntax variable-stx (syntax->datum method-stx)))
	 (syntax-rules ()
	   ((_ ?arg (... ...))
	    (#,function-name-stx #,variable-stx ?arg (... ...))))))

    (main))

  (main))


(define-syntax* (%with-class-setter-and-getter stx)
  ;;Wrap the  body with  the LET-SYNTAX defining  the setter  and getter
  ;;bindings.   If  there  is  no   setter  or  getter:  leave  the  the
  ;;corresponding  identifiers undefined,  so  that it  does not  shadow
  ;;enclosing bindings.
  ;;

  (define (main)
    (syntax-case stx ()
      ((_ ?variable-name ?Setter ?Getter . ?body)
       (identifier? #'?variable-name)
       (with-syntax
	   (((BINDING ...) (make-setter-and-getter-bindings #'?variable-name #'?Setter #'?Getter)))
	 #`(let-syntax (BINDING ...) . ?body)))
      (_
       (synner "invalid syntax in with-setter-and-getter"))))

  (define (make-setter-and-getter-bindings variable-stx Setter-stx Getter-stx)
    (define (main)
      (append (if (not (syntax->datum Setter-stx))
		  '()
		(list (make-Setter-binding variable-stx Setter-stx)))
	      (if (not (syntax->datum Getter-stx))
		  '()
		(list (make-Getter-binding variable-stx Getter-stx)))))

    (define (make-Setter-binding variable-stx setter-stx)
      #`(#,(%variable-name->Setter-name variable-stx)
	 (syntax-rules ()
	   ((_ key0 key (... ...) value)
	    (#,setter-stx #,variable-stx key0 key (... ...) value)))))

    (define (make-Getter-binding variable-stx getter-stx)
      #`(#,(%variable-name->Getter-name variable-stx)
	 (syntax-rules ()
	   ((_ key0 key (... ...))
	    (#,getter-stx #,variable-stx key0 key (... ...))))))

    (main))

  (main))


;;;; core public syntaxes to access fields, methods, setters and getters

(define-syntax* (with-class stx)
  ;;This  is the  public entry  point  for fields,  methods, setter  and
  ;;getter access;  the name  WITH-CLASS is a  bit misleading but  it is
  ;;cute.  The gist of it is to expand:
  ;;
  ;;  (with-class ((<var> <class>) . <body>))
  ;;
  ;;into:
  ;;
  ;;  (<class> with-class-bindings-of (#t #t #t #t #t) <var> . <body>)
  ;;
  ;;which is the syntax having  knowledge of the context of <class>; the
  ;;list of true  values enable all the dot  notation syntaxes.  We want
  ;;the full expansion of:
  ;;
  ;;  (with-class ((<var> <class0> <class1>)) . <body>)
  ;;
  ;;to be:
  ;;
  ;;  (<class0> with-class-bindings-of (#t #t #t #t #t) <var>
  ;;    (<class1> with-class-bindings-of (#t #t #t #t #t) <var>
  ;;      . <body>))
  ;;
  ;;that is:  a single  contract function for  each clause and  then one
  ;;nested with-class-bindings-of for each given class.
  ;;
  ;;We  allow  an  empty list  of  clauses  because  it is  useful  when
  ;;expanding other macros into WITH-CLASS uses.
  ;;
  (syntax-case stx (<top>)
    ((_ () ?body0 ?body ...)
     #'(begin ?body0 ?body ...))

    ((_ ((?var) ?clause ...) ?body0 ?body ...)
     (identifier? #'?var)
     #'(with-class (?clause ...) ?body0 ?body ...))

    ((_ ((?var <top> ?class ...) ?clause ...) ?body0 ?body ...)
     (identifier? #'?var)
     #'(with-class ((?var ?class ...) ?clause ...) ?body0 ?body ...))

    ((_ ((?var ?class0 ?class ...) ?clause ...) ?body0 ?body ...)
     (and (identifier? #'?var) (identifier? #'?class0))
     #'(?class0 :with-class-bindings-of (#t #t #t #t #t) ?var
		(with-class ((?var ?class ...) ?clause ...) ?body0 ?body ...)))

    (_
     (synner "invalid clause in with-class form"))))

(define-syntax* (setf stx)
  (syntax-case stx (setter setter-multi-key set!)

    ((_ (?variable-name ?key0 ?key ...) ?value)
     (identifier? #'?variable-name)
     #`(#,(%variable-name->Setter-name #'?variable-name) ?key0 ?key ... ?value))

    ((_ ?variable-name ?value)
     (identifier? #'?variable-name)
     #'(set! ?variable-name ?value))

    (_
     (synner "invalid class setter syntax"))))

(define-syntax* (getf stx)
  (syntax-case stx (setter setter-multi-key set!)

    ((_ (?variable-name ?key0 ?key ...))
     (identifier? #'?variable-name)
     #`(#,(%variable-name->Getter-name #'?variable-name) ?key0 ?key ...))

    (_
     (synner "invalid class getter syntax"))))


;;;; LET and DO wrappers

;;This is used below to distinguish between named-LET and ordinary LET.
(define-auxiliary-syntax no-loop)

(define-syntax %do-let/no-types
  ;;Produce the  output forms for a  LET form in which  all the bindings
  ;;are untyped.  Hand the rest to %DO-LET/ADD-TOP.
  ;;
  (lambda (stx)
    (syntax-case stx (no-loop)

      ;;Ordinary LET,  no bindings: expand  to ?LET to  allow definition
      ;;forms in the body.
      ((_ ?let ?let/with-class no-loop () ?body0 ?body ...)
       #'(?let () ?body0 ?body ...))

      ;;Named LET, no bindings.
      ((_ ?let ?let/with-class ?loop   () ?body0 ?body ...)
       #'(?let ?loop () ?body0 ?body ...))

      ;;Ordinary LET, all bindings are without types.
      ((_ ?let ?let/with-class no-loop ((?var ?init) ...) ?body0 ?body ...)
       (all-identifiers? #'(?var ...)) ;no types if this is true
       #'(?let ((?var ?init) ...) ?body0 ?body ...))

      ;;Named LET, all bindings are without types.
      ((_ ?let ?let/with-class ?loop   ((?var ?init) ...) ?body0 ?body ...)
       (all-identifiers? #'(?var ...)) ;no types if this is true
       #'(?let ?loop ((?var ?init) ...) ?body0 ?body ...))

      ;;At least one binding has types.
      ((_ ?let ?let/with-class ?loop   ((?var ?init) ...) ?body0 ?body ...)
       (with-syntax (((VAR ...) (map (lambda (var)
				       (if (pair? var) var (list var #'<top>)))
				  (unwrap-syntax-object #'(?var ...)))))
	 #'(?let/with-class ?loop ((VAR ?init) ...) ?body0 ?body ...)))

      (_
       (syntax-violation '%do-let/no-types "invalid input form" (syntax->datum stx)))
      )))

;;; --------------------------------------------------------------------

(define-syntax let/with-class
  ;;Entry point for LET with types.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?loop ((?var ?init) ...) ?body0 ?body ...)
       #'(%do-let/no-types let %let/with-class ?loop   ((?var ?init) ...) ?body0 ?body ...))
      ((_ ((?var ?init) ...) ?body0 ?body ...)
       #'(%do-let/no-types let %let/with-class no-loop ((?var ?init) ...) ?body0 ?body ...))
      (_
       (syntax-violation 'let/with-class "invalid input form" (syntax->datum stx)))
      )))

(define-syntax* (%let/with-class stx)
  ;;Produce output forms for LET with types.
  ;;
  (syntax-case stx (no-loop)

    ((_ no-loop (((?var ?class ...) ?init) ...) ?body0 ?body ...)
     (with-syntax (((CONTRACT ...) (generate-temporaries #'(?var ...))))
       #'(let ((?var ?init) ...)
	   (with-class ((?var ?class ...) ...) ?body0 ?body ...))))

    ((_ ?loop (((?var ?class0 ?class ...) ?init) ...) ?body0 ?body ...)
     (with-syntax (((CONTRACT ...) (generate-temporaries #'(?var ...))))
       #'(let ?loop ((?var ?init) ...)
	   (with-class ((?var ?class0 ?class ...) ...) ?body0 ?body ...))))

    (_
     (synner "invalid input form"))))

;;; --------------------------------------------------------------------

(define-syntax* (let*/with-class stx)
  ;;Entry point for LET* with types.
  ;;
  (syntax-case stx (no-loop)
    ((_ ((?var ?init) ...) ?body0 ?body ...)
     #'(%do-let/no-types let* %let*/with-class no-loop ((?var ?init) ...) ?body0 ?body ...))
    (_
     (synner "invalid input form"))))

(define-syntax* (%let*/with-class stx)
  ;;Produce output forms for LET* with types.
  ;;
  (syntax-case stx (no-loop)

    ((_ no-loop (((?var0 ?class0 ?class00 ...) ?init0)
		 ((?var1 ?class1 ?class11 ...) ?init1)
		 ...)
	?body0 ?body ...)
     (let ((id (duplicated-identifiers? #'(?var0 ?var1 ...))))
       (if id
	   (synner "duplicated binding names" id)
	 #'(%let/with-class no-loop (((?var0 ?class0 ?class00 ...) ?init0))
	     (let*/with-class (((?var1 ?class1 ?class11 ...) ?init1) ...) ?body0 ?body ...))
	 )))

    ((_ no-loop () ?body0 ?body ...)
     #'(begin ?body0 ?body ...))
    ((_ ?loop . ?rest)
     (synner "named LET* is not allowed"))
    (_
     (synner "invalid input form"))))

;;; --------------------------------------------------------------------

(define-syntax* (letrec/with-class stx)
  ;;Entry point for LETREC with types.
  ;;
  (syntax-case stx ()
    ((_ ((?var ?init) ...) ?body0 ?body ...)
     #'(%do-let/no-types letrec %letrec/with-class no-loop ((?var ?init) ...) ?body0 ?body ...))
    (_
     (synner "invalid input form"))))

(define-syntax* (%letrec/with-class stx)
  ;;Produce  output forms  for LETREC  with types.   We rely  on  LET to
  ;;detect duplicated bindings.
  ;;
  (syntax-case stx (no-loop)

    ((_ no-loop (((?var ?class0 ?class ...) ?init) ...) ?body0 ?body ...)
     (with-syntax (((T ...) (generate-temporaries #'(?var ...))))
       #'(let ((?var sentinel.undefined) ...)
	   (with-class ((?var ?class0 ?class ...) ...)
	     (let ((T ?init) ...) ;do not enforce the order of evaluation of ?INIT
	       (set! ?var T) ...
	       ?body0 ?body ...)))))

    ((_ ?loop . ?rest)
     (synner "named LETREC is not allowed"))
    (_
     (synner "invalid input form"))))

;;; --------------------------------------------------------------------

(define-syntax* (letrec*/with-class stx)
  ;;Entry point for LETREC* with types.
  ;;
  (syntax-case stx ()
    ((_ ((?var ?init) ...) ?body0 ?body ...)
     #'(%do-let/no-types letrec* %letrec*/with-class no-loop ((?var ?init) ...) ?body0 ?body ...))
    (_
     (synner "invalid input form"))))

(define-syntax* (%letrec*/with-class stx)
  ;;Produce output forms for LETREC* with types.
  ;;
  ;;The  difference between  LETREC and  LETREC*  is only  the order  of
  ;;evaluation of ?INIT, which is enforced in LETREC*.
  ;;
  (syntax-case stx (no-loop)

    ((_ no-loop (((?var ?class0 ?class ...) ?init) ...) ?body0 ?body ...)
     ;;We rely on LET to detect duplicated bindings.
     (with-syntax (((C ...) (generate-temporaries #'(?var ...))))
       #'(let ((?var sentinel.undefined) ...)
	   (with-class ((?var ?class0 ?class ...) ...)
	     (set! ?var ?init) ... ;enforces the order of evaluation of ?INIT
	     ?body0 ?body ...))))

    ((_ ?loop . ?rest)
     (synner "named LETREC* is not allowed"))
    (_
     (synner "invalid input form"))))

;;; --------------------------------------------------------------------

(define-syntax do/with-class
  (syntax-rules ()
    ((_ ((?var ?init ?step ...) ...)
	(?test ?expr ...)
	?form ...)
     (let-syntax ((the-expr (syntax-rules ()
			      ((_)
			       (values))
			      ((_ ?-expr0 ?-expr (... ...))
			       (begin ?-expr0 ?-expr (... ...)))))
		  (the-step (syntax-rules ()
			      ((_ ?-var)
			       ?-var)
			      ((_ ?-var ?-step)
			       ?-step)
			      ((_ ?-var ?-step0 ?-step (... ...))
			       (syntax-violation 'do/with-class
				 "invalid step specification"
				 '(?-step0 ?-step (... ...)))))))
       (let/with-class loop ((?var ?init) ...)
		       (if ?test
			   (the-expr ?expr ...)
			 (begin
			   ?form ...
			   (loop (the-step ?var ?step ...) ...))))))))

(define-syntax* (do*/with-class stx)
  (define (%parse-var stx)
    (syntax-case stx ()
      (?id
       (identifier? #'?id)
       #'?id)
      ((?id ?class ...)
       (all-identifiers? #'(?id ?class ...))
       #'?id)
      (_
       (synner "invalid binding declaration"))))
  (syntax-case stx ()
    ((_ ((?var ?init ?step ...) ...)
	(?test ?expr ...)
	?form ...)
     (with-syntax (((ID ...) (map %parse-var (unwrap-syntax-object #'(?var ...)))))
       #'(let-syntax ((the-expr (syntax-rules ()
				  ((_)
				   (values))
				  ((_ ?-expr0 ?-expr (... ...))
				   (begin ?-expr0 ?-expr (... ...)))))
		      (the-step (syntax-rules ()
				  ((_ ?-var)
				   ?-var)
				  ((_ ?-var ?-step)
				   ?-step)
				  ((_ ?-var ?-step0 ?-step (... ...))
				   (syntax-violation 'do/with-class
				     "invalid step specification"
				     '(?-step0 ?-step (... ...)))))))
	   (let*/with-class ((?var ?init) ...)
	     (let/with-class loop ((?var ID) ...)
			     (if ?test
				 (the-expr ?expr ...)
			       (begin
				 ?form ...
				 (loop (the-step ID ?step ...) ...))))))
       ))))


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

(define-syntax defmethod
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?class (?method-name . ?args) ?body0 ?body ...)
       (with-syntax
	   ((FUNCNAME	(syntax-method-identifier #'?class #'?method-name))
	    (THIS	(datum->syntax #'?method-name 'this)))
	 ;;This output form must be kept in sync with the output form of
	 ;;DEFMETHOD-VIRTUAL below.
	 #'(define/with-class (FUNCNAME THIS . ?args)
	     (?class :with-class-bindings-of (#f #t #t #t #t) ;enable everything, but dot notation
	 	     THIS ?body0 ?body ...)))))))

(define-syntax defmethod-virtual
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?class (?method-name . ?args) ?body0 ?body ...)
       (with-syntax ((THIS (datum->syntax #'?method-name 'this)))
	 ;;Notice that  we cannot expand this syntax  using DEFMETHOD as
	 ;;something like:
	 ;;
	 ;; (begin
	 ;;   (defmethod ?class (?method-name . ?args) ?body0 ?body ...)
	 ;;   (define-virtual-method ?class ?method-name FUNCNAME))
	 ;;
	 ;;where FUNCNAME  is built with:
	 ;;
	 ;;  (syntax-method-identifier #'?class #'?method-name)
	 ;;
	 ;;this  is because  DEFMETHOD  and DEFINE-VIRTUAL-METHOD  would
	 ;;create a procedure  and a macro bound to  the same identifier
	 ;;"<class>-<method-name>": this cause an expand error.
	 ;;
	 ;;Also, it is not possible to expand the macro to:
	 ;;
	 ;; (begin
	 ;;   (defmethod ?class (the-method . ?args) ?body0 ?body ...)
	 ;;   (define-virtual-method ?class ?method-name FUNCNAME))
	 ;;
	 ;;where FUNCNAME  is built with:
	 ;;
	 ;;  (syntax-method-identifier #'?class #'the-method)
	 ;;
	 ;;because  the expander  renames "the-method"  before expanding
	 ;;DEFMETHOD, so the generated procedure name is not FUNCNAME.
	 ;;
	 ;;In   the   end,   our   only   option  is   to   repeat   the
	 ;;DEFINE/WITH-CLASS as  below, keeping it in sync  with the one
	 ;;in the definition of DEFMETHOD.
	 ;;
	 #'(begin
	     (define/with-class (the-method THIS . ?args)
	       (?class :with-class-bindings-of (#f #t #t #t #t) ;enable everything, but dot notation
		       THIS ?body0 ?body ...))
	     (define-virtual-method ?class ?method-name the-method))
	 )))))

;;; --------------------------------------------------------------------

(define-syntax* (lambda/with-class stx)
  (define (%parse-formals formals)
    (let loop ((formals	formals)
	       (args	'())	;list of formal identifiers
	       (cls	'()))	;list of lists of class identifiers
      (syntax-case formals (<top>)
	(()
	 (values (reverse args) (reverse cls) #f))
	(?rest
	 (identifier? #'?rest)
	 (values (reverse args) (reverse cls) #'?rest))
	((?car . ?cdr)
	 (identifier? #'?car)
	 (loop #'?cdr (cons #'?car args) (cons #'(<top>) cls)))
	(((?id ?cls0 ?cls ...) . ?cdr)
	 (all-identifiers? #'(?id ?cls0 ?cls ...))
	 (loop #'?cdr (cons #'?id args) (cons #'(?cls0 ?cls ...) cls)))
	(_
	 (synner "invalid formals in lambda definition" formals))
	)))

  (syntax-case stx ()

    ((_ ?identifier . ?body)
     (identifier? #'?identifier)
     #'(lambda ?identifier . ?body))

    ((_ ?formals . ?body)
     (let-values (((args cls rest) (%parse-formals #'?formals)))
       (with-syntax (((ARG ...)		args)
		     (((CLS ...) ...)	cls))
	 (if rest
	     #`(lambda (ARG ... . #,rest)
		 (with-class ((ARG CLS ...) ...) . ?body))
	   #'(lambda (ARG ...)
	       (with-class ((ARG CLS ...) ...) . ?body))))))

    (_
     (synner "invalid syntax in lambda definition" stx))
    ))

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

(define-foreign-class <builtin>
  (nongenerative nausicaa:builtin:<builtin>))

(define-syntax define-builtin-class
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?class-name ?clause ...)
       #`(define-foreign-class ?class-name
	   (inherit <builtin>)
	   (nongenerative #,(identifier-prefix "nausicaa:builtin:" #'?class-name))
	   ?clause ...)))))

(define-builtin-class <pair>
  (predicate pair?)
  (virtual-fields (immutable car car)
		  (immutable cdr cdr)))

(define-foreign-class <list>
  (nongenerative nausicaa:builtin:<list>)
  (inherit <pair>)
  (predicate list?)
  (virtual-fields (immutable length length))

  (method-syntax find
    (syntax-rules ()
      ((_ o proc)
       (find proc o))))

  (method-syntax for-all
    (syntax-rules ()
      ((_ o proc)
       (for-all proc o))))

  (method-syntax exists
    (syntax-rules ()
      ((_ o proc)
       (exists proc o))))

  (method-syntax filter
    (syntax-rules ()
      ((_ o proc)
       (filter proc o))))

  (method-syntax partition
    (syntax-rules ()
      ((_ o proc)
       (partition proc o))))

  (method-syntax fold-left
    (syntax-rules ()
      ((_ o nil proc)
       (fold-left proc nil o))))

  (method-syntax fold-right
    (syntax-rules ()
      ((_ o nil proc)
       (fold-right proc nil o))))

  (method-syntax remp
    (syntax-rules ()
      ((_ o proc)
       (remp proc o))))

  (method-syntax remove
    (syntax-rules ()
      ((_ o proc)
       (remove proc o))))

  (method-syntax remv
    (syntax-rules ()
      ((_ o proc)
       (remv proc o))))

  (method-syntax remq
    (syntax-rules ()
      ((_ o proc)
       (remq proc o))))

  (method-syntax memp
    (syntax-rules ()
      ((_ o proc)
       (memp proc o))))

  (method-syntax member
    (syntax-rules ()
      ((_ o proc)
       (member proc o))))

  (method-syntax memv
    (syntax-rules ()
      ((_ o proc)
       (memv proc o))))

  (method-syntax memq
    (syntax-rules ()
      ((_ o proc)
       (memq proc o))))

  (method-syntax assp
    (syntax-rules ()
      ((_ o proc)
       (assp proc o))))

  (method-syntax assoc
    (syntax-rules ()
      ((_ o proc)
       (assoc proc o))))

  (method-syntax assv
    (syntax-rules ()
      ((_ o proc)
       (assv proc o))))

  (method-syntax assq
    (syntax-rules ()
      ((_ o proc)
       (assq proc o))))
  )

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

(define-foreign-class <input-port>
  (inherit <port>)
  (predicate input-port?)
  (nongenerative nausicaa:builtin:<input-port>))

(define-foreign-class <output-port>
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

(define-foreign-class <binary-port>
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

(define-foreign-class <textual-port>
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

(define-foreign-class <complex>
  (inherit <number>)
  (predicate complex?)
  (virtual-fields (immutable real-part	real-part)
		  (immutable imag-part	imag-part)
		  (immutable magnitude	magnitude)
		  (immutable angle	angle))
  (nongenerative nausicaa:builtin:<complex>))

(define-foreign-class <real-valued>
  (inherit <complex>)
  (predicate real-valued?)
  (virtual-fields (immutable positive?		positive?)
		  (immutable negative?		negative?)
		  (immutable non-positive?	non-positive?)
		  (immutable non-negative?	non-negative?))
  (nongenerative nausicaa:builtin:<real-valued>))

(define-foreign-class <real>
  (inherit <real-valued>)
  (predicate real?)
  (nongenerative nausicaa:builtin:<real>)
  (virtual-fields (immutable abs)))

(define-foreign-class <rational-valued>
  (inherit <real>)
  (predicate rational-valued?)
  (nongenerative nausicaa:builtin:<rational-valued>))

(define-foreign-class <flonum>
  (inherit <real>)
  (predicate flonum?)
  (nongenerative nausicaa:builtin:<flonum>))

(define-foreign-class <rational>
  (inherit <rational-valued>)
  (predicate rational?)
  (nongenerative nausicaa:builtin:<rational>))

(define-foreign-class <integer-valued>
  (inherit <rational-valued>)
  (predicate integer-valued?)
  (nongenerative nausicaa:builtin:<integer-valued>))

(define-foreign-class <integer>
  (inherit <integer-valued>)
  (predicate integer?)
  (nongenerative nausicaa:builtin:<integer>))

(define-foreign-class <fixnum>
  (inherit <integer>)
  (predicate fixnum?)
  (nongenerative nausicaa:builtin:<fixnum>))


;;;; done

)

;;; end of file
