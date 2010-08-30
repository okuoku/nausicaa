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
    (auxiliary-syntaxes)
    (classes top))


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

(define (%vector-copy dst src len)
  (do ((i 0 (+ 1 i)))
      ((= i len))
    (vector-set! dst i (vector-ref src i))))


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

(define-syntax make*
  ;;Build a new class instance using the maker constructor.
  ;;
  (syntax-rules ()
    ((_ ?class-name ?arg ...)
     (?class-name make* ?arg ...))))

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


(define-syntax define-foreign-class
  ;;A foreign class  is just a tag  we slap on any value  to use virtual
  ;;fields  and methods  with dot  notation,  but nevertheless  it is  a
  ;;proper record type.
  ;;
  (syntax-rules ()
    ((_ ?name ?clause ...)
     (%define-foreign-class (define-class ?name ?clause ...) ?name () ?clause ...))))

(define-syntax %define-foreign-class
  ;;Raise an error if  a PUBLIC-PROTOCOL, MAKER-PROTOCOL or FIELD clause
  ;;is present in the body of the definition; else define the class with
  ;;DEFINE-CLASS specifying a public protocol which raises an error when
  ;;invoked.
  ;;
  (lambda (stx)
    (syntax-case stx ()

      ;;no more clauses to collect
      ((_ ?input-form ?name (?collected-clause ...))
       #'(define-class ?name
	   (public-protocol (lambda (make-parent)
			      (lambda args
				(syntax-violation #f
				  "attempt to instantiate foreign class" (quote ?name)))))
	   ?collected-clause ...))

      ;;found PUBLIC-PROTOCOL clause
      ((_ ?input-form ?name (?collected-clause ...) (?keyword ?pro ...) ?clause ...)
       (free-identifier=? #'?keyword #'public-protocol)
       (syntax-violation 'define-class
	 "public-protocol clause used in definition of foreign class"
	 (syntax->datum stx)
	 (syntax->datum #'(public-protocol ?pro ...))))

      ;;found MAKER-PROTOCOL clause
      ((_ ?input-form ?name (?collected-clause ...) (?keyword ?pro ...) ?clause ...)
       (free-identifier=? #'?keyword #'maker-protocol)
       (syntax-violation 'define-class
	 "maker-protocol clause used in definition of foreign class"
	 (syntax->datum #'?input-form)
	 (syntax->datum #'(public-protocol ?pro ...))))

      ;;found FIELDS clause
      ((_ ?input-form ?name (?collected-clause ...) (?keyword ?fie ...) ?clause ...)
       (free-identifier=? #'?keyword #'fields)
       (syntax-violation 'define-class
	 "fields clause used in definition of foreign class"
	 (syntax->datum #'?input-form)
	 (syntax->datum #'(fields ?fie ...))))

      ;;other clauses
      ((_ ?input-form ?name (?collected-clause ...) ?clause0 ?clause ...)
       #'(%define-foreign-class ?input-form ?name (?collected-clause ... ?clause0) ?clause ...))

      )))


(define-syntax define-class
  (lambda (stx)
    (define (generate-field-indexes number-of-fields)
      ;;Derived  from   IOTA  from  (lists);  generate   a  sequence  of
      ;;non-negative exact integers to be  used as indexes in the vector
      ;;of concrete fields.
      ;;
      (do ((count number-of-fields (- count 1))
	   (val (- number-of-fields 1) (- val 1))
	   (ret '() (cons val ret)))
	  ((<= count 0)
	   ret)))

    (define (doit input-form class-identifier constructor-identifier predicate-identifier clauses)
      ;;Parse the definition clauses and generate the output forms.
      ;;
      (define (%synner msg subform)
	(syntax-violation 'define-class
	  (string-append msg " in class definition")
	  (syntax->datum input-form)
	  (syntax->datum subform)))

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

	   ;;A syntax  object holding  an expression which  evaluates to
	   ;;the class' common protocol function.
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

	   ;;Null/null  or  a   list  of  identifiers  representing  the
	   ;;positional  arguments to  the  maker and  a  list of  maker
	   ;;clauses representing optional arguments.
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

	   ;;Set to  PREDICATE-IDENTIFIER or an  identifier representing
	   ;;the custom predicate for the class.
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
	   ((methods definitions)
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
	(set! definitions	(append definitions syntax-definitions))

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
	  (set! parent-rtd	#`(#,superclass-identifier class-record-type-descriptor))
	  (set! parent-cd	#`(#,superclass-identifier superclass-constructor-descriptor)))

	 ;;The PARENT clause is present.
	 ((and parent-name
	       (not superclass-identifier)
	       (not parent-rtd)
	       (not parent-cd))
	  (set! superclass-identifier #'<top>-superclass)
	  (set! parent-rtd	#`(record-type-descriptor        #,parent-name))
	  (set! parent-cd	#`(record-constructor-descriptor #,parent-name)))

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
	    ((CLASS-NAME			class-identifier)
	     (SUPERCLASS-NAME			superclass-identifier)
	     (THE-PARENT-IS-A-CLASS?		the-parent-is-a-class?)
	     (PARENT-RTD			parent-rtd)
	     (PARENT-CD				parent-cd)
	     (UID				uid-symbol)
	     (SEALED				sealed)
	     (OPAQUE				opaque)
	     (COMMON-PROTOCOL			common-protocol)
	     (PUBLIC-PROTOCOL			public-protocol)
	     (MAKER-PROTOCOL			maker-protocol)
	     (SUPERCLASS-PROTOCOL		superclass-protocol)
	     (CONSTRUCTOR-IDENTIFIER		constructor-identifier)
	     ((MAKER-POSITIONAL-ARG ...)	maker-positional-args)
	     ((MAKER-OPTIONAL-ARG ...)		maker-optional-args)
	     (PREDICATE-IDENTIFIER		predicate-identifier)
	     (CUSTOM-PREDICATE			custom-predicate)
	     ((DEFINITION ...)			definitions)
	     (INHERIT-CONCRETE-FIELDS?		inherit-concrete-fields?)
	     (INHERIT-VIRTUAL-FIELDS?		inherit-virtual-fields?)
	     (INHERIT-METHODS?			inherit-methods?)
	     (INHERIT-SETTER-AND-GETTER?	inherit-setter-and-getter?)
	     (SETTER				setter)
	     (GETTER				getter)
	     (BINDINGS-MACRO			bindings-macro)
	     (((METHOD METHOD-IDENTIFIER) ...)	methods)

	     (((MUTABILITY FIELD ACCESSOR/MUTATOR ...)
	       ...)
	      fields)
	     (((VIRTUAL-MUTABILITY VIRTUAL-FIELD VIRTUAL-ACCESSOR/MUTATOR ...) ...)
	      virtual-fields)

	     ((FIELD-INDEX ...)			(generate-field-indexes (length fields))))

	  #'(begin
	      (define the-parent-rtd		PARENT-RTD)
	      (define the-parent-cd		PARENT-CD)

	      (define the-rtd
		(make-record-type-descriptor (quote CLASS-NAME) the-parent-rtd
					     (quote UID) SEALED OPAQUE
					     (quote #((MUTABILITY FIELD) ...))))

	      (%define-class/output-forms/fields-accessors-and-mutators
	       the-rtd (FIELD-INDEX ...) (MUTABILITY FIELD ACCESSOR/MUTATOR ...) ...)

	      DEFINITION ...

	      (define from-fields-protocol
		(%make-default-protocol the-rtd))
	      (define the-from-fields-cd
		(%define-class/output-forms/make-from-fields-constructor-descriptor
		 THE-PARENT-IS-A-CLASS? SUPERCLASS-NAME the-rtd from-fields-protocol))
	      (define from-fields-constructor
		(record-constructor the-from-fields-cd))

	      (define the-common-protocol
		(or COMMON-PROTOCOL from-fields-protocol))

	      (define the-public-protocol
		(or PUBLIC-PROTOCOL the-common-protocol))
	      (define the-public-cd
		(make-record-constructor-descriptor the-rtd the-parent-cd the-public-protocol))
	      (define CONSTRUCTOR-IDENTIFIER
		(record-constructor the-public-cd))

	      (define the-maker-protocol
		MAKER-PROTOCOL)
	      (define the-maker-cd
		(if the-maker-protocol
		    (make-record-constructor-descriptor the-rtd the-parent-cd the-maker-protocol)
		  the-public-cd))
	      (define maker-constructor
		(record-constructor the-maker-cd))

	      (define the-superclass-protocol
		(or SUPERCLASS-PROTOCOL the-common-protocol))
	      (define the-superclass-cd
		(make-record-constructor-descriptor the-rtd the-parent-cd the-superclass-protocol))
	      (define superclass-constructor
		(record-constructor the-superclass-cd))

	      (define PREDICATE-IDENTIFIER	(record-predicate the-rtd))

	      (define the-parent-uid-list
		(cons (quote UID) (if the-parent-rtd
				       (map record-type-uid (record-parent-list the-parent-rtd))
				     '())))

	      (define (the-parent-rtd-list)
		(cons the-rtd (if the-parent-rtd
				  (record-parent-list the-parent-rtd)
				'())))

	      (define-syntax CLASS-NAME
	      	(lambda (stx)
	      	  (syntax-case stx (class-record-type-descriptor
	      			    class-type-uid
	      			    class-uid-list
	      			    parent-rtd-list
	      			    public-constructor-descriptor
	      			    superclass-constructor-descriptor
	      			    from-fields-constructor-descriptor
				    superclass-protocol
				    list-of-concrete-fields
				    list-of-virtual-fields
				    list-of-methods
	      			    make make* make-from-fields is-a?
	      			    with-class-bindings-of)

	      	    ((_ class-record-type-descriptor)
	      	     #'the-rtd)

	      	    ((_ class-type-uid)
	      	     #'(quote UID))

	      	    ((_ class-uid-list)
	      	     #'the-parent-uid-list)

	      	    ((_ parent-rtd-list)
	      	     #'(the-parent-rtd-list))

		    ;; --------------------------------------------------

	      	    ((_ public-constructor-descriptor)
	      	     #'the-public-cd)

	      	    ((_ superclass-constructor-descriptor)
	      	     #'the-superclass-cd)

	      	    ((_ from-fields-constructor-descriptor)
	      	     #'the-from-fields-cd)

		    ((_ superclass-protocol)
		     #'the-superclass-protocol)

		    ;; --------------------------------------------------

		    ((_ list-of-concrete-fields)
		     #'((MUTABILITY FIELD ACCESSOR/MUTATOR ...) ...))

		    ((_ list-of-virtual-fields)
		     #'((VIRTUAL-MUTABILITY VIRTUAL-FIELD VIRTUAL-ACCESSOR/MUTATOR ...) ...))

		    ((_ list-of-methods)
		     #'((METHOD METHOD-IDENTIFIER) ...))

		    ;; --------------------------------------------------

	      	    ((_ make ?arg (... ...))
	      	     #'(CONSTRUCTOR-IDENTIFIER ?arg (... ...)))

	      	    ((_ make* ?arg (... ...))
	      	     #'(the-maker ?arg (... ...)))

	      	    ((_ make-from-fields ?arg (... ...))
	      	     #'(from-fields-constructor ?arg (... ...)))

	      	    ((_ is-a? ?arg)
	      	     #'(CUSTOM-PREDICATE ?arg))

	      	    ((_ with-class-bindings-of
	      		(?inherit-concrete-fields
	      		 ?inherit-virtual-fields
	      		 ?inherit-methods
	      		 ?inherit-setter-and-getter)
	      		?variable-name ?arg (... ...))
	      	     (for-all boolean? (syntax->datum #'(?inherit-concrete-fields
	      						 ?inherit-virtual-fields
	      						 ?inherit-methods
	      						 ?inherit-setter-and-getter)))
	      	     #'(with-class-bindings
	      		(?inherit-concrete-fields
	      		 ?inherit-virtual-fields
	      		 ?inherit-methods
	      		 ?inherit-setter-and-getter)
	      		?variable-name ?arg (... ...)))

	      	    ((_ ?keyword . ?rest)
	      	     (syntax-violation 'CLASS-NAME
	      	       "invalid class internal keyword"
	      	       (syntax->datum stx)
	      	       (syntax->datum #'?keyword)))
	      	    )))

	      (%define-class/output-forms/maker CLASS-NAME the-maker maker-constructor
						(MAKER-POSITIONAL-ARG ...)
						(MAKER-OPTIONAL-ARG ...))

	      (define-syntax with-class-bindings
	      	(syntax-rules ()
	      	  ((_ (?inherit-concrete-fields
	      	       ?inherit-virtual-fields
	      	       ?inherit-methods
	      	       ?inherit-setter-and-getter)
	      	      ?variable-name ?body0 ?body (... ...))
	      	   (SUPERCLASS-NAME with-class-bindings-of (INHERIT-CONCRETE-FIELDS?
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
	      				(BINDINGS-MACRO ?class-name ?variable-name
	      						?body0 ?body (... ...))))))))
	      	  ))

	      (define-syntax with-class-bindings/concrete-fields
	      	(lambda (stx)
	      	  (syntax-case stx ()
	      	    ((_ ?inherit-concrete-fields ?variable-name . ?body)
	      	     (syntax->datum #'?inherit-concrete-fields)
	      	     #'(%with-class-fields ?variable-name
	      				   ((MUTABILITY FIELD ACCESSOR/MUTATOR ...) ...)
	      				   . ?body))
	      	    ((_ ?inherit-fields ?variable-name . ?body)
	      	     #'(begin . ?body))
	      	    )))

	      (define-syntax with-class-bindings/virtual-fields
	      	(lambda (stx)
	      	  (syntax-case stx ()
	      	    ((_ ?inherit-virtual-fields ?variable-name . ?body)
	      	     (syntax->datum #'?inherit-virtual-fields)
	      	     #'(%with-class-fields ?variable-name
	      				   ((VIRTUAL-MUTABILITY VIRTUAL-FIELD
								VIRTUAL-ACCESSOR/MUTATOR ...) ...)
	      				   . ?body))
	      	    ((_ ?inherit-fields ?variable-name . ?body)
	      	     #'(begin . ?body))
	      	    )))

	      (define-syntax with-class-bindings/methods
	      	(lambda (stx)
	      	  (syntax-case stx ()
	      	    ((_ ?inherit-methods ?variable-name . ?body)
	      	     (syntax->datum #'?inherit-methods)
	      	     #'(%with-class-methods ?variable-name ((METHOD METHOD-IDENTIFIER) ...) . ?body))
	      	    ((_ ?inherit-methods ?variable-name . ?body)
	      	     #'(begin . ?body))
	      	    )))

	      (define-syntax with-class-bindings/setter-and-getter
	      	(lambda (stx)
	      	  (syntax-case stx ()
	      	    ((_ ?inherit-setter-and-getter ?variable-name . ?body)
	      	     (syntax->datum #'?inherit-setter-and-getter)
	      	     #'(%with-class-setter-and-getter ?variable-name SETTER GETTER . ?body))
	      	    ((_ ?inherit-setter-and-getter ?variable-name . ?body)
	      	     #'(begin . ?body))
	      	    )))

	      ))))

    (syntax-case stx (fields mutable immutable parent sealed opaque parent-rtd nongenerative
			     virtual-fields methods method predicate setter getter inherit
			     bindings)

      ((_ (?name ?constructor ?predicate) ?clause ...)
       (all-identifiers? #'(?name ?constructor ?predicate))
       (doit stx #'?name #'?constructor #'?predicate (unwrap-syntax-object #'(?clause ...))))

      ((_ ?name ?clause ...)
       (identifier? #'?name)
       (doit stx #'?name
	     (syntax-maker-identifier #'?name)
	     (syntax-predicate-identifier #'?name)
	     (unwrap-syntax-object #'(?clause ...))))

      ((_ ?name-spec . ?clauses)
       (syntax-violation 'define-class
	 "invalid name specification in class definition"
	 (syntax->datum stx)
	 (syntax->datum #'?name-spec)))
      )))


(define-syntax %define-class/output-forms/fields-accessors-and-mutators
  ;;Subroutine of  DEFINE-CLASS which expands to the  definitions of the
  ;;class' concrete field accessors and mutators.
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

(define-syntax %define-class/output-forms/maker
  (syntax-rules ()
    ((_ class-name the-maker constructor () ())
     (define-syntax the-maker
       (lambda (stx)
	 (syntax-case stx ()
	   ((_ . ?args)
	    (syntax-violation (quote class-name)
	      "no maker was defined for this class"
	      (syntax->datum #'(make* class-name . ?args)) #f))))))

    ((_ class-name the-maker constructor (MAKER-POSITIONAL-ARG ...) (MAKER-OPTIONAL-ARG ...))
     (define-maker (the-maker MAKER-POSITIONAL-ARG ...)
       constructor (MAKER-OPTIONAL-ARG ...)))))

(define-syntax %define-class/output-forms/make-from-fields-constructor-descriptor
  ;;We do  this to  allow inheritance from  non-class record  types.  We
  ;;select the  appropriate clause  to make the  construction efficient:
  ;;the constructor for  non-class record types is slow  compared to the
  ;;one of class types.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ #t ?superclass-name ?the-rtd ?from-fields-protocol)
       (identifier? #'?superclass-name)
       #'(make-record-constructor-descriptor ?the-rtd
					     (?superclass-name from-fields-constructor-descriptor)
					     ?from-fields-protocol))
      ((_ #f ?superclass-name ?the-rtd ?from-fields-protocol)
       #'(%make-from-fields-cd ?the-rtd ?from-fields-protocol))
      (_
       (syntax-violation 'make-from-fields-constructor-descriptor
	 "invalid arguments"
	 (syntax->datum stx))))))


(define-syntax define-label
  ;;A label is just a tag we slap on any value to use virtual fields and
  ;;methods with dot notation, it  has NO record type.  Labels canNOT be
  ;;used in the inheritance hierarchy of classes.
  ;;
  (lambda (stx)
    (define (doit input-form label-identifier predicate-identifier clauses)
      ;;Parse the definition clauses and generate the output forms.
      ;;
      (define (%synner msg subform)
	(syntax-violation 'define-label
	  (string-append msg " in label definition")
	  (syntax->datum input-form)
	  (syntax->datum subform)))

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

	(set! methods		(append methods methods-from-methods syntax-methods))
	(set! definitions	(append definitions syntax-definitions))

	(let ((id (duplicated-identifiers? (append (map cadr virtual-fields)
						   (map car  methods)))))
	  (when id
	    (%synner "duplicated field names" id)))

	(with-syntax ((LABEL-NAME			label-identifier)
		      (SUPERLABEL-IDENTIFIER		superlabel-identifier)
		      (PREDICATE			predicate-identifier)
		      (CUSTOM-PREDICATE			custom-predicate)
		      ((DEFINITION ...)			definitions)
		      (INHERIT-VIRTUAL-FIELDS?		inherit-virtual-fields?)
		      (INHERIT-METHODS?			inherit-methods?)
		      (INHERIT-SETTER-AND-GETTER?	inherit-setter-and-getter?)
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
		  (or p (lambda (x)
			  (assertion-violation (quote PREDICATE)
			    "no predicate definition for label"
			    (quote ?input-form))))))

	      DEFINITION ...

	      (define-syntax LABEL-NAME
		(lambda (stx)
		  (syntax-case stx (is-a? with-class-bindings-of)

		    ((_ is-a? ?arg)
		     #'(PREDICATE ?arg))

		    ((_ with-class-bindings-of
			(?inherit-concrete-fields ;this comes from WITH-CLASS
			 ?inherit-virtual-fields
			 ?inherit-methods
			 ?inherit-setter-and-getter)
			?variable-name ?arg (... ...))
		     (for-all boolean? (syntax->datum #'(?inherit-concrete-fields
							 ?inherit-virtual-fields
							 ?inherit-methods
							 ?inherit-setter-and-getter)))
		     #'(with-label-bindings
			(?inherit-virtual-fields
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
		  ((_ (?inherit-virtual-fields ?inherit-methods ?inherit-setter-and-getter)
		      ?variable-name ?body0 ?body (... ...))
		   (SUPERLABEL-IDENTIFIER
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
		       (BINDINGS-MACRO LABEL-NAME ?variable-name
				       ?body0 ?body (... ...)))))))
		  ))

	      (define-syntax with-label-bindings/virtual-fields
		(lambda (stx)
		  (syntax-case stx ()
		    ((_ ?inherit-virtual-fields ?variable-name . ?body)
		     (syntax->datum #'?inherit-virtual-fields)
		     #'(%with-class-fields ?variable-name
					   ((VIRTUAL-MUTABILITY VIRTUAL-FIELD VIRTUAL-ACCESSOR/MUTATOR ...)
					    ...)
					   . ?body))
		    ((_ ?inherit-fields ?variable-name . ?body)
		     #'(begin . ?body))
		    )))

	      (define-syntax with-label-bindings/methods
		(lambda (stx)
		  (syntax-case stx ()
		    ((_ ?inherit-methods ?variable-name . ?body)
		     (syntax->datum #'?inherit-methods)
		     #'(%with-class-methods ?variable-name ((METHOD METHOD-FUNCTION) ...)
					    . ?body))
		    ((_ ?inherit-methods ?variable-name . ?body)
		     #'(begin . ?body))
		    )))

	      (define-syntax with-label-bindings/setter-and-getter
		(lambda (stx)
		  (syntax-case stx ()
		    ((_ ?inherit-setter-and-getter ?variable-name . ?body)
		     (syntax->datum #'?inherit-setter-and-getter)
		     #'(%with-class-setter-and-getter ?variable-name SETTER GETTER . ?body))
		    ((_ ?inherit-setter-and-getter ?variable-name . ?body)
		     #'(begin . ?body))
		    )))

	      ))))

    (syntax-case stx ()
      ((_ (?name ?predicate) ?clause ...)
       (all-identifiers? #'(?name ?predicate))
       (doit stx #'?name #'?predicate
	     (unwrap-syntax-object #'(?clause ...))))

      ((_ ?name ?clause ...)
       (identifier? (syntax ?name))
       (doit stx #'?name
	     (syntax-predicate-identifier #'?name)
	     (unwrap-syntax-object #'(?clause ...))))

      ((_ ?name-spec . ?clauses)
       (syntax-violation 'define-label
	 "invalid name specification in label definition"
	 (syntax->datum (syntax ?input-form))
	 (syntax->datum (syntax ?name-spec))))
      )))


(define $virtual-methods-table
  (make-eq-hashtable))

(define-syntax define-virtual-method
  (lambda (stx)
    (define who 'define-virtual-method)
    (syntax-case stx ()

      ((_ ?class ?name)
       #'(define-virtual-method ?class ?name #f))

      ((_ ?class ?name ?lambda)
       (not (identifier? #'?class))
       (syntax-violation who "expected class identifier as first argument" (syntax->datum stx)))

      ((_ ?class ?name ?lambda)
       (not (identifier? #'?name))
       (syntax-violation who "expected method name identifier as second argument" (syntax->datum stx)))

      ((_ ?class ?name ?lambda)
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
	       ;;This  is  a  method,  so  we  know  that  ?self  is  an
	       ;;identifier: we can safely use it multiple times.
	       ((_ ?self . ?args)
		((%retrieve-virtual-method-implementation the-table (record-type-of ?self) '?name)
		 ?self . ?args))))
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
  (let next-rtd ((rtd rtd))
    (if rtd
	(let ((uid (record-type-uid rtd)))
	  (if (eq? uid 'nausicaa:builtin:<top>)
	      (%error-missing-implementation)
	    (or (hashtable-ref method-table uid #f)
		(next-rtd (record-type-parent rtd)))))
      (%error-missing-implementation))))


(define-syntax %with-class-fields
  ;;Handle  access to  fields, both  concrete and  virtual;  expand into
  ;;nested uses of WITH-ACCESSOR-AND-MUTATOR from (language-extensions).
  ;;
  (lambda (stx)
    (syntax-case stx ()

      ;;Process a field clause with both accessor and mutator.
      ((_ ?variable-name ((?mutable ?field ?accessor ?mutator) ?clause ...) . ?body)
       (and (identifier? #'?mutable) (free-identifier=? #'?mutable #'mutable)
	    (identifier? #'?variable-name)
	    (identifier? #'?field)
	    (identifier? #'?accessor)
	    (identifier? #'?mutator))
       #`(with-accessor-and-mutator ((#,(syntax-dot-notation-identifier #'?variable-name #'?field)
       				      ?variable-name ?accessor ?mutator))
       				    (%with-class-fields ?variable-name (?clause ...) . ?body)))

      ;;Process a field clause with accessor only.
      ((_ ?variable-name ((?immutable ?field ?accessor) ?clause ...) . ?body)
       (and (identifier? #'?immutable) (free-identifier=? #'?immutable #'immutable)
	    (identifier? #'?variable-name)
	    (identifier? #'?field)
	    (identifier? #'?accessor))
       #`(with-accessor-and-mutator ((#,(syntax-dot-notation-identifier #'?variable-name #'?field)
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
					  (syntax-dot-notation-identifier #'?variable-name method/stx))
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
       (and (identifier? #'?var) (identifier? #'?class0) (free-identifier=? #'?class0 #'<top>))
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

;;This is used below to distinguish between named-LET and ordinary LET.
(define no-loop)

(define-syntax %do-let/no-types
  ;;Produce the  output forms for a  LET form in which  all the bindings
  ;;are untyped.  Hand the rest to %DO-LET/ADD-TOP.
  ;;
  (lambda (stx)
    (syntax-case stx (no-loop)

      ;;Ordinary LET, no bindings.  Expand to ?LET to allow <definition>
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
       #'(%do-let/add-top ?let ?let/with-class ?loop () ((?var ?init) ...) ?body0 ?body ...))

      (_
       (syntax-violation '%do-let/no-types "invalid input form" (syntax->datum stx)))
      )))

(define-syntax %do-let/add-top
  ;;Add <top>  to the  bindings with no  type.  Then hand  everything to
  ;;?LET/WITH-CLASS.
  ;;
  (lambda (stx)
    (syntax-case stx ()

      ;;No more bindings to collect.
      ((_ ?let ?let/with-class ?loop (?bind ...) () ?body0 ?body ...)
       #'(?let/with-class ?loop (?bind ...) ?body0 ?body ...))

      ;;Add <top> as type to an untyped binding.
      ((_ ?let ?let/with-class ?loop (?bind ...) ((?var0 ?init0) (?var ?init) ...) ?body0 ?body ...)
       (identifier? #'?var0)
       #'(%do-let/add-top ?let ?let/with-class ?loop
			  (?bind ... ((?var0 <top>) ?init0))
			  ((?var ?init) ...)
			  ?body0 ?body ...))

      ;;Add <top> as type to an untyped binding.
      ((_ ?let ?let/with-class ?loop (?bind ...) (((?var0) ?init0) (?var ?init) ...) ?body0 ?body ...)
       (identifier? #'?var0)
       #'(%do-let/add-top ?let ?let/with-class ?loop
			  (?bind ... ((?var0 <top>) ?init0))
			  ((?var ?init) ...)
			  ?body0 ?body ...))

      ;;Collect a typed binding.
      ((_ ?let ?let/with-class ?loop (?bind ...)
	  (((?var0 ?class0 ?class ...) ?init0) (?var ?init) ...) ?body0 ?body ...)
       #'(%do-let/add-top ?let ?let/with-class ?loop
			  (?bind ... ((?var0 ?class0 ?class ...) ?init0))
			  ((?var ?init) ...)
			  ?body0 ?body ...))

      (_
       (syntax-violation '%do-let/add-top "invalid input form" (syntax->datum stx)))
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

(define-syntax %let/with-class
  ;;Produce output forms for LET with types.
  ;;
  (lambda (stx)
    (syntax-case stx (no-loop)

      ((_ no-loop (((?var ?class0 ?class ...) ?init) ...) ?body0 ?body ...)
       #'(let ((?var ?init) ...)
	   (with-class ((?var ?class0 ?class ...) ...) ?body0 ?body ...)))

      ((_ ?loop (((?var ?class0 ?class ...) ?init) ...) ?body0 ?body ...)
       #'(let ?loop ((?var ?init) ...)
	   (with-class ((?var ?class0 ?class ...) ...) ?body0 ?body ...)))

      (_
       (syntax-violation '%let/with-class "invalid input form" (syntax->datum stx)))
      )))

;;; --------------------------------------------------------------------

(define-syntax let*/with-class
  ;;Entry point for LET* with types.
  ;;
  (lambda (stx)
    (syntax-case stx (no-loop)
      ((_ ((?var ?init) ...) ?body0 ?body ...)
       #'(%do-let/no-types let* %let*/with-class no-loop ((?var ?init) ...) ?body0 ?body ...))
      (_
       (syntax-violation 'let*/with-class "invalid input form" (syntax->datum stx)))
      )))

(define-syntax %let*/with-class
  ;;Produce output forms for LET* with types.
  ;;
  (lambda (stx)
    (syntax-case stx (no-loop)

      ((_ no-loop (((?var0 ?class0 ?class00 ...) ?init0)
		   ((?var1 ?class1 ?class11 ...) ?init1)
		   ...)
	  ?body0 ?body ...)
       (let ((id (duplicated-identifiers? #'(?var0 ?var1 ...))))
	 (if id
	     (syntax-violation 'let*/with-class "duplicated binding names"
	       (syntax->datum stx) (syntax->datum id))
	   #'(let ((?var0 ?init0))
	       (with-class ((?var0 ?class0 ?class00 ...))
		 (let*/with-class (((?var1 ?class1 ?class11 ...) ?init1) ...) ?body0 ?body ...))))))

      ((_ no-loop () ?body0 ?body ...)
       #'(begin ?body0 ?body ...))

      ((_ ?loop . ?rest)
       (syntax-violation 'let*/with-class "named LET* is not allowed" (syntax->datum stx)))

      (_
       (syntax-violation '%let*/with-class "invalid input form" (syntax->datum stx)))
      )))

;;; --------------------------------------------------------------------

(define-syntax letrec/with-class
  ;;Entry point for LETREC with types.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((?var ?init) ...) ?body0 ?body ...)
       #'(%do-let/no-types letrec %letrec/with-class no-loop ((?var ?init) ...) ?body0 ?body ...))
      (_
       (syntax-violation 'letrec/with-class "invalid input form" (syntax->datum stx)))
      )))

(define-syntax %letrec/with-class
  ;;Produce output forms for LETREC with types.
  ;;
  (lambda (stx)
    (syntax-case stx (no-loop)

      ((_ no-loop (((?var ?class0 ?class ...) ?init)
		   ...)
	  ?body0 ?body ...)
       ;;We rely on LET to detect duplicated bindings.
       (with-syntax (((T ...) (generate-temporaries #'(?var ...))))
	 #'(let ((?var sentinel.undefined) ...)
	     (with-class ((?var ?class0 ?class ...) ...)
	       (let ((T ?init) ...)
		 (set! ?var T) ...
		 ?body0 ?body ...)))))

      ((_ ?loop . ?rest)
       (syntax-violation 'letrec/with-class "named LETREC is not allowed" (syntax->datum stx)))

      (_
       (syntax-violation '%letrec/with-class "invalid input form" (syntax->datum stx)))
      )))

;;; --------------------------------------------------------------------

(define-syntax letrec*/with-class
  ;;Entry point for LETREC* with types.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((?var ?init) ...) ?body0 ?body ...)
       #'(%do-let/no-types letrec* %letrec*/with-class no-loop ((?var ?init) ...) ?body0 ?body ...))
      (_
       (syntax-violation 'letrec*/with-class "invalid input form" (syntax->datum stx)))
      )))

(define-syntax %letrec*/with-class
  ;;Produce output forms for LETREC* with types.
  ;;
  ;;The  difference between  LETREC and  LETREC*  is only  the order  of
  ;;evaluation of ?INIT, which is enforced in LETREC*.
  ;;
  (lambda (stx)
    (syntax-case stx (no-loop)

      ((_ no-loop (((?var ?class0 ?class ...) ?init) ...) ?body0 ?body ...)
       ;;We rely on LET to detect duplicated bindings.
       #'(let ((?var #f) ...)
	   (with-class ((?var ?class0 ?class ...) ...)
	     (set! ?var ?init) ...
	     ?body0 ?body ...)))

      ((_ ?loop . ?rest)
       (syntax-violation 'letrec*/with-class "named LETREC* is not allowed" (syntax->datum stx)))

      (_
       (syntax-violation '%letrec*/with-class "invalid input form" (syntax->datum stx)))
      )))


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
    ;;identifier, example: (lambda args ---).
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
