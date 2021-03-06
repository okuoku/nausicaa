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
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa language classes)
  (export

    ;; usage macros
    define-class			;;define-foreign-class
    define-label			define-mixin
    make				make-from-fields
    make*				is-a?
    define-virtual-method
    defmethod				defmethod-virtual
    slot-ref				slot-set!

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
    with-class				class-case
    setf				getf
    define/with-class			lambda/with-class
    case-lambda/with-class		receive/with-class
    let/with-class			let*/with-class
    letrec/with-class			letrec*/with-class
    do/with-class			do*/with-class

    let-make				let*-make
    let-make*				let*-make*
    letrec-make				letrec*-make
    letrec-make*			letrec*-make*

    ;;Exporting  this is  needed by  Racket 5.1  even though  it  is not
    ;;needed for the library (nausicaa) to reexport it.
    with-field-class

    ;; auxiliary syntaxes
    parent sealed opaque parent-rtd nongenerative
    protocol fields mutable immutable
    inherit predicate maker maker-transformer custom-maker
    setter getter bindings
    public-protocol maker-protocol superclass-protocol
    virtual-fields methods method method-syntax
    <> mixins satisfies

    ;; builtin classes
    <top> <builtin> <pair> <list>
    <char> <string> <vector> <bytevector> <hashtable> <condition>
    <port> <binary-port> <input-port> <output-port> <textual-port>
    <fixnum> <flonum> <integer> <integer-valued> <rational> <rational-valued>
    <real> <real-valued> <complex> <number>)
  (import (rnrs)
;;;(nausicaa language pretty-print)
    (for (prefix (only (nausicaa language syntax-utilities)
		       all-identifiers?
		       duplicate-identifiers?
		       identifier-prefix
		       syntax-dot-notation-identifier
		       syntax-maker-identifier
		       syntax-method-identifier
		       syntax-predicate-identifier
		       syntax->list
		       unwrap)
		 sx.)
	 expand)
    (for (prefix (nausicaa language classes helpers) help.)	expand)
    (for (nausicaa language classes binding-makers)		expand)
    (for (nausicaa language classes clause-parsers)		expand)
    (prefix (nausicaa language sentinel) sentinel.)
    (nausicaa language makers)
    (only (nausicaa language auxiliary-syntaxes)
	  parent sealed opaque parent-rtd nongenerative
	  protocol fields mutable immutable
	  inherit predicate maker maker-transformer setter getter bindings
	  public-protocol maker-protocol superclass-protocol virtual-fields
	  methods method method-syntax custom-maker mixins satisfies)
    (for (nausicaa language extensions) run expand)
    (for (prefix (nausicaa language classes properties) prop.) expand)
    (nausicaa language classes internal-auxiliary-syntaxes)
    (nausicaa language classes top))


;;;; routines for the default record protocol

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
     (and parent-rtd
	  (if (eq? 'nausicaa:builtin:<top> (record-type-uid parent-rtd))
	      (<top> :superclass-constructor-descriptor)
	    (%make-from-fields-cd parent-rtd (%make-default-protocol parent-rtd)))))
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
   ((pair?		obj)
    ;;Order does matter  here!!!  Better leave these at  the end because
    ;;qualifying a long list can be time-consuming.
    (cond ((list?	obj)	(class-record-type-descriptor <list>))
	  (else			(class-record-type-descriptor <pair>))))
   (else (class-record-type-descriptor <top>))))

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
  (syntax-rules ()
    ((_ ?class-name)
     (?class-name :class-record-type-descriptor))))

;;; --------------------------------------------------------------------

(define-syntax class-public-constructor-descriptor
  ;;Expand into the class' public constructor descriptor associated to a
  ;;class name.
  ;;
  (syntax-rules ()
    ((_ ?class-name)
     (?class-name :public-constructor-descriptor))))

(define-syntax class-superclass-constructor-descriptor
  ;;Expand into the  class' superclass constructor descriptor associated
  ;;to a class name.
  ;;
  (syntax-rules ()
    ((_ ?class-name)
     (?class-name :superclass-constructor-descriptor))))

(define-syntax class-from-fields-constructor-descriptor
  ;;Expand into the class' from-fields constructor descriptor associated
  ;;to a class name.
  ;;
  (syntax-rules ()
    ((_ ?class-name)
     (?class-name :from-fields-constructor-descriptor))))

;;; --------------------------------------------------------------------

(define-syntax class-type-uid
  ;;Expand into the class type UID associated to a class name.
  ;;
  (syntax-rules ()
    ((_ ?class-name)
     (?class-name :class-type-uid))))

(define-syntax class-uid-list
  ;;Expand into  the list of type UIDs  of the parents of  a class name.
  ;;The first element is the UID of the class itself, then comes the UID
  ;;of the parent, then the UID of the parent's parent, etc.
  ;;
  (syntax-rules ()
    ((_ ?class-name)
     (?class-name :class-uid-list))))

;;; --------------------------------------------------------------------

(define-syntax class-parent-rtd-list
  (syntax-rules ()
    ((_ ?class-name)
     (?class-name :parent-rtd-list))))


;;;; usage macros

(define-syntax make
  ;;Build  a new  class instance  using the  maker constructor,  if any;
  ;;defaults to the public constructor.
  ;;
  (syntax-rules ()
    ((_ ?class-name ?arg ...)
     (?class-name :make ?arg ...))))

(define-syntax make*
  ;;Build a new class instance using the public constructor.
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
  (syntax-case stx (<top> <>)

    ((_ ?obj <top>)
     (syntax #t))

    ((_ <> ?class-name)
     #'(?class-name :predicate))

    ((_ ?obj ?class-name)
     (identifier? #'?class-name)
     #'(?class-name :is-a? ?obj))

    (_
     (synner "invalid syntax use"))))

(define-syntax* (slot-ref stx)
  (syntax-case stx (<>)
    ((_ ?object-expr ?slot-name ?class)
     (not (identifier? #'?slot-name))
     (synner "expected identifier as slot name" #'?slot-name))

    ((_ ?object-expr ?slot-name ?class)
     (not (identifier? #'?class))
     (synner "expected identifier as class name" #'?class))

    ((_ <> ?slot-name ?class)
     #'(?class :slot-accessor ?slot-name))

    ((_ ?object-expr ?slot-name ?class)
     #'((?class :slot-accessor ?slot-name) ?object-expr))

    (_
     (synner "invalid syntax in slot-ref form"))))

(define-syntax* (slot-set! stx)
  (syntax-case stx (<>)
    ((_ ?object-expr ?slot-name ?class ?value-expr)
     (not (identifier? #'?slot-name))
     (synner "expected identifier as slot name" #'?slot-name))

    ((_ ?object-expr ?slot-name ?class ?value-expr)
     (not (identifier? #'?class))
     (synner "expected identifier as class name" #'?class))

    ((_ <> ?slot-name ?class <>)
     #'(?class :slot-mutator ?slot-name))

    ((_ ?object-expr ?slot-name ?class ?value-expr)
     #'((?class :slot-mutator ?slot-name) ?object-expr ?value-expr))

    (_
     (synner "invalid syntax in slot-set! form"))))


(define-syntax* (define-foreign-class stx)
  ;;A foreign class  is just a tag  we slap on any value  to use virtual
  ;;fields  and methods  with dot  notation,  but nevertheless  it is  a
  ;;proper record type.
  ;;
  ;;Raise  an error  if  a PUBLIC-PROTOCOL,  MAKER-PROTOCOL or  FIELDS
  ;;clause is present  in the body of the  definition; else define the
  ;;class with DEFINE-CLASS specifying  a public protocol which raises
  ;;an error when invoked.
  ;;
  (syntax-case stx ()
    ((_ ?name ?clause ...)
     (let loop ((clauses	#'(?clause ...))
		(collected	'()))
       (syntax-case clauses (public-protocol maker-protocol maker-transformer maker fields)
	 (()
	  #`(define-class ?name
	      (public-protocol (lambda (make-parent)
				 (lambda args
				   (syntax-violation #f
				     "attempt to instantiate foreign class" '?name))))
	      #,@collected))

	 (((public-protocol ?x ...) ?clause ...)
	  (synner "public-protocol clause used in definition of foreign class"
		  #'(public-protocol ?x ...)))

	 (((maker-protocol ?x ...) ?clause ...)
	  (synner "maker-protocol clause used in definition of foreign class"
		  #'(maker-protocol ?x ...)))

	 (((maker-transformer ?x ...) ?clause ...)
	  (synner "maker-transformer clause used in definition of foreign class"
		  #'(maker-transformer ?x ...)))

	 (((maker ?x ...) ?clause ...)
	  (synner "maker clause used in definition of foreign class" #'(maker ?x ...)))

	 (((fields ?x ...) ?clause ...)
	  (synner "fields clause used in definition of foreign class" #'(fields ?x ...)))

	 ((?clause0 ?clause ...)
	  (loop #'(?clause ...) (cons #'?clause0 collected)))

	 )))))


(define-syntax* (define-class stx)
  (define (main)
    (define-values (class-identifier constructor-identifier predicate-identifier original-clauses)
      (syntax-case stx ()
	((_ (?name ?constructor ?predicate) ?clause ...)
	 (sx.all-identifiers? #'(?name ?constructor ?predicate))
	 (values #'?name #'?constructor #'?predicate (sx.unwrap #'(?clause ...))))

	((_ ?name ?clause ...)
	 (identifier? #'?name)
	 (values #'?name
		 (sx.syntax-maker-identifier #'?name)
		 (sx.syntax-predicate-identifier #'?name)
		 (sx.unwrap #'(?clause ...))))

	((_ ?name-spec . ?clauses)
	 (%synner "invalid name specification in class definition" #'?name-spec))))

    (help.validate-class-clauses original-clauses %synner)

    (let*-values
	;;The list of definition clauses joined with the requested mixin
	;;clauses; the  list of requested  mixin identifiers to  be used
	;;for inspection purposes.
	;;
	;;In the  original clauses  from the class  definition: separate
	;;the MIXINS clauses from the other clauses.  For each requested
	;;mixin: retrieve its clasuses,  specialise them and add them to
	;;the non-MIXINS original clauses.
	;;
	(((clauses mixin-identifiers)
	  (help.filter-and-compose-with-mixin-clauses original-clauses class-identifier
						      help.validate-class-clauses %synner))

	 ;;The superclass identifier or  false; the inherit options: all
	 ;;boolean values.
	 ((superclass-identifier inherit-concrete-fields? inherit-virtual-fields?
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

	 ;;A syntax object holding  an expression which evaluates to the
	 ;;class' public protocol function.
	 ((public-protocol)
	  (%collect-clause/public-protocol clauses %synner))

	 ;;A syntax object holding  an expression which evaluates to the
	 ;;class' maker protocol function.
	 ((maker-protocol)
	  (%collect-clause/maker-protocol clauses %synner))

	 ;;False  or  a  syntax   object  holding  an  expression  which
	 ;;evaluates to the class' superclass protocol function.
	 ((superclass-protocol)
	  (%collect-clause/superclass-protocol clauses %synner))

	 ;;False or a syntax object holding the maker transformer.
	 ((maker-transformer)
	  (%collect-clause/maker-transformer clauses %synner))

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

	 ;;False or an identifier  representing the custom maker for the
	 ;;class.
	 ((custom-maker)
	  (%collect-clause/custom-maker clauses %synner))

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

	 ;;Null or  a validated list of concrete  fields having elements
	 ;;with format:
	 ;;
	 ;;    (immutable <field name> <field accessor> <field getter> <field class> ...)
	 ;;    (mutable   <field name> <field accessor> <field mutator> <field getter> <field class> ...)
	 ;;
	 ;;where  IMMUTABLE  and  MUTABLE  are  symbols  and  the  other
	 ;;elements are identifiers.
	 ((fields)
	  (%collect-clause/fields class-identifier clauses %synner))

	 ;;Null or  a validated list  of virtual fields  having elements
	 ;;with format:
	 ;;
	 ;;    (immutable <field name> <field accessor> <field getter> <field class> ...)
	 ;;    (mutable   <field name> <field accessor> <field mutator> <field getter> <field class> ...)
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
	  (%collect-clause/method-syntax class-identifier clauses %synner))

	 ;;Null or a list of satisfaction function identifiers.
	 ;;
	 ((satisfactions)
	  (%collect-clause/satisfies clauses %synner))

	 ;;The full list of method identifiers.
	 ;;
	 ((all-methods)
	  (append methods methods-from-methods syntax-methods))

	 ;;The full list of method definitions.
	 ;;
	 ((all-method-definitions)
	  (append method-definitions syntax-definitions)))
      (let*-values
	  (((superclass-identifier parent-rtd parent-cd)
	    (help.normalise-class-inheritance superclass-identifier parent-name
					      parent-rtd parent-cd %synner))

	   ;;If the parent  is a record, rather than  a class, there are
	   ;;no superclass properties to be inspected.
	   ;;
	   ((the-parent-is-a-class? superclass-properties)
	    (help.extract-super-properties-if-any superclass-identifier))

	   ;;List of  identifiers representing  the field types  in both
	   ;;this class and all its superclasses, if any.
	   ;;
	   ((list-of-field-tags)
	    (help.list-of-unique-field-types fields virtual-fields
					     (if the-parent-is-a-class?
						 (prop.class-list-of-field-tags superclass-properties)
					       '())
					     %synner))

	   ;;A proper list  of identifiers representing the superclasses
	   ;;of this  class.  The  first identifier in  the list  is the
	   ;;direct  superclass, then  comes its  superclass and  so on.
	   ;;The last element in the list is usually "<top>".
	   ;;
	   ((list-of-superclasses)
	    (if the-parent-is-a-class?
		(cons superclass-identifier (prop.class-list-of-supers superclass-properties))
	      '())))

	(let ((id (sx.duplicate-identifiers? (append (map cadr fields)
						     (map cadr virtual-fields)
						     (map car  all-methods)))))
	  (when id
	    (%synner "duplicate field names" id)))

	(with-syntax
	    ((THE-CLASS			class-identifier)
	     (THE-SUPERCLASS		superclass-identifier)
	     (THE-RTD			#'the-rtd)
	     (THE-PARENT-CD		#'the-parent-cd)
	     (THE-PUBLIC-CD		#'the-public-cd)
	     (THE-PUBLIC-CONSTRUCTOR	constructor-identifier)
	     (THE-MAKER-CONSTRUCTOR	#'the-maker-constructor)
	     (THE-FROM-FIELDS-PROTOCOL	#'the-from-fields-protocol)
	     (THE-COMMON-PROTOCOL	#'the-common-protocol)
	     (THE-MAKER-PROTOCOL	#'the-maker-protocol)
	     (THE-PREDICATE		predicate-identifier)
	     (THE-MAKER			#'the-maker)

	     (PARENT-RTD-FORM		parent-rtd)
	     (PARENT-CD-FORM		parent-cd)
	     (UID			uid-symbol)
	     (SEALED			sealed)
	     (OPAQUE			opaque)
	     (COMMON-PROTOCOL		common-protocol)
	     (PUBLIC-PROTOCOL		public-protocol)
	     (MAKER-PROTOCOL		maker-protocol)
	     (SUPERCLASS-PROTOCOL	superclass-protocol)
	     (CUSTOM-PREDICATE		custom-predicate)
	     (THE-CUSTOM-MAKER		custom-maker)
	     ((METHOD-DEFINITION ...)	all-method-definitions)
	     (INHERIT-CONCRETE-FIELDS?	inherit-concrete-fields?)
	     (INHERIT-VIRTUAL-FIELDS?	inherit-virtual-fields?)
	     (INHERIT-METHODS?		inherit-methods?)
	     (INHERIT-SETTER-AND-GETTER? inherit-setter-and-getter?)
	     (SETTER			setter)
	     (GETTER			getter)
	     (BINDINGS-MACRO		bindings-macro)
	     (FIELD-SPECS		fields)
	     (VIRTUAL-FIELD-SPECS	virtual-fields)
	     (METHOD-SPECS		all-methods)
	     ;;We need MUTABILITY and FIELD to build the RTD.
	     (((MUTABILITY FIELD X ...) ...) fields)
	     (LIST-OF-FIELD-TAGS	list-of-field-tags)
	     (LIST-OF-SUPERCLASSES	list-of-superclasses)
	     (MIXIN-IDENTIFIERS		mixin-identifiers)
	     ((SATISFACTION ...)	satisfactions)
	     (INPUT-FORM		stx))
	  (with-syntax
	      ;;Here we  try to  build and select  at expand time  what is
	      ;;possible.
	      (((FIELD-DEFINITION ...)	(%make-fields-accessors-and-mutators #'THE-RTD fields))
	       (MAKER-DEFINITION	(%make-maker-definition #'THE-MAKER
								#'THE-MAKER-CONSTRUCTOR
								maker-positional-args
								maker-optional-args
								maker-transformer
								#'THE-PUBLIC-CONSTRUCTOR))
	       (FROM-FIELDS-CD-FORM	(%make-from-fields-cd-form the-parent-is-a-class?
								   #'THE-RTD
								   #'THE-SUPERCLASS
								   #'THE-FROM-FIELDS-PROTOCOL))
	       (COMMON-PROTOCOL-FORM	(or common-protocol	#'THE-FROM-FIELDS-PROTOCOL))
	       (PUBLIC-PROTOCOL-FORM	(or public-protocol	#'THE-COMMON-PROTOCOL))
	       (SUPERCLASS-PROTOCOL-FORM (or superclass-protocol	#'THE-COMMON-PROTOCOL))
	       (MAKER-CD-FORM		(if maker-protocol
					    #'(make-record-constructor-descriptor
					       THE-RTD THE-PARENT-CD THE-MAKER-PROTOCOL)
					  #'THE-PUBLIC-CD))
	       (SLOT-ACCESSOR-OF-TRANSFORMER
		(help.make-fields-accessor-of-transformer class-identifier fields virtual-fields %synner))
	       (SLOT-MUTATOR-OF-TRANSFORMER
		(help.make-fields-mutator-of-transformer class-identifier fields virtual-fields %synner))
	       (WITH-FIELD-CLASS-BINDINGS
		(help.make-with-field-class-bindings fields virtual-fields %synner)))
	    #'(begin
		(define the-parent-rtd	PARENT-RTD-FORM)
		(define THE-PARENT-CD	PARENT-CD-FORM)
		(define THE-RTD
		  (make-record-type-descriptor (quote THE-CLASS) the-parent-rtd
					       (quote UID) SEALED OPAQUE
					       (quote #((MUTABILITY FIELD) ...))))

		(define THE-PREDICATE (record-predicate THE-RTD))

		;;The protocol to use with the MAKE-FROM-FIELDS macro.
		(define THE-FROM-FIELDS-PROTOCOL	(%make-default-protocol THE-RTD))
		(define the-from-fields-cd	FROM-FIELDS-CD-FORM)
		(define from-fields-constructor	(record-constructor the-from-fields-cd))

		;;The default protocol to use when the other protocols are
		;;not defined.
		(define THE-COMMON-PROTOCOL	COMMON-PROTOCOL-FORM)

		;;The protocol to use with the MAKE* macro.
		(define the-public-protocol	PUBLIC-PROTOCOL-FORM)
		(define THE-PUBLIC-CD		(make-record-constructor-descriptor
						 THE-RTD THE-PARENT-CD the-public-protocol))
		(define THE-PUBLIC-CONSTRUCTOR	(record-constructor THE-PUBLIC-CD))

		;;The protocol to use with the MAKE macro.
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

		FIELD-DEFINITION ...
		METHOD-DEFINITION ...
		MAKER-DEFINITION

		(define-syntax THE-CLASS
		  (lambda (stx)
		    (syntax-case stx (:class-record-type-descriptor
				      :class-type-uid
				      :class-uid-list
				      :from-fields-constructor-descriptor
				      :is-a?
				      :predicate
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
				      :with-class-bindings-of
				      :slot-accessor
				      :slot-mutator)

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
		       #'(quote FIELD-SPECS))

		      ((_ :list-of-virtual-fields)
		       #'(quote VIRTUAL-FIELD-SPECS))

		      ((_ :list-of-methods)
		       #'(quote METHOD-SPECS))

		      ;; --------------------------------------------------

		      ((_ :make ?arg (... ...))
		       (if (syntax->datum #'THE-CUSTOM-MAKER)
			   #'(THE-CUSTOM-MAKER ?arg (... ...))
			 #'(THE-MAKER ?arg (... ...))))

		      ((_ :make* ?arg (... ...))
		       #'(THE-PUBLIC-CONSTRUCTOR ?arg (... ...)))

		      ((_ :make-from-fields ?arg (... ...))
		       #'(from-fields-constructor ?arg (... ...)))

		      ((_ :is-a? ?arg)
		       #'(CUSTOM-PREDICATE ?arg))

		      ((_ :predicate)
		       #'CUSTOM-PREDICATE)

		      ((_ :with-class-bindings-of
			  (?use-dot-notation
			   ?inherit-concrete-fields
			   ?inherit-virtual-fields
			   ?inherit-methods
			   ?inherit-setter-and-getter)
			  ?variable-name ?instance ?arg (... ...))
		       (and (identifier? #'?variable-name)
			    (for-all boolean? (syntax->datum #'(?use-dot-notation
								?inherit-concrete-fields
								?inherit-virtual-fields
								?inherit-methods
								?inherit-setter-and-getter))))
		       #'(with-class-bindings
			  (?use-dot-notation
			   ?inherit-concrete-fields
			   ?inherit-virtual-fields
			   ?inherit-methods
			   ?inherit-setter-and-getter)
			  ?variable-name ?instance ?arg (... ...)))

		      ((_ :slot-accessor ?slot-name)
		       (identifier? #'?slot-name)
		       #'(slot-accessor-of ?slot-name))

		      ((_ :slot-mutator ?slot-name)
		       (identifier? #'?slot-name)
		       #'(slot-mutator-of ?slot-name))

		      ((_ ?keyword . ?rest)
		       (syntax-violation 'THE-CLASS
			 "invalid class internal keyword"
			 (syntax->datum stx)
			 (syntax->datum #'?keyword)))
		      )))

		;;We  must set  the identifier  properties  of THE-CLASS
		;;after  THE-CLASS itself has  been bound  to something,
		;;else  it will  be seen  as an  unbound  identifier and
		;;FREE-IDENTIFIER=? will get confused and not do what we
		;;want.   (Especially when evaluating  class definitions
		;;with an EVAL as we do in the test suite.)
		;;
		(define-syntax dummy
		  (begin
		    (prop.struct-properties-define
		     #'THE-CLASS (prop.make-class
				  (sx.syntax->list #'LIST-OF-SUPERCLASSES)
				  (sx.unwrap #'FIELD-SPECS)
				  (sx.unwrap #'VIRTUAL-FIELD-SPECS)
				  (sx.unwrap #'METHOD-SPECS)
				  (sx.syntax->list #'MIXIN-IDENTIFIERS)
				  (sx.syntax->list #'LIST-OF-FIELD-TAGS)))
		    (help.detect-circular-tagging #'THE-CLASS #'INPUT-FORM)
		    (SATISFACTION #'THE-CLASS) ...
		    values))

		(define-syntax* (with-class-bindings stx)
		  ;;This  macro defines  all the  syntaxes to  be  used by
		  ;;WITH-CLASS in a single LET-SYNTAX form.
		  ;;
		  (define-inline (or-null bool form)
		    (if (syntax->datum bool) form '()))
		  (syntax-case stx ()
		    ((_ (?use-dot-notation
			 ?inherit-concrete-fields ?inherit-virtual-fields
			 ?inherit-methods ?inherit-setter-and-getter)
			?variable-name ?instance ?body0 ?body (... ...))
		     (let ((use-dot-notation?	(syntax->datum #'?use-dot-notation)))
		       (with-syntax
			   ((((CVAR CVAL) (... ...)) ;concrete fields
			     (or-null #'?inherit-concrete-fields
				      (make-field-bindings use-dot-notation?
							   #'?variable-name #'?instance
							   #'FIELD-SPECS synner)))
			    (((VVAR VVAL) (... ...)) ;virtual fields
			     (or-null #'?inherit-virtual-fields
				      (make-field-bindings use-dot-notation?
							   #'?variable-name #'?instance
							   #'VIRTUAL-FIELD-SPECS synner)))
			    (((MVAR MVAL) (... ...)) ;methods
			     (or-null #'?inherit-methods
				      (make-method-bindings use-dot-notation?
							    #'?variable-name #'?instance
							    #'METHOD-SPECS synner)))
			    (((SVAR SVAL) (... ...)) ;setter and getter
			     (or-null #'?inherit-setter-and-getter
				      (make-setter-getter-bindings #'?variable-name #'?instance
								   #'SETTER #'GETTER))))
			 #`(THE-SUPERCLASS
			    :with-class-bindings-of
			    (?use-dot-notation
			     INHERIT-CONCRETE-FIELDS?
			     INHERIT-VIRTUAL-FIELDS?
			     INHERIT-METHODS?
			     INHERIT-SETTER-AND-GETTER?)
			    ?variable-name ?instance
			    (let-syntax ((CVAR CVAL) (... ...)
					 (VVAR VVAL) (... ...)
					 (MVAR MVAL) (... ...)
					 (SVAR SVAL) (... ...))
			      (BINDINGS-MACRO THE-CLASS ?variable-name ?instance
					      (with-field-class ?variable-name ?instance
								WITH-FIELD-CLASS-BINDINGS
								?body0 ?body (... ...)))))
			 )))))

		(define-syntax slot-accessor-of	SLOT-ACCESSOR-OF-TRANSFORMER)
		(define-syntax slot-mutator-of	SLOT-MUTATOR-OF-TRANSFORMER)

		))))))

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
    ;;    (mutable   ?field ?accessor ?mutator . ?rest)
    ;;    (immutable ?field ?accessor . ?rest)
    ;;
    ;;the  order of  the field  specifications must  match the  order of
    ;;fields in the RTD definition.
    ;;
    ;;Example, given the class definition:
    ;;
    ;;    (define-class <alpha>
    ;;      (fields (mutable a)
    ;;              (mutable b)
    ;;              (immutable c)))
    ;;
    ;;this function is called with FIELDS being the syntax object:
    ;;
    ;;	  ((mutable   a <alpha>-a <alpha>-a-set!)
    ;;	   (mutable   b <alpha>-b <alpha>-b-set!)
    ;;	   (immutable c <alpha>-c))
    ;;
    ;;we want to return a syntax object holding:
    ;;
    ;;	  ((define <alpha>-a		(record-accessor rtd 0))
    ;;	   (define <alpha>-a-set!	(record-mutator  rtd 0))
    ;;	   (define <alpha>-b		(record-accessor rtd 1))
    ;;	   (define <alpha>-b-set!	(record-mutator  rtd 1))
    ;;	   (define <alpha>-c		(record-accessor rtd 2)))
    ;;
    (let loop ((definitions	'())
	       (field-index	0)
	       (fields		fields))
      (syntax-case fields (mutable immutable)
	(()
	 definitions)

	(((mutable ?field ?accessor ?mutator . ?rest) . ?clauses)
	 (loop (cons* #`(define ?accessor  (record-accessor #,rtd-name #,field-index))
		      #`(define ?mutator   (record-mutator  #,rtd-name #,field-index))
		      definitions)
	       (+ 1 field-index)
	       #'?clauses))

	(((immutable ?field ?accessor . ?rest) . ?clauses)
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
    ;;
    (if the-parent-is-a-class?
	#`(make-record-constructor-descriptor #,the-rtd
					      (#,the-superclass :from-fields-constructor-descriptor)
					      #,the-from-fields-protocol)
      ;; the parent is a record
      #`(%make-from-fields-cd #,the-rtd #,the-from-fields-protocol)))

  (define (%make-maker-definition the-maker the-maker-constructor
				  maker-positional-args maker-optional-args
				  maker-transformer the-public-constructor)
    ;;Build and return a syntax object holding the maker definitions for
    ;;this  class.  If  no maker  is  specified: we  want the  following
    ;;output forms with which MAKE expansions equal MAKE* expansions:
    ;;
    ;;	(define-syntax THE-MAKER
    ;;	  (syntax-rules ()
    ;;	    ((_ . ?args)
    ;;	     (THE-PUBLIC-CONSTRUCTOR . ?args)))
    ;;
    ;;if a maker is specified, but no maker transformer is specified: we
    ;;want the  following output forms,  with which MAKE expands  to the
    ;;maker constructor:
    ;;
    ;;  (define-maker (THE-MAKER . MAKER-POSITIONAL-ARGS)
    ;;    THE-MAKER-CONSTRUCTOR MAKER-OPTIONAL-ARGS)
    ;;
    ;;if both a  maker and a maker transformer  are specified, there are
    ;;two cases:  (1) the specified maker transformer  is an identifier,
    ;;so we want the following output forms:
    ;;
    ;;  (define-maker (THE-MAKER . MAKER-POSITIONAL-ARGS)
    ;;    (MAKER-TRANSFORMER the-maker-constructor)
    ;;    MAKER-OPTIONAL-ARGS)
    ;;
    ;;(2) the specified  maker transformer is an expression,  so we want
    ;;the following output forms:
    ;;
    ;;  (define-maker (THE-MAKER . MAKER-POSITIONAL-ARGS)
    ;;    (the-maker-transformer the-maker-constructor)
    ;;    MAKER-OPTIONAL-ARGS)
    ;;  (define-syntax the-maker-transformer
    ;;    MAKER-TRANSFORMER)
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
    ;;MAKER-TRANSFORMER  must be  false or  a syntax  object if  a maker
    ;;transformer is  specified; the syntax object can  be an identifier
    ;;or not.
    ;;
    ;;THE-PUBLIC-CONSTRUCTOR must  be an identifier bound  to the public
    ;;constructor for the class, to be used when no maker was declared.
    ;;
    (cond ((not (or maker-positional-args maker-optional-args))
	   ;; no maker defined
	   #`(define-syntax #,the-maker
	       (syntax-rules ()
		 ((_ . ?args)
		  (#,the-public-constructor . ?args)))))
	  ((not maker-transformer)
	   ;; maker defined without maker transformer
	   #`(define-maker (#,the-maker #,@maker-positional-args)
	       #,the-maker-constructor #,maker-optional-args))
	  ((identifier? maker-transformer)
	   ;; maker defined with identifier maker transformer
	   #`(define-maker (#,the-maker #,@maker-positional-args)
	       (#,maker-transformer #,the-maker-constructor) #,maker-optional-args))
	  (else
	   ;; maker defined with expression maker transformer
	   #`(begin
	       (define-maker (#,the-maker #,@maker-positional-args)
		 (the-maker-transformer #,the-maker-constructor) #,maker-optional-args)
	       (define-syntax the-maker-transformer #,maker-transformer))
	   )))

  (define %synner
    (case-lambda
     ((msg)
      (%synner msg #f))
     ((msg subform)
      (syntax-violation 'define-class
	(string-append msg " in class definition")
	(syntax->datum stx) (syntax->datum subform)))))

  (main))


(define-syntax* (define-label stx)
  ;;A label is just a tag we slap on any value to use virtual fields and
  ;;methods with dot notation, it  has NO record type.  Labels canNOT be
  ;;used in the inheritance hierarchy of classes.
  ;;
  (define (%synner msg subform)
    (syntax-violation 'define-label
      (string-append msg " in label definition")
      (syntax->datum stx)
      (syntax->datum subform)))

  (define-values (label-identifier predicate-identifier original-clauses)
    (syntax-case stx ()
      ((_ (?name ?predicate) ?clause ...)
       (sx.all-identifiers? #'(?name ?predicate))
       (values #'?name #'?predicate (sx.unwrap #'(?clause ...))))

      ((_ ?name ?clause ...)
       (identifier? #'?name)
       (values #'?name (sx.syntax-predicate-identifier #'?name)
	       (sx.unwrap #'(?clause ...))))

      ((_ ?name-spec . ?clauses)
       (%synner "invalid name specification in label definition" #'?name-spec))))

  (let*-values
      ;;The list  of definition clauses joined with  the requested mixin
      ;;clauses; the list of requested  mixin identifiers to be used for
      ;;inspection purposes.
      ;;
      ;;In the original clauses  from the label definition: separate the
      ;;MIXINS  clauses  from the  other  clauses.   For each  requested
      ;;mixin: retrieve  its clasuses, specialise  them and add  them to
      ;;the non-MIXINS original clauses.
      ;;
      (((clauses mixin-identifiers)
	(help.filter-and-compose-with-mixin-clauses original-clauses label-identifier
						    help.validate-label-clauses %synner))

       ;;The  superlabel identifier  or false;  the inherit  options: all
       ;;boolean values.
       ((superlabel-identifier
	 inherit-concrete-fields? inherit-virtual-fields? inherit-methods? inherit-setter-and-getter?)
	(%collect-clause/label/inherit clauses %synner))

       ;;False or  an identifier representing  the custom maker  for the
       ;;label.
       ((custom-maker)
	(%collect-clause/custom-maker clauses %synner))

       ;;False or  an identifier  representing the custom  predicate for
       ;;the label.
       ((custom-predicate)
	(%collect-clause/label/predicate clauses %synner))

       ;;False or an identifier representing the setter for the label.
       ((setter)
	(%collect-clause/setter clauses %synner))

       ;;False or an identifier representing the getter for the label.
       ((getter)
	(%collect-clause/getter clauses %synner))

       ;;An identifier  representing the  custom bindings macro  for the
       ;;label.
       ((bindings-macro)
	(%collect-clause/bindings clauses %synner))

       ;;Null or a validated list of virtual fields having elements with
       ;;format:
       ;;
       ;;    (immutable <field name> <field accessor> <field getter> <field class> ...)
       ;;    (mutable   <field name> <field accessor> <field mutator> <field getter> <field class> ...)
       ;;
       ;;where IMMUTABLE and MUTABLE  are symbols and the other elements
       ;;are identifiers.
       ((virtual-fields)
	(%collect-clause/virtual-fields label-identifier clauses %synner))

       ;;Null  or  a  validated  list of  method  specifications  having
       ;;elements with format:
       ;;
       ;;	(<field identifier> <method identifier>)
       ;;
       ((methods-from-methods)
	(%collect-clause/methods label-identifier clauses %synner))

       ;;Null/null or  a validated list of  method specifications having
       ;;elements with format:
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
	(%collect-clause/method label-identifier clauses %synner #'define/with-class))

       ;;Null/null or  a validated list of  method syntax specifications
       ;;having elements with format:
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
	(%collect-clause/method-syntax label-identifier clauses %synner))

       ;;Null or a list of satisfaction function identifiers.
       ;;
       ((satisfactions)
	(%collect-clause/satisfies clauses %synner))

       ;;A boolean, true  if the parent ia a label or  class; false or a
       ;;record representing superlabel properties.   If the parent is a
       ;;record, rather than  a class or label, there  are no superclass
       ;;properties to be inspected.
       ;;
       ((the-parent-is-a-label? superlabel-properties)
	(help.extract-super-properties-if-any superlabel-identifier))

       ;;The full list of method identifiers.
       ;;
       ((all-methods)
	(append methods methods-from-methods syntax-methods))

       ;;The full list of method definitions.
       ;;
       ((all-method-definitions)
	(append method-definitions syntax-definitions))

       ;;A  list of identifiers  representing the  field types  in: this
       ;;label; all its superlabels, if any; all the imported mixins, if
       ;;any.
       ;;
       ((list-of-field-tags)
	(help.list-of-unique-field-types '() virtual-fields
					 (if the-parent-is-a-label?
					     (prop.label-list-of-field-tags superlabel-properties)
					   '())
					 %synner))

       ;;A proper  list of identifiers representing  the superlabels and
       ;;superclasses of  this label.  The first identifier  in the list
       ;;is the direct superlabel, then  comes its superlabel and so on.
       ;;The last element in the list is usually "<top>".
       ;;
       ((list-of-superlabels)
	(if the-parent-is-a-label?
	    (cons superlabel-identifier
		  (prop.label-list-of-supers superlabel-properties))
	  '())))

    (let ((id (sx.duplicate-identifiers? (append (map cadr virtual-fields)
					      (map car  all-methods)))))
      (when id
	(%synner "duplicate field names" id)))

    (with-syntax
	((THE-LABEL			label-identifier)
	 (THE-SUPERLABEL		superlabel-identifier)
	 (THE-PREDICATE			predicate-identifier)
	 (CUSTOM-PREDICATE		custom-predicate)
	 (CUSTOM-MAKER			custom-maker)
	 ((METHOD-DEFINITION ...)	all-method-definitions)
	 (INHERIT-CONCRETE-FIELDS?	inherit-concrete-fields?)
	 (INHERIT-VIRTUAL-FIELDS?	inherit-virtual-fields?)
	 (INHERIT-METHODS?		inherit-methods?)
	 (INHERIT-SETTER-AND-GETTER?	inherit-setter-and-getter?)
	 (SETTER			setter)
	 (GETTER			getter)
	 (BINDINGS-MACRO		bindings-macro)
	 (VIRTUAL-FIELD-SPECS		virtual-fields)
	 (METHOD-SPECS			all-methods)
	 (LIST-OF-FIELD-TAGS		list-of-field-tags)
	 (LIST-OF-SUPERLABELS		list-of-superlabels)
	 ((SATISFACTION ...)		satisfactions)
	 (MIXIN-IDENTIFIERS		mixin-identifiers)
	 (INPUT-FORM			stx))
      (with-syntax
	  ;;Here  we try  to build  and select  at expand  time  what is
	  ;;possible.
	  ((SLOT-ACCESSOR-OF-TRANSFORMER
	    (help.make-fields-accessor-of-transformer label-identifier '() virtual-fields %synner))
	   (SLOT-MUTATOR-OF-TRANSFORMER
	    (help.make-fields-mutator-of-transformer label-identifier '() virtual-fields %synner))
	   (WITH-FIELD-CLASS-BINDINGS
	    (help.make-with-field-class-bindings '() virtual-fields %synner)))
	#'(begin
	    (define THE-PREDICATE
	      (let ((p CUSTOM-PREDICATE))
		(or p (lambda (x) #t))))

	    METHOD-DEFINITION ...

	    (define-syntax THE-LABEL
	      (lambda (stx)
		(syntax-case stx (:is-a?
				  :predicate
				  :with-class-bindings-of
				  :make
				  :slot-accessor
				  :slot-mutator)

		  ((_ :is-a? ?arg)
		   #'(THE-PREDICATE ?arg))

		  ((_ :predicate)
		   #'THE-PREDICATE)

		  ((_ :make . ?args)
		   (if (syntax->datum #'CUSTOM-MAKER)
		       #'(CUSTOM-MAKER . ?args)
		     (syntax-violation 'THE-LABEL
		       "label has no custom maker" stx)))

		  ((_ :with-class-bindings-of
		      (?use-dot-notation ;this comes from WITH-CLASS
		       ?inherit-concrete-fields
		       ?inherit-virtual-fields
		       ?inherit-methods
		       ?inherit-setter-and-getter)
		      ?variable-name ?instance ?arg (... ...))
		   (for-all boolean? (syntax->datum #'(?use-dot-notation
						       ?inherit-concrete-fields
						       ?inherit-virtual-fields
						       ?inherit-methods
						       ?inherit-setter-and-getter)))
		   #'(with-label-bindings
		      (?use-dot-notation
		       ?inherit-concrete-fields
		       ?inherit-virtual-fields
		       ?inherit-methods
		       ?inherit-setter-and-getter)
		      ?variable-name ?instance ?arg (... ...)))

		  ((_ :slot-accessor ?slot-name)
		   (identifier? #'?slot-name)
		   #'(slot-accessor-of ?slot-name))

		  ((_ :slot-mutator ?slot-name)
		   (identifier? #'?slot-name)
		   #'(slot-mutator-of ?slot-name))

		  ((_ ?keyword . ?rest)
		   (syntax-violation 'THE-LABEL
		     "invalid label internal keyword"
		     (syntax->datum #'(THE-LABEL ?keyword . ?rest))
		     (syntax->datum #'?keyword)))
		  )))

	    ;;We must  set the identifier properties  of THE-LABEL after
	    ;;THE-LABEL itself has been bound to something, else it will
	    ;;be  seen as  an unbound  identifier  and FREE-IDENTIFIER=?
	    ;;will get  confused and not  do what we  want.  (Especially
	    ;;when evaluating label definitions with an EVAL as we do in
	    ;;the test suite.)
	    ;;
	    (define-syntax dummy
	      (begin
		(prop.struct-properties-define
		 #'THE-LABEL (prop.make-label
			      (sx.syntax->list #'LIST-OF-SUPERLABELS)
			      (sx.unwrap #'VIRTUAL-FIELD-SPECS)
			      (sx.unwrap #'METHOD-SPECS)
			      (sx.syntax->list #'MIXIN-IDENTIFIERS)
			      (sx.syntax->list #'LIST-OF-FIELD-TAGS)))
		(help.detect-circular-tagging #'THE-LABEL #'INPUT-FORM)
		(SATISFACTION #'THE-LABEL) ...
		values))

	    (define-syntax* (with-label-bindings stx)
	      ;;This  macro  defines all  the  syntaxes  to  be used  by
	      ;;WITH-CLASS in a single LET-SYNTAX form.
	      ;;
	      (define-inline (or-null bool form)
		(if (syntax->datum bool) form '()))
	      (syntax-case stx ()
		((_ (?use-dot-notation
		     ?inherit-concrete-fields ?inherit-virtual-fields
		     ?inherit-methods ?inherit-setter-and-getter)
		    ?variable-name ?instance ?body0 ?body (... ...))
		 (let ((use-dot-notation? (syntax->datum #'?use-dot-notation)))
		   (with-syntax
		       ((((VVAR VVAL) (... ...))
			 (or-null #'?inherit-virtual-fields
				  (make-field-bindings use-dot-notation?
						       #'?variable-name #'?instance
						       #'VIRTUAL-FIELD-SPECS synner)))
			(((MVAR MVAL) (... ...))
			 (or-null #'?inherit-methods
				  (make-method-bindings use-dot-notation?
							#'?variable-name #'?instance
							#'METHOD-SPECS synner)))
			(((SVAR SVAL) (... ...))
			 (or-null #'?inherit-setter-and-getter
				  (make-setter-getter-bindings #'?variable-name #'?instance
							       #'SETTER #'GETTER))))
		     #'(THE-SUPERLABEL
			:with-class-bindings-of
			(?use-dot-notation
			 INHERIT-CONCRETE-FIELDS?
			 INHERIT-VIRTUAL-FIELDS?
			 INHERIT-METHODS?
			 INHERIT-SETTER-AND-GETTER?)
			?variable-name ?instance
			(let-syntax ((VVAR VVAL) (... ...)
				     (MVAR MVAL) (... ...)
				     (SVAR SVAL) (... ...))
			  (BINDINGS-MACRO THE-LABEL ?variable-name ?instance
					  (with-field-class ?variable-name ?instance
							    WITH-FIELD-CLASS-BINDINGS
							    ?body0 ?body (... ...)))))
		     )))))

	    (define-syntax slot-accessor-of	SLOT-ACCESSOR-OF-TRANSFORMER)
	    (define-syntax slot-mutator-of	SLOT-MUTATOR-OF-TRANSFORMER)

	    )))))


(define-syntax* (define-mixin stx)
  (syntax-case stx ()
    ((_ ?the-mixin)
     (synner "at least one clause needed in mixin definition"))
    ((_ ?the-mixin . ?clauses)
     (let ((mixin-identifier	#'?the-mixin)
	   (original-clauses	(sx.unwrap #'?clauses)))
       (help.validate-mixin-clauses original-clauses synner)

       ;;We parse  the original clauses  in the same  way we do  for the
       ;;class clauses, with the following exceptions:
       ;;
       ;;* CUSTOM-PREDICATE is not generated.
       ;;
       ;;* LIST-OF-FIELD-TAGS is not generated.
       ;;
       ;;This allows us  to detect errors and also  to fill the property
       ;;record for the  mixin.  Some of the generated  bindings are not
       ;;used,  they  are  there  only  to  validate  the  corresponding
       ;;clauses.
       ;;
       (let*-values
	   ;;The list  of definition  clauses joined with  the requested
	   ;;mixin clauses; the list of mixin identifiers to be used for
	   ;;inspection purposes.
	   ;;
	   ;;In the original clauses from the mixin definition: separate
	   ;;the  MIXINS  clauses  from  the other  clauses.   For  each
	   ;;requested mixin: retrieve its clasuses, specialise them and
	   ;;add them to the non-MIXINS original clauses.
	   ;;
	   (((clauses requested-mixin-identifiers)
	     (help.filter-and-compose-with-mixin-clauses original-clauses mixin-identifier
							 help.validate-mixin-clauses synner))

	    ;;The superclass  identifier or false;  the inherit options:
	    ;;all boolean values.
	    ((superclass-identifier inherit-concrete-fields? inherit-virtual-fields?
				    inherit-methods? inherit-setter-and-getter?)
	     (%collect-clause/class/inherit clauses synner))

	    ;;An identifier representing the UID of the class.
	    ((uid-symbol)
	     (%collect-clause/nongenerative mixin-identifier clauses synner))

	    ;;A boolean establishing if the class type is sealed.
	    ((sealed)
	     (%collect-clause/sealed clauses synner))

	    ;;A boolean establishing if the class type is opaque.
	    ((opaque)
	     (%collect-clause/opaque clauses synner))

	    ;;A syntax  object holding an expression  which evaluates to
	    ;;the  class' common protocol  function; it  is false  if no
	    ;;protocol clause was present.
	    ((common-protocol)
	     (%collect-clause/protocol clauses synner))

	    ;;A syntax  object holding an expression  which evaluates to
	    ;;the class' public protocol function.
	    ((public-protocol)
	     (%collect-clause/public-protocol clauses synner))

	    ;;A syntax  object holding an expression  which evaluates to
	    ;;the class' maker protocol function.
	    ((maker-protocol)
	     (%collect-clause/maker-protocol clauses synner))

	    ;;False  or  a syntax  object  holding  an expression  which
	    ;;evaluates to the class' superclass protocol function.
	    ((superclass-protocol)
	     (%collect-clause/superclass-protocol clauses synner))

	    ;;False or a syntax object holding the maker transformer.
	    ((maker-transformer)
	     (%collect-clause/maker-transformer clauses synner))

	    ;;False/false  or:  a  syntax   object  holding  a  list  of
	    ;;identifiers representing  the positional arguments  to the
	    ;;maker;  a syntax object  holding a  list of  maker clauses
	    ;;representing optional arguments.
	    ;;
	    ;;*NOTE*:  when  no  maker  clause is  present,  the  values
	    ;;false/false  *cannot* be  matched by  WITH-SYNTAX patterns
	    ;;like:
	    ;;
	    ;;    ((POS-ARG ...) maker-positional-args)
	    ;;    ((OPT-ARG ...) maker-optional-args)
	    ;;
	    ;;so we must test the values first.
	    ((maker-positional-args maker-optional-args)
	     (%collect-clause/maker clauses synner))

	    ;;False or  an identifier representing the  custom maker for
	    ;;the class.
	    ((custom-maker)
	     (%collect-clause/custom-maker clauses synner))

	    ;;False or  the identifier of the parent  *record* type (not
	    ;;class type).
	    ((parent-name)
	     (%collect-clause/parent clauses synner))

	    ;;False/false  or  an expression  evaluating  to the  parent
	    ;;record type descriptor and an expression evaluating to the
	    ;;parent constructor descriptor.
	    ((parent-rtd parent-cd)
	     (%collect-clause/parent-rtd clauses synner))

	    ;;False  or an  identifier representing  the setter  for the
	    ;;class.
	    ((setter)
	     (%collect-clause/setter clauses synner))

	    ;;False or an identifier representing the getter for the
	    ;;class.
	    ((getter)
	     (%collect-clause/getter clauses synner))

	    ;;An identifier  representing the custom  bindings macro for
	    ;;the class.
	    ((bindings-macro)
	     (%collect-clause/bindings clauses synner))

	    ;;Null  or  a  validated  list  of  concrete  fields  having
	    ;;elements with format:
	    ;;
	    ;;    (immutable <field name> <field accessor> <field getter> <field class> ...)
	    ;;    (mutable   <field name> <field accessor> <field getter> <field mutator> <field class> ...)
	    ;;
	    ;;where  IMMUTABLE and  MUTABLE are  symbols and  the other
	    ;;elements are identifiers.
	    ((fields)
	     (%collect-clause/fields mixin-identifier clauses synner))

	    ;;Null or a validated list of virtual fields having elements
	    ;;with format:
	    ;;
	    ;;    (immutable <field name> <field accessor> <field getter> <field class> ...)
	    ;;    (mutable   <field name> <field accessor> <field getter> <field mutator> <field class> ...)
	    ;;
	    ;;where  IMMUTABLE and  MUTABLE  are symbols  and the  other
	    ;;elements are identifiers.
	    ((virtual-fields)
	     (%collect-clause/virtual-fields mixin-identifier clauses synner))

	    ;;Null or a validated list of method specifications from the
	    ;;METHODS clauses, having elements with format:
	    ;;
	    ;;	(<method name> <function or macro identifier>)
	    ;;
	    ((methods-from-methods)
	     (%collect-clause/methods mixin-identifier clauses synner))

	    ;;Null/null  or a  validated list  of  method specifications
	    ;;from the METHOD clauses, having elements with format:
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
	     (%collect-clause/method mixin-identifier clauses synner #'define/with-class))

	    ;;Null/null   or   a  validated   list   of  method   syntax
	    ;;specifications  from  the  METHOD-SYNTAX  clauses,  having
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
	     (%collect-clause/method-syntax mixin-identifier clauses synner))

	    ;;Null or a list of satisfaction function identifiers.
	    ((satisfactions)
	     (%collect-clause/satisfies clauses synner))

	    ;;The full list of method identifiers.
	    ((all-methods)
	     (append methods methods-from-methods syntax-methods))

	    ;;The full list of method definitions.
	    ((all-method-definitions)
	     (append method-definitions syntax-definitions))

	    ;;If the parent is a  record, rather than a class, there are
	    ;;no superclass properties to be inspected.
	    ((the-parent-is-a-class? superclass-properties)
	     (help.extract-super-properties-if-any superclass-identifier))

	    ;;A proper list of identifiers representing the superclasses
	    ;;of this  class.  The first  identifier in the list  is the
	    ;;direct superclass,  then comes  its superclass and  so on.
	    ;;The last element in the list is usually "<top>".
	    ((list-of-superclasses)
	     (if the-parent-is-a-class?
		 (cons superclass-identifier (prop.mixin-list-of-supers superclass-properties))
	       '())))

	 (prop.mixin-clauses-define mixin-identifier clauses)

	 (with-syntax
	     ((THE-MIXIN		mixin-identifier)
	      (MIXIN-IDENTIFIERS	requested-mixin-identifiers)
	      (LIST-OF-SUPERCLASSES	list-of-superclasses))
	   #'(begin
	       (define-syntax THE-MIXIN
		 (lambda (stx)
		   (syntax-violation #'THE-MIXIN "invalid macro invocation" (syntax->datum stx) #f)))

	       ;;We  must  set the  identifier  properties of  THE-MIXIN
	       ;;after  THE-MIXIN itself  has been  bound  to something,
	       ;;else  it will  be  seen as  an  unbound identifier  and
	       ;;FREE-IDENTIFIER=? will get confused  and not do what we
	       ;;want.
	       ;;
	       ;;Notice   that  we   neither  invoke   the  satisfaction
	       ;;functions,  nor  try  to  detect  circular  tagging  of
	       ;;fields.
	       ;;
	       (define-syntax dummy
		 (begin
		   (prop.struct-properties-define
		    #'THE-MIXIN (prop.make-class
				 (sx.syntax->list #'LIST-OF-SUPERCLASSES)
				 (sx.unwrap #'FIELD-SPECS)
				 (sx.unwrap #'VIRTUAL-FIELD-SPECS)
				 (sx.unwrap #'METHOD-SPECS)
				 (sx.syntax->list #'MIXIN-IDENTIFIERS)
				 '() ;list-of-field-tags
				 ))
		   values))
	       )))))))


;;;; virtual methods
;;
;;Each virtual method has a name (method name) and a class (the class it
;;belongs to).   There is a hash  table ($VIRTUAL-METHODS-TABLE) mapping
;;virtual method names to hash  tables; the nested hash tables map class
;;UIDs to method implementation procedures.
;;

(define $virtual-methods-table
  (make-eq-hashtable))

(define-syntax* (define-virtual-method stx)
  (syntax-case stx ()

    ;;Define a method without implementation.
    ((_ ?class ?name)
     (sx.all-identifiers? #'(?class ?name))
     #'(define-virtual-method ?class ?name #f))

    ;;Define a  method with implementation.   Being a method:  the first
    ;;argument ?THIS must be present and it will be the instance itself.
    ((_ ?class (?name ?this . ?args) ?body0 ?body ...)
     #'(define-virtual-method ?class ?name (lambda/with-class (?this . ?args) ?body0 ?body ...)))

    ;;Define a method with an expression as implementation.
    ((_ ?class ?name ?lambda)
     (begin
       (unless (identifier? #'?class)
	 (synner "expected class identifier as first argument in virtual method definition" #'?class))
       (unless (identifier? #'?name)
	 (synner "expected method name identifier as second argument in virtual method definition"
		 #'?name))
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
	   (define-syntax #,(sx.syntax-method-identifier #'?class #'?name)
	     (syntax-rules ()
	       ;;This  is  a method,  so  we  know  that ?SELF  is  an
	       ;;identifier bound to the class instance: we can safely
	       ;;use it multiple times.
	       ((_ ?self . ?args)
		((%retrieve-virtual-method-implementation the-table (record-type-of ?self) '?name)
		 ?self . ?args)))))
       ))
    (_
     (synner "invalid virtual method definition"))))

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


;;;; core public syntaxes to access fields, methods, setters and getters

(define-syntax* (with-class stx)
  ;;This  is the  public entry  point  for fields,  methods, setter  and
  ;;getter access;  the name  WITH-CLASS is a  bit misleading but  it is
  ;;cute.  The gist of it is to expand:
  ;;
  ;;  (with-class (((<var> <instance>) <class>) . <body>))
  ;;
  ;;into:
  ;;
  ;;  (<class> :with-class-bindings-of (#t #t #t #t #t) <var> <instance> . <body>)
  ;;
  ;;and:
  ;;
  ;;  (with-class ((<var> <class>) . <body>))
  ;;
  ;;into:
  ;;
  ;;  (<class> :with-class-bindings-of (#t #t #t #t #t) <var> <var> . <body>)
  ;;
  ;;which is the syntax having  knowledge of the context of <class>; the
  ;;list of #t values enables all the dot notation syntaxes.
  ;;
  ;;<VAR> is meant to be the identifier to use as prefix in dot notation:
  ;;
  ;;	<var>.field
  ;;	<var>.method
  ;;
  ;;while  <INSTANCE> is  meant to  be an  expression evaluating  to the
  ;;instance object; when  <INSTANCE> is not used, <VAR>  is used in its
  ;;place.
  ;;
  ;;We want the full expansion of:
  ;;
  ;;  (with-class ((<var> <class0> <class1>)) . <body>)
  ;;
  ;;to be:
  ;;
  ;;  (<class0> :with-class-bindings-of (#t #t #t #t #t) <var> <var>
  ;;    (<class1> :with-class-bindings-of (#t #t #t #t #t) <var> <var>
  ;;      . <body>))
  ;;
  ;;We  allow  an  empty list  of  clauses  because  it is  useful  when
  ;;expanding other macros into WITH-CLASS uses.
  ;;
  (syntax-case stx (<top>)
    ;;no body
    ((_ ((?var ?class ...) ...))
     (map sx.all-identifiers? (sx.syntax->list #'((?var ?class ...) ...)))
     #'(values))
    ((_ (((?var ?instance) ?class ...) ...))
     (map sx.all-identifiers? (sx.syntax->list #'((?var ?class ...) ...)))
     #'(values))

    ;;no clauses
    ((_ () . ?body)
     #'(begin . ?body))

    ;;no classes, skip the clause
    ((_ (((?var ?instance)) . ?clauses) . ?body)
     (identifier? #'?var)
     #'(with-class ?clauses . ?body))

    ;;no classes, skip the clause
    ((_ ((?var) . ?clauses) . ?body)
     (identifier? #'?var)
     #'(with-class ?clauses . ?body))

    ;;discard <top> class
    ((_ ((?var/instance <top> . ?classes) . ?clauses) . ?body)
     #'(with-class ((?var/instance . ?classes) . ?clauses) . ?body))

    ;;explicit instance expression
    ((_ (((?var ?instance) ?class0 . ?classes) . ?clauses) . ?body)
     (and (identifier? #'?var) (identifier? #'?class0))
     #'(?class0 :with-class-bindings-of (#t #t #t #t #t) ?var ?instance
		(with-class (((?var ?instance) . ?classes) . ?clauses) . ?body)))

    ;;instance is bound to ?VAR
    ((_ ((?var ?class0 . ?classes) . ?clauses) . ?body)
     (and (identifier? #'?var) (identifier? #'?class0))
     #'(?class0 :with-class-bindings-of (#t #t #t #t #t) ?var ?var
                (with-class ((?var . ?classes) . ?clauses) . ?body)))

    ((_ (?clause0 . ?clauses) . ?body)
     (synner "invalid clause in with-class form" #'?clause0))
    (_
     (synner "invalid syntax for with-class"))))

(define-syntax* (with-field-class stx)
  ;;This is the public entry point  for typed fields.  The gist of it is
  ;;to expand:
  ;;
  ;;  (with-field-class <var> <instance> ((<field> <getter> <class>)) . <body>)
  ;;
  ;;into:
  ;;
  ;;  (with-class ((((<var>.<field> (<getter> <instance>)) <class>)) . <body>)
  ;;
  ;;We  allow  an  empty list  of  clauses  because  it is  useful  when
  ;;expanding other macros into WITH-CLASS uses.
  ;;
  (syntax-case stx (<top>)
    ;;no body
    ((_ ?var ?instance ((?field ?getter ?class ...) ...))
     (and (sx.all-identifiers? #'(?field  ...))
	  (sx.all-identifiers? #'(?getter ...))
	  (map sx.all-identifiers? (sx.syntax->list #'((?class ...) ...))))
     #'(values))

    ;;no field clauses
    ((_ ?var ?instance () . ?body)
     #'(begin . ?body))

    ;;detect fully non-tagged fields
    ((_ ?var ?instance ((?field ?getter) ...) . ?body)
     (and (sx.all-identifiers? #'(?field  ...))
	  (sx.all-identifiers? #'(?getter ...)))
     #'(begin . ?body))

    ((_ ?var ?instance ((?field ?getter ?class ...) ...) . ?body)
     (and (sx.all-identifiers? #'(?field  ...))
	  (sx.all-identifiers? #'(?getter ...))
	  (map sx.all-identifiers? (sx.syntax->list #'((?class ...) ...))))
     (with-syntax (((VAR ...) (map (lambda (field)
				     (sx.syntax-dot-notation-identifier #'?var field))
				(sx.unwrap #'(?field ...)))))
       #'(with-class (((VAR (?getter ?instance)) ?class ...) ...) . ?body)))

    ((_ ?var ?instance (?field-clause . ?field-clauses) . ?body)
     (synner "invalid field clause" #'?field-clause))
    (_
     (synner "invalid syntax for with-field-class"))))

(define-syntax* (setf stx)
  (syntax-case stx (setter setter-multi-key set!)

    ((_ (?variable-name ?key0 ?key ...) ?value)
     (identifier? #'?variable-name)
     #`(#,(help.variable-name->Setter-name #'?variable-name) ?key0 ?key ... ?value))

    ((_ ?variable-name ?value)
     (identifier? #'?variable-name)
     #'(set! ?variable-name ?value))

    (_
     (synner "invalid class setter syntax"))))

(define-syntax* (getf stx)
  (syntax-case stx (setter setter-multi-key set!)

    ((_ (?variable-name ?key0 ?key ...))
     (identifier? #'?variable-name)
     #`(#,(help.variable-name->Getter-name #'?variable-name) ?key0 ?key ...))

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
       (sx.all-identifiers? #'(?var ...)) ;no types if this is true
       #'(?let ((?var ?init) ...) ?body0 ?body ...))

      ;;Named LET, all bindings are without types.
      ((_ ?let ?let/with-class ?loop   ((?var ?init) ...) ?body0 ?body ...)
       (sx.all-identifiers? #'(?var ...)) ;no types if this is true
       #'(?let ?loop ((?var ?init) ...) ?body0 ?body ...))

      ;;At least one binding has types.
      ((_ ?let ?let/with-class ?loop   ((?var ?init) ...) ?body0 ?body ...)
       (with-syntax (((VAR ...) (map (lambda (var)
				       (if (pair? var) var (list var #'<top>)))
				  (sx.unwrap #'(?var ...)))))
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
     #'(let ((?var ?init) ...)
	 (with-class ((?var ?class ...) ...) ?body0 ?body ...)))

    ((_ ?loop (((?var ?class0 ?class ...) ?init) ...) ?body0 ?body ...)
     #'(let ?loop ((?var ?init) ...)
	 (with-class ((?var ?class0 ?class ...) ...) ?body0 ?body ...)))

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
     (let ((id (sx.duplicate-identifiers? #'(?var0 ?var1 ...))))
       (if id
	   (synner "duplicate binding names" id)
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
  ;;detect duplicate bindings.
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
     ;;We rely on LET to detect duplicate bindings.
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
       (sx.all-identifiers? #'(?id ?class ...))
       #'?id)
      (_
       (synner "invalid binding declaration"))))
  (syntax-case stx ()
    ((_ ((?var ?init ?step ...) ...)
	(?test ?expr ...)
	?form ...)
     (with-syntax (((ID ...) (map %parse-var (sx.unwrap #'(?var ...)))))
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

;;; --------------------------------------------------------------------

(define-syntax let-make
  (syntax-rules ()
    ((_ ((?var ?class ?arg ...) ...) ?body0 ?body ...)
     (let/with-class (((?var ?class) (make ?class ?arg ...)) ...)
       ?body0 ?body ...))))

(define-syntax let*-make
  (syntax-rules ()
    ((_ ((?var ?class ?arg ...) ...) ?body0 ?body ...)
     (let*/with-class (((?var ?class) (make ?class ?arg ...)) ...)
       ?body0 ?body ...))))

(define-syntax letrec-make
  (syntax-rules ()
    ((_ ((?var ?class ?arg ...) ...) ?body0 ?body ...)
     (letrec/with-class (((?var ?class) (make ?class ?arg ...)) ...)
       ?body0 ?body ...))))

(define-syntax letrec*-make
  (syntax-rules ()
    ((_ ((?var ?class ?arg ...) ...) ?body0 ?body ...)
     (letrec*/with-class (((?var ?class) (make ?class ?arg ...)) ...)
       ?body0 ?body ...))))

(define-syntax let-make*
  (syntax-rules ()
    ((_ ((?var ?class ?arg ...) ...) ?body0 ?body ...)
     (let/with-class (((?var ?class) (make* ?class ?arg ...)) ...)
       ?body0 ?body ...))))

(define-syntax let*-make*
  (syntax-rules ()
    ((_ ((?var ?class ?arg ...) ...) ?body0 ?body ...)
     (let*/with-class (((?var ?class) (make* ?class ?arg ...)) ...)
       ?body0 ?body ...))))

(define-syntax letrec-make*
  (syntax-rules ()
    ((_ ((?var ?class ?arg ...) ...) ?body0 ?body ...)
     (letrec/with-class (((?var ?class) (make* ?class ?arg ...)) ...)
       ?body0 ?body ...))))

(define-syntax letrec*-make*
  (syntax-rules ()
    ((_ ((?var ?class ?arg ...) ...) ?body0 ?body ...)
     (letrec*/with-class (((?var ?class) (make* ?class ?arg ...)) ...)
       ?body0 ?body ...))))


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
	   ((FUNCNAME	(sx.syntax-method-identifier #'?class #'?method-name))
	    (THIS	(datum->syntax #'?method-name 'this)))
	 ;;This output form must be kept in sync with the output form of
	 ;;DEFMETHOD-VIRTUAL below.
	 #'(define/with-class (FUNCNAME THIS . ?args)
	     (?class :with-class-bindings-of
		     (#f #t #t #t #t) ;enable everything, but dot notation
	 	     THIS THIS ?body0 ?body ...)))))))

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
	 ;;  (sx.syntax-method-identifier #'?class #'?method-name)
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
	 ;;where FUNCNAME is built with:
	 ;;
	 ;;  (sx.syntax-method-identifier #'?class #'the-method)
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
	       (?class :with-class-bindings-of
		       (#f #t #t #t #t) ;enable everything, but dot notation
		       THIS THIS ?body0 ?body ...))
	     (define-virtual-method ?class ?method-name the-method))
	 )))))

;;; --------------------------------------------------------------------

(define-syntax* (lambda/with-class stx)
  (define (%parse-formals formals)
    ;;Process a  LAMBDA formals argument for  the case in which  it is a
    ;;list of identifiers or identifier+classes list.  Returns 3 values:
    ;;a list of identifiers representing  the arguments; a list of lists
    ;;representing the classes for each argument; false or an identifier
    ;;representing the "rest" argument if present.
    ;;
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
	 (sx.all-identifiers? #'(?id ?cls0 ?cls ...))
	 (loop #'?cdr (cons #'?id args) (cons #'(?cls0 ?cls ...) cls)))
	(_
	 (synner "invalid formals in lambda definition" formals)))))

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
     (synner "invalid syntax in lambda definition" stx))))

(define-syntax receive/with-class
  (syntax-rules ()
    ((_ ?formals ?expression ?form0 ?form ...)
     (call-with-values
	 (lambda () ?expression)
       (lambda/with-class ?formals ?form0 ?form ...)))))

(define-syntax* (case-lambda/with-class stx)
  (define (main stx)
    (syntax-case stx ()
      ((_ (?formals . ?body) ...)
       #`(case-lambda #,@(map %process-clause (sx.unwrap #'((?formals . ?body) ...)))))
      (_
       (synner "invalid syntax in case-lambda definition"))))

  (define (%process-clause clause)
    ;;Process a  single CASE-LAMBDA  clause returning the  clause itself
    ;;with the body embraced by WITH-CLASS inserted if needed.
    ;;
    (syntax-case clause ()
      ((() . ?body)
       #'(() . ?body))
      ((?formals . ?body)
       (identifier? #'?formals)
       #'(?formals . ?body))
      ((?formals . ?body)
       (let-values (((args cls rest) (%parse-formals #'?formals)))
	 (with-syntax (((ARG ...)	args)
		       (((CLS ...) ...)	cls))
	   (if rest
	       #`((ARG ... . #,rest)
		  (with-class ((ARG CLS ...) ...) . ?body))
	     #'((ARG ...)
		(with-class ((ARG CLS ...) ...) . ?body))))))))

  (define (%parse-formals formals)
    ;;Process a  LAMBDA formals argument for  the case in which  it is a
    ;;list of identifiers or identifier+classes list.  Returns 3 values:
    ;;a list of identifiers representing  the arguments; a list of lists
    ;;representing the classes for each argument; false or an identifier
    ;;representing the "rest" argument if present.
    ;;
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
	 (sx.all-identifiers? #'(?id ?cls0 ?cls ...))
	 (loop #'?cdr (cons #'?id args) (cons #'(?cls0 ?cls ...) cls)))
	(_
	 (synner "invalid formals in lambda definition" formals)))))

  (main stx))


;;;; miscellaneous syntaxes involving classes

(define-syntax* (class-case stx)
  ;;Example:
  ;;
  ;;    (case thing
  ;;      ((<alpha>)    (alpha-body))
  ;;      ((<beta>)     (beta-body))
  ;;      (else         (else-body)))
  ;;
  ;;expands to:
  ;;
  ;;     (cond ((is-a? thing <alpha>)
  ;;            (with-class ((thing <alpha>))
  ;;              (alpha-body)))
  ;;           ((is-a? thing <beta>)
  ;;            (with-class ((thing <beta>))
  ;;              (alpha-body)))
  ;;           (else
  ;;            (else-body)))
  ;;
  ;;notice that  only a  single class identifier  can be present  in the
  ;;``datum list'' of the case clauses.
  ;;
  (syntax-case stx (else)

    ((_ ?thing ((?class) . ?body) ... (else . ?else-body))
     (sx.all-identifiers? #'(?thing ?class ...))
     #'(cond ((is-a? ?thing ?class)
	      (with-class ((?thing ?class))
		. ?body))
	     ...
	     (else . ?else-body)))

    ((_ ?thing ((?class) . ?body) ...)
     (sx.all-identifiers? #'(?thing ?class ...))
     #'(cond ((is-a? ?thing ?class)
	      (with-class ((?thing ?class))
		. ?body))
	     ...))

    (_
     (synner "invalid syntax"))))


;;;; builtin classes

(define-foreign-class <builtin>
  (nongenerative nausicaa:builtin:<builtin>))

(define-syntax define-builtin-class
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?class-name ?clause ...)
       #`(define-foreign-class ?class-name
	   (inherit <builtin>)
	   (nongenerative #,(sx.identifier-prefix "nausicaa:builtin:" #'?class-name))
	   ?clause ...)))))

(define-builtin-class <pair>
  (predicate pair?)
  (virtual-fields (immutable car car)
		  (immutable cdr cdr)))

(define-foreign-class <list>
  (nongenerative nausicaa:builtin:<list>)
  (predicate list?)
  (virtual-fields (immutable length	length)
		  (immutable null?	null?)
		  (immutable car	car)
		  (immutable cdr	cdr)
		  (immutable caar	caar)
		  (immutable cadr	cadr)
		  (immutable cdar	cdar)
		  (immutable cddr	cddr)
		  (immutable caaar	caaar)
		  (immutable caadr	caadr)
		  (immutable cadar	cadar)
		  (immutable caddr	caddr)
		  (immutable cdaar	cdaar)
		  (immutable cdadr	cdadr)
		  (immutable cddar	cddar)
		  (immutable cdddr	cdddr))

  (method-syntax find
    (syntax-rules ()
      ((_ o proc)
       (find proc o))))

  (method-syntax reverse
    (syntax-rules ()
      ((_ o)
       (reverse o))))

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
		  (immutable foldcase	string-foldcase)
		  (immutable empty?	<string>-empty?))
  (getter string-ref)
  (method-syntax substring
    (syntax-rules ()
      ((_ ?o ?begin)
       (substring ?o ?begin (string-length ?o)))
      ((_ ?o ?begin ?end)
       (substring ?o ?begin ?end))
      ))
  (method-syntax append
    (syntax-rules ()
      ((_ ?o . ?strings)
       (string-append ?o . ?strings))))
  (method-syntax list
    (syntax-rules ()
      ((_ ?o)
       (string->list ?o))))
  (method-syntax for-each
    (syntax-rules ()
      ((_ ?o ?proc)
       (string-for-each ?proc ?o))))
  (method-syntax copy
    (syntax-rules ()
      ((_ ?o)
       (string-copy ?o))))
  )

(define-inline (<string>-empty? S)
  (zero? (string-length S)))

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
		  (immutable non-positive?)
		  (immutable non-negative?)
		  (immutable sign))
  (nongenerative nausicaa:builtin:<real-valued>))

(define-inline (<real-valued>-non-positive? x)
  (or (zero? x) (negative? x)))

(define-inline (<real-valued>-non-negative? x)
  (or (zero? x) (positive? x)))

(define-inline (<real-valued>-sign x)
  (cond ((positive? x) +1)
	((negative? x) -1)
	(else           0)))

(define-foreign-class <real>
  (inherit <real-valued>)
  (predicate real?)
  (nongenerative nausicaa:builtin:<real>)
  (virtual-fields (immutable abs abs)))

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
