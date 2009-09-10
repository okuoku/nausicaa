;;; -*- coding: utf-8-unix -*-
;;;
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: Nausicaa Object System
;;;Date: Wed Aug 26, 2009
;;;
;;;Abstract
;;;
;;;	This library is derived from (scmobj).
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
;;;Copyright (c) 1996 Dorai Sitaram
;;;
;;;This is free software; you can redistribute it and/or modify it under
;;;the terms  of the GNU Lesser  General Public License  as published by
;;;the Free Software  Foundation; either version 3.0 of  the License, or
;;;(at your option) any later version.
;;;
;;;This library is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;Lesser General Public License for more details.
;;;
;;;You  should have received  a copy  of the  GNU Lesser  General Public
;;;License along with  this library; if not, write  to the Free Software
;;;Foundation, Inc.,  59 Temple Place, Suite 330,  Boston, MA 02111-1307
;;;USA.
;;;


(library (nos)
  (export

    <builtin>	<builtin>?

    <pair>		<pair>?
    <list>		<list>?
    <circular-list>	<circular-list>?
    <dotted-list>	<dotted-list>?

    <char>		<char>?
    <string>		<string>?
    <vector>		<vector>?
    <bytevector>	<bytevector>?
    <hashtable>		<hashtable>?

    <record>		<record>?
    <condition>		<condition>?

    <port>		<port>?
    <binary-port>	<binary-port>?
    <input-port>	<input-port>?
    <output-port>	<output-port>?
    <textual-port>	<textual-port>?
    <fixnum>		<fixnum>?
    <flonum>		<flonum>?
    <integer>		<integer>?
    <integer-valued>	<integer-valued>?
    <rational>		<rational>?
    <rational-valued>	<rational-valued>?
    <real>		<real>?
    <real-valued>	<real-valued>?
    <complex>		<complex>?
    <number>		<number>?

    is-a? class-of subclass?
    define-generic define-method  add-method
    call-next-method next-method?)
  (import (rnrs)
    (language-extensions)
    (rnrs mutable-pairs (6))
    (keywords)
    (lists)
    (parameters)
    (pretty-print)
    (records))


;;;; helpers

(define-syntax position
  (syntax-rules ()
    ((_ ?element ?list)
     (list-index (lambda (elm)
		   (eq? ?element elm))
       ?list))))


(define-record-type <builtin>
  (nongenerative nausicaa:nos:<builtin>))

(define-syntax define-builtin
  (lambda (stx)
    (syntax-case stx ()
      ((k ?name ?parent ?builtin-pred)
       (let ((name (symbol->string (syntax->datum (syntax ?name)))))
	 (with-syntax ((?record-make (datum->syntax (syntax k)
						    (string->symbol (string-append ":make-" name))))
		       (?record-pred (datum->syntax (syntax k)
						    (string->symbol (string-append ":" name))))
		       (?pred (datum->syntax (syntax k)
					     (string->symbol (string-append name "?")))))
	   (syntax
	    (begin
	      (define-record-type (?name ?record-make ?record-pred)
		(parent ?parent))
	      (define ?pred ?builtin-pred)))))))))

(define-builtin <pair>		<builtin>	pair?)
(define-builtin <list>		<pair>		list?)
(define-builtin <circular-list>	<list>		circular-list?)
(define-builtin <dotted-list>	<list>		dotted-list?)
(define-builtin <char>		<builtin>	char?)
(define-builtin <string>	<builtin>	string?)
(define-builtin <vector>	<builtin>	vector?)
(define-builtin <bytevector>	<builtin>	bytevector?)
(define-builtin <hashtable>	<builtin>	hashtable?)

(define-builtin <record>	<builtin>		record?)
(define-builtin <condition>	<record>		condition?)

(define-builtin <port>		<builtin>		port?)
(define-builtin <input-port>	<port>			input-port?)
(define-builtin <output-port>	<port>			output-port?)
(define-builtin <binary-port>	<port>			binary-port?)
(define-builtin <textual-port>	<port>			textual-port?)

(define-builtin <number>	<builtin>		number?)
(define-builtin <complex>	<number>		complex?)
(define-builtin <real-valued>	<complex>		real-valued?)
(define-builtin <real>		<real-valued>		real?)
(define-builtin <rational-valued> <real>		rational-valued?)
(define-builtin <flonum>	<real>			flonum?)
(define-builtin <rational>	<rational-valued>	rational?)
(define-builtin <integer-valued> <rational-valued>	integer-valued?)
(define-builtin <integer>	<integer-valued>	integer?)
(define-builtin <fixnum>	<integer>		fixnum?)

;;Other possible classes that require more library loading:
;;
;;	<stream>	stream?
;;


;;;; class inspection

(define (is-a? obj class-rtd)
  (and (record? obj)
       ((record-predicate class-rtd) obj)))

#;(define (subclass? rtd1 rtd2)
  (cond ((eq? rtd1 rtd2) #t)
 	;; ((eq? rtd1 #t) #f)
 	;; ((eq? rtd2 #t) #t)
	(else
	 (memq rtd2 (record-precedence-list rtd1)) #t)))

(define (subclass? rtd1 rtd2)
  (cond ((eq? rtd1 rtd2) #t)
	((eq? rtd1 #t) #f)
   	((eq? rtd2 #t) #t)
	(else
	 (memq rtd2 (record-precedence-list rtd1)))))

;; (define (subclass? rtd1 rtd2)
;;   (memq rtd2 (record-precedence-list rtd1)))

(define (class-of value)
  (cond ((record? value)
	 (record-rtd value))

	((number? value)
	 ;;Order does matter here!!!
	 (cond ((fixnum?		value)	(record-type-descriptor <fixnum>))
	       ((integer?		value)	(record-type-descriptor <integer>))
	       ((rational?		value)	(record-type-descriptor <rational>))
	       ((integer-valued?	value)	(record-type-descriptor <integer-valued>))
	       ((rational-valued?	value)	(record-type-descriptor <rational-valued>))
	       ((flonum?		value)	(record-type-descriptor <flonum>))
	       ((real?			value)	(record-type-descriptor <real>))
	       ((real-valued?		value)	(record-type-descriptor <real-valued>))
	       ((complex?		value)	(record-type-descriptor <complex>))
	       ((number?		value)	(record-type-descriptor <number>))
	       (else #f)))
	((char?		value)		(record-type-descriptor <char>))
	((string?	value)		(record-type-descriptor <string>))
	((vector?	value)		(record-type-descriptor <vector>))
	((hashtable?	value)		(record-type-descriptor <hashtable>))
	((port? value)
	 ;;Order here is arbitrary.
	 (cond ((input-port?	value)	(record-type-descriptor <input-port>))
	       ((output-port?	value)	(record-type-descriptor <output-port>))
	       ((binary-port?	value)	(record-type-descriptor <binary-port>))
	       ((textual-port?	value)	(record-type-descriptor <textual-port>))
	       ((port?		value)	(record-type-descriptor <port>))
	       (else #f)))
	((condition?	value)		(record-type-descriptor <condition>))
	((record?	value)		(record-type-descriptor <record>))
	((bytevector?	value)		(record-type-descriptor <bytevector>))
	((pair?		value)
	 ;;Order does matter  here!!!  Better leave these at  the end because
	 ;;qualifying a long list can be time-consuming.
	 (cond ((circular-list	value)	(record-type-descriptor <circular-list>))
	       ((dotted-list?	value)	(record-type-descriptor <dotted-list>))
	       ((list?		value)	(record-type-descriptor <list>))
	       ((pair?		value)	(record-type-descriptor <pair>))
	       (else #f)))
	(else #f)))


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

(define :method-adder (make-special-argument))

(define-syntax define-generic
  (syntax-rules ()
    ((_ ?name)
     (define ?name (make-generic-function)))))

(define-syntax make-generic-function
  (syntax-rules ()
    ((_)
     (let* ((method-alist '())
	    (method-adder (lambda (signature has-rest closure)
			    (set! method-alist
				  (%add-method-to-method-alist method-alist
							       signature has-rest closure)))))
       (lambda args
	 (if (and (pair? args)
		  (special-argument? (car args)))
	     (let ((arg (car args)))
	       (cond ((eq? (car args) :method-adder)
		      method-adder)
		     (else
		      (assertion-violation #f
			"internal error with invalid special argument"
			arg))))
	   (let-syntax ((apply-function/stx (syntax-rules ()
					      ((_ ?closure)
					       (apply ?closure args))))
			(consume-closure (syntax-rules ()
					   ((_ ?closure-list)
					    (begin0
						(car ?closure-list)
					      (set! ?closure-list (cdr ?closure-list)))))))
	     (letrec* ((signature
			(map class-of args))

		       (applicable-methods
			(%compute-applicable-methods signature method-alist))

		       (method-called?  #f)

		       (is-a-next-method-available?
			(lambda ()
			  (null? applicable-methods)))

		       (apply-function
			(lambda (f) (apply-function/stx f)))

		       (call-methods
			(lambda ()
			  (cond
			   (method-called?
			    ;;We enter here only  if a method has been
			    ;;called  and,  in  its  body, a  call  to
			    ;;CALL-NEXT-METHOD is evaluated.
			    (when (null? applicable-methods)
			      (assertion-violation #f
				"called next method but no more methods available"))
			    (apply-function/stx (consume-closure applicable-methods)))

			   ((null? applicable-methods)
			    ;;Raise an error if no applicable methods.
			    (assertion-violation #f
			      "no method defined for these argument classes"
			      (map record-type-name signature)))

			   (else
			    ;;Apply the methods.
			    (set! method-called? #t)
			    (apply-function/stx (consume-closure applicable-methods)))))))
	       (parameterize ((next-method-func-parm call-methods)
			      (next-method-pred-parm is-a-next-method-available?))
		 (call-methods))))))))))


;;;; syntaxes to define and add methods

(define-syntax define-method
  ;;Define a new method and store it in the given generic function.
  ;;
  (syntax-rules ()

    ;;This is for the syntax:
    ;;
    ;;	(define-method (doit (a <alpha>) (b <beta>))
    ;;	  ---)
    ;;
    ((_ (?generic-function . ?args) . ?body)
     (%collect-classes-and-arguments ?generic-function ?args () () . ?body))

    ;;This is for the syntax:
    ;;
    ;;	(define-method doit ((a <alpha>) (b <beta>))
    ;;	  ---)
    ;;
    ((_ ?generic-function ?args . ?body)
     (%collect-classes-and-arguments ?generic-function ?args () () . ?body))))

(define-syntax add-method
  (syntax-rules ()
    ((_ ?generic-function (?record-name ...) ?has-rest ?closure)
     ((?generic-function :method-adder)
      (list (record-type-descriptor ?record-name) ...)	;this is the signature
      ?has-rest ?closure))))

(define-syntax %collect-classes-and-arguments
  ;;Analyse the list  of method arguments collecting a  list of names, a
  ;;list  of  classes and  a  boolean  representing  the rest  argument.
  ;;Finally call the ADD-METHOD syntax to add the method.
  ;;
  (syntax-rules ()
    ((_ ?generic-function ((?next-arg-name ?next-record-name) . ?args)
	(?record-name ...)
	(?arg-name    ...) . ?body)
     ;;Matches the  form when  the next argument  to be processed  has a
     ;;class.
     (%collect-classes-and-arguments ?generic-function ?args
				     (?record-name ... ?next-record-name)
				     (?arg-name    ... ?next-arg-name)
				     . ?body))

    ((_ ?generic-function (?next-arg-name . ?args) (?record-name ...) (?arg-name ...) . ?body)
     ;;Matches the  form when the next  argument to be  processed has no
     ;;class.
     (%collect-classes-and-arguments ?generic-function ?args
				     (?record-name ... #t)
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

(define-syntax for-all*
  ;;Test that the lists have equal  length and all the elements are EQ?.
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
      (every subclass? call-signature signature))

     ;;If  the closure  supports rest  arguments, compare  only  as much
     ;;record types as there are in SIGNATURE.
     ((and has-rest (> call-len len))
      (every subclass? (take-left call-signature len) signature))

     ;;This method is not applicable.
     (else #f))))

(define (%more-specific-method? method1 method2 call-signature)
  ;;Return true if METHOD1 is more specific than METHOD2 with respect to
  ;;CALL-SIGNATURE.   This  function   must  be  applied  to  applicable
  ;;methods.  The longest signature is more specific, by definition.
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
		 (caar method2)

	       (let ((class1 (car signature1))
		     (class2 (car signature2)))
		 (cond
		  ((eq? class1 class2)
		   (loop (cdr signature1) (cdr signature2) (cdr call-signature)))
		  ((subclass? class1 class2) #t)
		  ((subclass? class2 class1) #f)
		  (else
		   (let* ((c (car call-signature))
			  (cpl (if (eq? c #t)
				   '()
				 (record-precedence-list c))))
		     (< (position class1 cpl)
			(position class2 cpl))))))))))))


;;;; done

)

;;; end of file
