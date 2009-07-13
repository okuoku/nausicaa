;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: object system for Scheme
;;;Date: Tue Nov 11, 2008
;;;
;;;Abstract
;;;
;;;	This  is a  port to  R6RS and  LIMY Schemes  of ScmObj  by Dorai
;;;	Sitaram.  The original code is available at:
;;;
;;;	    <http://www.ccs.neu.edu/home/dorai/scmobj/scmobj.html>
;;;
;;;	(last checked Thu  Nov 13, 2008).  The original  code has been a
;;;	little overhauled to make it work with R6RS libraries.
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
;;;Copyright (c) 1996 Dorai Sitaram
;;;
;;;This is free software; you can redistribute it and/or modify it under
;;;the terms  of the GNU Lesser  General Public License  as published by
;;;the Free Software  Foundation; either version 2.1 of  the License, or
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


(library (scmobj)
  (export
    ;;Built in classes.
    <class> <builtin-class>

    <circular-list> <dotted-list> <proper-list> <list> <pair>
    <vector> <bytevector> <hashtable> <record> <condition>
    <binary-port> <textual-port> <input-port> <output-port> <port>
    <fixnum> <flonum> <integer> <integer-valued>
    <rational> <rational-valued> <real> <real-valued>
    <complex> <number>

    ;; Constructors.
    define-class define-generic define-method
    make-class make make-generic-function add-method

    ;;Class inspection.
    class-of
    class-definition-name class-precedence-list
    class-slots class-direct-slots
    class? instance? is-a? subclass?

    ;;Slot accessors.
    slot-ref slot-set!

    ;;Next method interface.
    call-next-method next-method?)
  (import (nausicaa)
    (rnrs mutable-pairs (6))
    (lists))


;;;; helpers

(define-syntax position
  (syntax-rules ()
    ((_ ?element ?list)
     (list-index (lambda (elm)
		   (eq? ?element elm))
       ?list))))

(define (%build-class-precedence-list . superclasses)
  ;;Given a  list of  superclasses for class  <x>: Build and  return the
  ;;class precedence list for <x> to be used in multimethod dispatching.
  (if (null? superclasses)
      superclasses
    (delete-duplicates
     (concatenate (map (lambda (super)
			 (cons super (class-precedence-list super)))
		    superclasses))
     eq?)))

(define (%build-slot-list direct-slots . superclasses)
  ;;Given the  list of direct slot names  for class <x> and  its list of
  ;;superclasses: Build and return full list of slots for <x>.
  (if (null? superclasses)
      direct-slots
    (delete-duplicates
     (concatenate (cons direct-slots (map class-slots superclasses)))
     eq?)))


;;;; access to slots

(define-syntax get-slot
  ;;Slot access should  be as fast as possible, for  this reason we make
  ;;this a syntax (it gets expanded in the function).
  (syntax-rules ()
    ((_ ?caller ?object ?slot-name)
     (or (assq ?slot-name ?object)
	 (assertion-violation ?caller
	   "trying to access nonexistent slot"
	   ?slot-name ?object)))))

(define (slot-ref object slot-name)
  (cdr (get-slot 'slot-ref object slot-name)))

(define (slot-set! object slot-name value)
  (set-cdr! (get-slot 'slot-set! object slot-name) value))


;;;; class inspection functions

(define (class-definition-name class-object)
  (slot-ref class-object ':class-definition-name))

(define (class-precedence-list class-object)
  (slot-ref class-object ':class-precedence-list))

(define (class-slots class-object)
  (slot-ref class-object ':slots))

(define (class-direct-slots class-object)
  (slot-ref class-object ':direct-slots))

(define (instance-classes instance)
  (let ((c (class-of instance)))
    (cons c (class-precedence-list c))))

(define (instance? value)
  (and (proper-list? value)
       (pair? (car value))
       (eq? ':class (caar value))
       (class? (cdar value))))

(define (class?/light value)
  (and (pair? (car value))
       (eq? ':class (caar value))
       (eq? <class> (cdar value))))

(define (class? value)
  (and (proper-list? value)
       (= 5 (length value))
       (first-class-slot?  (car value))
       (second-class-slot? (cadr value))
       (third-class-slot?  (caddr value))
       (fourth-class-slot? (cadddr value))
       (fifth-class-slot? (cadddr (cdr value)))))

(define (is-a?/light object class)
  (memq class (instance-classes object)))

(define (is-a? object class)
  (let ((full-class-list (instance-classes object)))
    (and (memq class full-class-list)
	 (if (memq <builtin-class> full-class-list)
	     #t	;make sure that it returns #t not a generic true
	   (let ((object-slots (cdr (map car object))))
	     (every ;check that all the slots are here
		 (lambda (slot-name)
		   (memq slot-name object-slots))
	       (class-slots class))))
	 #t)))	;make sure that it returns #t not a generic true

;;; --------------------------------------------------------------------

(define (first-class-slot? value)
  ;;It has to be:
  ;;
  ;;   (:class . class-object)
  ;;
  (and (pair? value)
       (eq? ':class (car value))
       (eq? <class> (cdr value))))

(define (second-class-slot? value)
  ;;It has to be:
  ;;
  ;;   (:class-definition-name . symbol)
  ;;
  (and (pair? value)
       (eq? ':class-definition-name (car value))
       (let ((v (cdr value)))
	 (or (symbol? v) (eq? v ':uninitialised)))))

(define (third-class-slot? value)
  ;; It has to be:
  ;;
  ;;   (:class-precedence-list . (... classes ...))
  ;;
  (and (pair? value)
       (eq? ':class-precedence-list (car value))
       (let ((v (cdr value)))
	 (and (proper-list? v)
	      (every class? v)))))

(define (fourth-class-slot? value)
  ;;It has to be:
  ;;
  ;;   (:slots . (... symbols ...))
  ;;
  (and (pair? value)
       (eq? ':slots (car value))
       (let ((v (cdr value)))
	 (and (proper-list? v)
	      (every symbol? v)))))

(define (fifth-class-slot? value)
  ;;It has to be:
  ;;
  ;;   (:direct-slots . (... symbols ...))
  ;;
  (and (pair? value)
       (eq? ':direct-slots (car value))
       (let ((v (cdr value)))
	 (and (proper-list? v)
	      (every symbol? v)))))


;;;; built in classes

;;Not all the Scheme implementations support the #0 syntax.
;;
;; (define <class>
;;   '#0=((:class . #0#)
;;        (:class-definition-name . <class>)
;;        (:class-precedence-list . ())
;;        (:slots . (:class-definition-name
;; 		  :class-precedence-list :slots :direct-slots))
;;        (:direct-slots . (:class-definition-name
;; 			 :class-precedence-list :slots :direct-slots))))
;;
(define <class>
  (let ((layout
	 ;;These two conses  make the list a mutable  value, while using
	 ;;quasiquotation it would be a literal constant.  Mutability is
	 ;;needed  to later  set the  value  of the  ":class" slot  with
	 ;;SET-CDR!.
	 (cons (cons ':class #f)
	       '((:class-definition-name . <class>)
		 (:class-precedence-list . ())
		 (:slots . (:class-definition-name
			    :class-precedence-list :slots :direct-slots))
		 (:direct-slots . (:class-definition-name
				   :class-precedence-list :slots :direct-slots))))))
    (set-cdr! (car layout) layout)
    layout))

(define <builtin-class>
  `((:class . ,<class>)
    (:class-definition-name . <builtin-class>)
    (:class-precedence-list . ())
    (:slots . ())
    (:direct-slots . ())))

;;; --------------------------------------------------------------------

(define-syntax define-builtin-class
  (syntax-rules ()
    ((_ ?name)
     (define-builtin-class ?name ()))
    ((_ ?name (?superclass ...))
     (define ?name
       `((:class . ,<builtin-class>)
	 (:class-definition-name . ?name)
	 (:class-precedence-list . ,(%build-class-precedence-list ?superclass ...))
	 (:slots . ())
	 (:direct-slots . ()))))
    ((_ ?name ?superclass)
     (define-builtin-class ?name (?superclass)))))

(define-builtin-class <pair>)
(define-builtin-class <list>		<pair>)
(define-builtin-class <circular-list>	<list>)
(define-builtin-class <dotted-list>	<list>)
(define-builtin-class <proper-list>	<list>)
(define-builtin-class <vector>)
(define-builtin-class <hashtable>)

(define-builtin-class <port>)
(define-builtin-class <input-port>	<port>)
(define-builtin-class <output-port>	<port>)
(define-builtin-class <binary-port>	<port>)
(define-builtin-class <textual-port>	<port>)

(define-builtin-class <record>)
(define-builtin-class <condition>	<record>)
(define-builtin-class <bytevector>)

(define-builtin-class <number>)
(define-builtin-class <complex>		<number>)
(define-builtin-class <real-valued>	<complex>)
(define-builtin-class <real>		<real-valued>)
(define-builtin-class <rational-valued>	<real>)
(define-builtin-class <flonum>		<real>)
(define-builtin-class <rational>	<rational-valued>)
(define-builtin-class <integer-valued>	<rational-valued>)
(define-builtin-class <integer>		<integer-valued>)
(define-builtin-class <fixnum>		<integer>)

;;Other possible classes that require more library loading:
;;
;;	<stream>	stream?
;;


;;;; class constructors

(define-syntax %make-class
  (syntax-rules ()
    ((_ ?name (?superclass ...) ?slot-spec ...)
     `((:class . ,<class>)
       (:class-definition-name . ?name)
       (:class-precedence-list . ,(%build-class-precedence-list ?superclass ...))
       (:slots                 . ,(%build-slot-list '(?slot-spec ...) ?superclass ...))
       (:direct-slots          . (?slot-spec ...))))))

(define-syntax make-class
  ;;Buld a new  class object, initialise all the  slots.  The class name
  ;;is  set  to the  symbol  ":uninitialised".   Notice  that the  class
  ;;precedence list does not include the new class itself.
  ;;
  ;;It  is  possible for  a  class  to add  no  new  slots: This  allows
  ;;subclassing for the only purpose of method dispatching.
  ;;
  (syntax-rules ()
    ((_)
     (make-class ()))
    ((_ (?superclass ...) ?slot-spec ...)
     (%make-class :uninitialised (?superclass ...) ?slot-spec ...))))

(define-syntax define-class
  ;;Build a new class object, defining a binding for it.  Give a name to
  ;;the class.
  ;;
  (syntax-rules ()
    ((_ ?name)
     (define-class ?name ()))
    ((_ ?name (?superclass ...) ?slot-spec ...)
     (define ?name
       (%make-class ?name (?superclass ...) ?slot-spec ...)))))


;;;; instance constructors, low level

(define (%make class-name class . init-args)
  ;;This  is a standard  make function,  in the  style of  CLOS.  Build,
  ;;initialise and return a new class instance.
  ;;
  (let ((instance (%allocate-instance class)))
    (%initialise instance class-name init-args)
    instance))

(define (%allocate-instance class)
  ;;Build a new  alist initialising all the slots,  but ":class", to the
  ;;symbol  ":uninitialised".  The  ":class" pair  has to  be  the first
  ;;element in the alist.
  ;;
  (cons (cons ':class class)
	(map (lambda (x)
	       (cons x ':uninitialised))
	  (class-slots class))))

(define (%initialise instance class-name slot-values)
  ;;Initialise an already allocated instance with the given slot values.
  ;;SLOT-VALUES  is  interpreted  as  an alist  of  slot-name/slot-value
  ;;pairs.
  ;;
  (map (lambda (p)
	 (slot-set! instance (car p) (cdr p)))
    slot-values))


;;;; instance constructors, high level

(define-syntax make
  ;;Build, initialise and return a  new class instance.  It is a wrapper
  ;;for %MAKE.  This macro exists so  that we can specify the slot names
  ;;without quoting them (as we have to do with %MAKE).
  ;;
  (syntax-rules (:slots)
    ((_ ?class (:slots (?key0 . ?value0) ...) ?key ?value ?thing ...)
     (make ?class (:slots (?key . ?value) (?key0 . ?value0) ...) ?thing ...))
    ((_ ?class (:slots (?key . ?value) ...))
     (%make (quote ?class) ?class (%slot ?class (quote ?key) ?value) ...))
    ((_ ?class ?key ?value ?thing ...)
     (make ?class (:slots (?key . ?value)) ?thing ...))
    ((_ ?class)
     (make ?class '()))))

(define (%slot class key value)
  ;;A slot builder used by the  MAKE syntax.  Make sure that the slot is
  ;;valid for the class.
  ;;
  (if (memq key (class-slots class))
      (cons key value)
    (syntax-violation 'make
      "invalid slot keyword" key (class-slots class))))


;;;; class inspection

(define (subclass? c1 c2)
  ;;Return true if C2 is a subclass of C1.
  ;;
  ;;  (subclass? <integer> <number>)  => #t
  ;;  (subclass? <integer> <integer>) => #t
  ;;  (subclass? <complex> <integer>) => #f
  ;;
  (cond ((eq? c1 c2) #t)
	;;These two equalities  are here because when a  method is added
	;;having an argument with no specified class, the class defaults
	;;to #t (see  the syntaxes defining new methods).   This #t ends
	;;up in the signature of  the method, so SUBCLASS? needs to deal
	;;with it when  used in dispatching.  #t is  the lowest class of
	;;all the classes, so #t is always a superclass of a true class:
	;;
	;;   (subclass? #t <x>) => #f     for all <x>
	;;   (subclass? <x> #t) => #t
	;;
	;;If both  C1 and C2 are  #t, it is also  (eq? c1 c2)  => #t, we
	;;decide to consider the first #t a superclass of the second #t.
	;;
 	((eq? c1 #t) #f)
 	((eq? c2 #t) #t)
	((memq c2 (class-precedence-list c1)) #t)
	(else #f)))

(define (class-of value)
  (cond
   ((and (pair? value)		;fast and approx  way to see if VALUE is
	 (pair? (car value))	;a class instance
	 (eq? ':class (caar value)))
    (cdar value))

   ((number? value)
    ;;Order does matter here!!!
    (cond
     ((fixnum?		value)	<fixnum>)
     ((integer?		value)	<integer>)
     ((rational?	value)	<rational>)
     ((integer-valued?	value)	<integer-valued>)
     ((rational-valued? value)	<rational-valued>)
     ((flonum?		value)	<flonum>)
     ((real?		value)	<real>)
     ((real-valued?	value)	<real-valued>)
     ((complex?		value)	<complex>)
     ((number?		value)	<number>)
     (else #f)))
   ((vector?	value)		<vector>)
   ((hashtable? value)		<hashtable>)
   ((port? value)
    (cond
     ;;Order here is arbitrary.
     ((input-port? value)	<input-port>)
     ((output-port? value)	<output-port>)
     ((binary-port? value)	<binary-port>)
     ((textual-port? value)	<textual-port>)
     ((port? value)		<port>)
     (else #f)))
   ((condition? value)		<condition>)
   ((record? value)		<record>)
   ((bytevector? value)		<bytevector>)
   ((pair? value)
    ;;Order does matter  here!!!  Better leave these at  the end because
    ;;qualifying a long list can be time-consuming.
    (cond
     ((circular-list value)	<circular-list>)
     ((dotted-list? value)	<dotted-list>)
     ((proper-list? value)	<proper-list>)
     ((list? value)		<list>)
     ((pair? value)		<pair>)
     (else #f)))
   (else			#f)))


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

(define-class <generic> ()
  ;;A "generic function"  is basically a couple of  values: an interface
  ;;procedure and an object of class <generic>.
  ;;
  ;;The interface procedure is stored in the ":interface-procedure" slot
  ;;of the object and it is used to apply the generic function to a list
  ;;of arguments.
  ;;
  :interface-procedure
  :add-primary-method
  :add-before-method
  :add-after-method
  :add-around-method)

(define *generic-functions*
  ;;This will hold all the generic functions ever created.  The keys are
  ;;the interface procedures, the values are the <generic> objects.
  (make-eq-hashtable))

(define (%add-method-to-method-alist method-alist signature has-rest function)
  ;;Helper function that adds a  method's entry to the appropriate alist
  ;;of methods.  It is used  in the expansion of the METHOD-ADDER syntax
  ;;below.  Return the method table (possibly modified).
  ;;
  ;;Each entry in the alist has the format:
  ;;
  ;;	((has-rest . signature) . function)
  ;;
  ;;so the  key is a list whose  CAR is the HAS-REST  boolean, and whose
  ;;CDR is  the SIGNATURE of the  method.  This allows  two methods with
  ;;equal signatures to  be distinct if one supports  rest arguments and
  ;;the other does not.
  ;;
  ;;A  new method  is added  only  if no  method with  the selected  key
  ;;already  exists.  If  a  method  with the  key  already exists,  its
  ;;function is overwritten with the new one.
  (let ((key (cons has-rest signature)))
    (cond ((find (lambda (entry)
		;Test  that  the keys  have  equal  length  and all  the
		;elements are  EQ?.  This  is more than  SRFI-1's EVERY,
		;because EVERY does not test for equal length.
		   (let loop ((key1 key)
			      (key2 (car entry)))
		     (cond ((null? key1) (null? key2))
			   ((null? key2) (null? key1))
			   ((eq? (car key1) (car key2))
			    (loop (cdr key1) (cdr key2)))
			   (else #f))))
		 method-alist)
	   => (lambda (entry) ;overwrite an existent function
		(set-cdr! entry function)
		method-alist))
	  (else
	   (alist-cons key function method-alist)))))

(define-syntax define-generic
  (syntax-rules ()
    ((_ ?name)
     (define ?name (make-generic-function)))))

(define-syntax make-generic-function
  (syntax-rules ()
    ((make-generic-function)
     (let* ((generic-object       (create-generic-procedure))
            (interface-procedure  (slot-ref generic-object ':interface-procedure)))
       (hashtable-set! *generic-functions* interface-procedure generic-object)
       interface-procedure))))

(define-syntax create-generic-procedure
  (syntax-rules ()
    ((_)
     (let ((primary-method-alist '())
	   (before-method-alist  '())
	   (after-method-alist   '())
	   (around-method-alist  '()))
       (let-syntax ((method-adder (syntax-rules ()
				    ((_ ?method-alist)
				     (lambda (signature has-rest function)
				       (set! ?method-alist
					     (%add-method-to-method-alist
					      ?method-alist signature has-rest function)))))))
	 (make <generic>
	   :add-primary-method (method-adder primary-method-alist)
	   :add-before-method  (method-adder before-method-alist)
	   :add-after-method   (method-adder after-method-alist)
	   :add-around-method  (method-adder around-method-alist)
	   :interface-procedure
	   (lambda args
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

			 (applicable-primary-closures
			  (%compute-applicable-methods signature primary-method-alist))

			 (applicable-before-closures
			  (%compute-applicable-methods signature before-method-alist))

			 (applicable-after-closures
			  (%compute-applicable-methods signature after-method-alist))

			 (applicable-around-closures
			  (%compute-applicable-methods signature around-method-alist))

			 (primary-method-called?  #f)
			 (reject-recursive-calls? #f)

			 (is-a-next-method-available?
			  (lambda ()
			    (not (if primary-method-called?
				     (null? applicable-primary-closures)
				   (and (null? applicable-around-closures)
					(null? applicable-primary-closures))))))

			 (apply-function
			  (lambda (f) (apply-function/stx f)))

			 (call-methods
			  (lambda ()
			    (cond
			     (reject-recursive-calls?
			      ;;Raise  an   error  if  a   ":before"  or
			      ;;":after" method invokes the next method.
			      (assertion-violation 'call-methods
				":before or :after methods are forbidden to call the next method"))

			     (primary-method-called?
			      ;;We enter  here only if  a primary method
			      ;;has been called and, in its body, a call
			      ;;to CALL-NEXT-METHOD is evaluated.
			      (when (null? applicable-primary-closures)
				(assertion-violation 'call-methods
				  "called next method but no more :primary methods available"))
			      (apply-function/stx (consume-closure applicable-primary-closures)))

			     ((null? applicable-primary-closures)
			      ;;Raise   an  error  if   no  applicable
			      ;;methods.
			      (assertion-violation 'call-methods
				"no method defined for these argument classes"
				(map (lambda (o) (class-definition-name (class-of o)))
				  args)))

			     ((not (null? applicable-around-closures))
			      ;;If around  methods exist: we  apply them
			      ;;first.   It is  expected that  an around
			      ;;method   invokes   CALL-NEXT-METHOD   to
			      ;;evaluate the primary methods.
			      (apply-function/stx (consume-closure applicable-around-closures)))

			     (else
			      ;;Apply  the  methods: before,  primary,
			      ;;after.  Return the return value of the
			      ;;primary.
			      (set! reject-recursive-calls? #t)
			      (for-each apply-function
				applicable-before-closures)
			      (set! reject-recursive-calls? #f)
			      (set! primary-method-called? #t)
			      (begin0
				  (apply-function/stx (consume-closure applicable-primary-closures))
				(set! reject-recursive-calls? #t)
				(for-each apply-function
				  applicable-after-closures)))))))
		 (parameterize ((next-method-func-parm call-methods)
				(next-method-pred-parm is-a-next-method-available?))
		   (call-methods)))))))))))


;;;; methods dispatching

(define (%compute-applicable-methods call-signature method-alist)
  ;;Filter out from  METHOD-ALIST the methods not applicable  to a tuple
  ;;of arguments with types in  the tuple CALL-SIGNATURE.  Then sort the
  ;;list of applicable  methods so that the more  specific are the first
  ;;ones.  Return the sorted list of applicable closures.
  ;;
  ;;The METHOD-ALIST is  expected to be an alist  whose entries have the
  ;;format:
  ;;
  ;;  ((has-rest . signature) . closure)
  ;;
  ;;where SIGNATURE is the  tuple of method arguments' classes, HAS-REST
  ;;a boolean  true if the  closure accepts rest arguments,  CLOSURE the
  ;;method's function.
  ;;
  (map cdr
    (list-sort
     (lambda (method1 method2)
       (%more-specific-method? method1 method2 call-signature))
     (filter
	 (lambda (method)
	   (%applicable-method? call-signature
				(cdar method) ;the method signature
				(caar method))) ;true if accepts rest
       method-alist))))

(define (%applicable-method? call-signature signature has-rest)
  ;;Return  true if  a  method  with SIGNATURE  as  tuple of  arguments'
  ;;classes can be applied to a tuple of arguments having CALL-SIGNATURE
  ;;as  classes.  HAS-REST  must be  true  if the  method supports  rest
  ;;arguments.
  (let ((len      (length signature))
	(call-len (length call-signature)))
    (cond
     ;;If SIGNATURE has the  same length of the call signature,
     ;;test it for applicability.
     ((= call-len len)
      (every subclass? call-signature signature))

     ;;If the closure supports  rest arguments, compare only as
     ;;much classes as there are in SIGNATURE.
     ((and has-rest (> call-len len))
      (every subclass? (take call-signature len) signature))

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
				 (cons c (slot-ref c ':class-precedence-list)))))
		     (< (position class1 cpl)
			(position class2 cpl))))))))))))


;;;; methods

(define-syntax define-method
  ;;Define a new method.  The pattern matching has tree phases:
  ;;
  ;;1. The method is recognised as primary, before, after or around.
  ;;
  ;;2. The  method arguments are accumulated  in a list  of "with class"
  ;;   and a list of "without class".
  ;;
  ;;3. The method is added  to the appropriate collection in the generic
  ;;   function.
  ;;
  ;;The ?QUALIFIER pattern variable is one of the literals:
  ;;
  ;;     :primary
  ;;     :before
  ;;     :after
  ;;     :around
  ;;
  ;;it  defaults to ":primary".
  ;;
  (syntax-rules (:primary :before :after :around)
    ((_ (?generic-function . ?args) . ?body)
     (%collect-classes-and-arguments ?generic-function :primary ?args () () . ?body))

    ((_ ?generic-function :primary ?args . ?body)
     (%collect-classes-and-arguments ?generic-function :primary ?args () () . ?body))

    ((_ ?generic-function :before  ?args . ?body)
     (%collect-classes-and-arguments ?generic-function :before  ?args () () . ?body))

    ((_ ?generic-function :after   ?args . ?body)
     (%collect-classes-and-arguments ?generic-function :after   ?args () () . ?body))

    ((_ ?generic-function :around  ?args . ?body)
     (%collect-classes-and-arguments ?generic-function :around  ?args () () . ?body))

    ((_ ?generic-function          ?args . ?body)
     (%collect-classes-and-arguments ?generic-function :primary ?args () () . ?body))))

(define-syntax %collect-classes-and-arguments
  (syntax-rules (:primary :before :after :around)
    ((_ ?generic-function ?qualifier ((?name ?type) . ?args) (?class ...) (?arg ...) . ?body)
     ;;Matches the  form when  the next argument  to be processed  has a
     ;;class.
     (%collect-classes-and-arguments ?generic-function ?qualifier
				     ?args (?class ... ?type) (?arg ... ?name) . ?body))

    ((_ ?generic-function ?qualifier (?name . ?args) (?class ...) (?arg ...) . ?body)
     ;;Matches the  form when the next  argument to be  processed has no
     ;;class.
     (%collect-classes-and-arguments ?generic-function ?qualifier
				     ?args (?class ... #t) (?arg ... ?name)  . ?body))

    ((_ ?generic-function ?qualifier () (?class ...) (?arg ...) . ?body)
     ;;Matches the form  when all the arguments have  been processed and
     ;;NO  rest argument  is present.   This  MUST come  before the  one
     ;;below.
     (add-method ?generic-function ?qualifier (?class ...)
		 #f ;means no rest argument
		 (lambda (?arg ...) . ?body)))

    ((_ ?generic-function ?qualifier ?rest (?class ...) (?arg ...) . ?body)
     ;;Matches the form  when all the arguments have  been processed and
     ;;only the  rest argument is there.   This MUST come  after the one
     ;;above.
     (add-method ?generic-function ?qualifier (?class ...)
		 #t ;means rest argument is present
		 (lambda (?arg ... . ?rest) . ?body)))))

(define-syntax add-method
  (lambda (stx)
    (syntax-case stx (:primary :before :after :around)
      ((_ ?generic-function ?qualifier (?class ...) ?has-rest ?closure)
       (syntax
	(%add-method-to-generic-function
	 (let ((qualifier (syntax->datum (syntax ?qualifier))))
	   (case qualifier
	     ((:primary)	':add-primary-method)
	     ((:before)		':add-before-method)
	     ((:after)		':add-after-method)
	     ((:around)		':add-around-method)
	     (else
	      (syntax-violation 'define-method "bad method qualifier" qualifier))))
	 ?generic-function (list ?class ...) ?has-rest ?closure))))))

;;The  following  exists only  for  reference on  "how  to  do it".   It
;;produces the slot name using a subfunction.  All the nested forms LET,
;;WITH-SYNTAX and DATUM->SYNTAX are needed to make it work.  It does not
;;make the code more readable, so the version above is preferred.
;;
#;(define-syntax %add-method
  (lambda (stx)
    (define (qualifier->slot-name qualifier)
      (case qualifier
	((:primary)	':add-primary-method)
	((:before)	':add-before-method)
	((:after)	':add-after-method)
	((:around)	':add-around-method)
	(else
	 (syntax-violation 'add-method "bad method qualifier" qualifier))))
    (syntax-case stx (:primary :before :after :around)
      ((k ?generic-function ?qualifier (?class ...) (?arg ...) . ?body)
       (let ((qualifier (syntax->datum (syntax ?qualifier))))
	 (with-syntax ((?slot-name (datum->syntax (syntax k)
						  (qualifier->slot-name qualifier))))
	   (syntax
	    (%add-method-to-generic-function (quote ?slot-name)
					     ?generic-function (list ?class ...) ?closure))))))))

(define-syntax %add-method-to-generic-function
  ;;Extract  the <generic> object  associated to  ?GENERIC-FUNCTION from
  ;;the  global table; extract  from the  appropriate slot,  the closure
  ;;used  to add  a  method; apply  the  closure to  the ?SIGNATURE  and
  ;;?CLOSURE.
  ;;
  ;;The ?SLOT-NAME value can be a symbol among:
  ;;
  ;;	:add-primary-method
  ;;	:add-before-method
  ;;	:add-after-method
  ;;	:add-around-method
  ;;
  (syntax-rules ()
    ((_ ?slot-name ?generic-function ?signature ?has-rest ?closure)
     ((slot-ref (hashtable-ref *generic-functions* ?generic-function #f) ?slot-name)
      ?signature ?has-rest ?closure))))


;;;; done

)

;;; end of file
