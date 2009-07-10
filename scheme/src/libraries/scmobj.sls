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
    (lists)
    (sentinel))


;;;; helpers

(define-syntax position
  (syntax-rules ()
    ((_ ?element ?list)
     (find (lambda (elm)
	     (eq? ?element elm))
	   ?list))))

;;Given a list of superclasses for class <x>: Build and return the class
;;precedence list for <x> to be used in multimethod dispatching.
;;
(define (%build-class-precedence-list . superclasses)
  (if (null? superclasses)
      superclasses
    (delete-duplicates (concatenate (map (lambda (super)
					   (cons super (class-precedence-list super)))
				      superclasses))
		       eq?)))

;;Given the  list of  direct slot names  for class  <x> and its  list of
;;superclasses: Build and return the class precedence list for <x> to be
;;used in multimethod dispatching.
;;
(define (%build-slot-list direct-slots . superclasses)
  (if (null? superclasses)
      direct-slots
    (delete-duplicates (concatenate (cons direct-slots
					  (map class-slots superclasses)))
		       eq?)))


;;;; access to slots

;;Slot access  should be as  fast as possible,  for this reason  we make
;;this a syntax (it gets expanded in the function).
(define-syntax get-slot
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
	 (or (symbol? v) (sentinel? v)))))

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


;;;; class and instance constructors

(define (%make class-name class . init-args)
  ;;This is a standard make function, in the style of CLOS.
  ;;
  (let ((instance (%allocate-instance class)))
    (%initialise instance class-name init-args)
    instance))

(define (%slot class key value)
  ;;A slot builder user by the  "make" syntax.  Makes sure that the slot
  ;;is valid for the class.
  (if (memq key (class-slots class))
      (cons key value)
    (syntax-violation 'slot "invalid slot keyword" key)))

(define-syntax make
  ;;This  macro exists so  that we  can specify  the slot  names without
  ;;quoting them.
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

(define (%allocate-instance class)
  ;;Build a new  alist initialising all the slots,  but ":class", to the
  ;;sentinel.   The ":class" pair  has to  be the  first element  in the
  ;;alist.
  ;;
  ;;NOTE: The structure  of this function is one of  the reasons why the
  ;;":class" slot is not in the list of slots.
  ;;
  (cons (cons ':class class)
	(map (lambda (x)
	       (cons x sentinel))
	  (class-slots class))))

(define (%initialise instance class-name slot-values)
  ;;Initialise an already allocated instance with the given slot values.
  ;;SLOT-VALUES  is  interpreted  as  an alist  of  slot-name/slot-value
  ;;pairs.
  ;;
  (map (lambda (p)
	 (slot-set! instance (car p) (cdr p)))
    slot-values))

(define-syntax make-class
  ;;Buld a new  class object, initialise all the  slots.  The class name
  ;;is set to the sentinel.   Notice that the class precedence list does
  ;;not include the new class itself.
  ;;
  ;;It  is  possible for  a  class  to add  no  new  slots: this  allows
  ;;subclassing for the only purpose of method dispatching.
  ;;
  ;;The quasiquoted alist is duplicated in DEFINE-CLASS.
  ;;
  (syntax-rules ()
    ((_)
     (make-class ()))
    ((_ (?superclass ...) ?slot-spec ...)
     `((:class . ,<class>)
       (:class-definition-name . ,sentinel)
       (:class-precedence-list
	. ,(%build-class-precedence-list ?superclass ...))
       (:slots
	. ,(%build-slot-list '(?slot-spec ...) ?superclass ...))
       (:direct-slots . (?slot-spec ...))))))

(define-syntax define-class
  ;;Define a binding for a class, giving it a name.
  ;;
  ;;This  syntax duplicates  the MAKE-CLASS  quasiquoted alist  to avoid
  ;;problems with  mutation of values.   To implement DEFINE-CLASS  as a
  ;;wrapper of MAKE-CLASS we should copy  the alist to make it a mutable
  ;;object.  The alist  is small, so we just  duplicate it accepting the
  ;;cost of keeping the two versions in sync.
  ;;
  (syntax-rules ()
    ((_ ?name)
     (define-class ?name ()))
    ((_ ?name (?superclass ...) ?slot-spec ...)
     ;;This  is the  same as  MAKE-CLASS, but  we also  store  the class
     ;;definition name in its slot.
     (define ?name
       `((:class . ,<class>)
	 (:class-definition-name . ?name)
	 (:class-precedence-list
	  . ,(%build-class-precedence-list ?superclass ...))
	 (:slots
	  . ,(%build-slot-list '(?slot-spec ...) ?superclass ...))
	 (:direct-slots . (?slot-spec ...)))))))


;;;; class inspection

(define (subclass? c1 c2)
  (cond ((eq? c1 c2) #t)
;;;These  equalities are  here  because the  original ScmObj  classified
;;;Scheme  built-in values as  objects of  class #t.   They are  no more
;;;needed, but  I leave them  here just in  case the other  code becomes
;;;nostalgic.
;;;
;;; 	((eq? c1 #t) #f)
;;; 	((eq? c2 #t) #t)
	((memq c2 (class-precedence-list c1)) #t)
	(else #f)))

(define (class-of value)
  (cond
   ((and (pair? value)
	 (pair? (car value))
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


;;;; methods dispatching

(define (%compute-applicable-methods call-signature method-table)
  ;;Filter out  from METHOD-TABLE (a  list of methods) the  methods that
  ;;are not applicable  to a tuple of arguments with  types in the tuple
  ;;CALL-SIGNATURE.  Then  sort the list  of applicable methods  so that
  ;;the more specific are the first ones.
  ;;
  (map cdr
    (list-sort
     (lambda (method1 method2)
       (%more-specific-method? method1 method2 call-signature))
     (filter
	 (lambda (method) ;return true if the method is applicable
	   (every subclass? call-signature (car method)))
       method-table))))

(define (%more-specific-method? method1 method2 call-signature)
  ;;Return  true   if  METHOD1  is   more  specific  than   METHOD2  for
  ;;CALL-SIGNATURE.   This  function  is   only  used  as  predicate  by
  ;;%COMPUTE-APPLICABLE-METHODS to sort a list of methods.
  ;;
  (let loop ((signature1 (car method1))
	     (signature2 (car method2))
	     (call-signature call-signature))
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
	     (position class2 cpl))))))))


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

(define (%add-method-to-method-table method-table signature function)
  ;;Helper function that adds a method's signature/function pointed list
  ;;to  the appropriate  alist of  methods (the  METHOD-TABLE argument).
  ;;Return  the method  table (possibly  modified).  It  is used  in the
  ;;expansion of the METHOD-ADDER syntax below.
  ;;
  ;;A new method is added only  if no method with the selected signature
  ;;already  exists.  If  a method  with the  signature  already exists,
  ;;overwrite its function with the new one.
  ;;
  (let ((signature.function (find (lambda (signature.function)
				    (every eq? signature (car signature.function)))
				  method-table)))
    (if signature.function
	(set-cdr! signature.function function)
      (set! method-table (alist-cons signature function method-table))))
  method-table)

(define-syntax define-generic
  (syntax-rules ()
    ((_ ?name ?arg ...)
     (define ?name (make-generic-function ?arg ...)))))

(define-syntax make-generic-function
  (syntax-rules ()
    ((make-generic-function ?arg ...)
     (let* ((generic-object       (create-generic-procedure ?arg ...))
            (interface-procedure  (slot-ref generic-object ':interface-procedure)))
       (hashtable-set! *generic-functions* interface-procedure generic-object)
       interface-procedure))))

(define-syntax create-generic-procedure
  ;;Create the interface procedure of generic functions.
  ;;
  (syntax-rules ()
    ((_ ?arg-name ...)
     (let ((primary-method-table '()) ;these are all alists
	   (before-method-table  '())
	   (after-method-table   '())
	   (around-method-table  '()))
       (let-syntax ((method-adder (syntax-rules ()
				    ((_ ?method-table)
				     (lambda (signature function)
				       (set! ?method-table
					     (%add-method-to-method-table
					      ?method-table signature function)))))))
	 (make <generic>
	   :add-primary-method (method-adder primary-method-table)
	   :add-before-method  (method-adder before-method-table)
	   :add-after-method   (method-adder after-method-table)
	   :add-around-method  (method-adder around-method-table)
	   :interface-procedure
	   (lambda (?arg-name ... . rest)
	     (let-syntax ((apply-function/stx (syntax-rules ()
						((_ ?func-name)
						 (apply ?func-name ?arg-name ... rest))))
			  (consume-method (syntax-rules ()
					    ((_ ?method-table)
					     (begin0
						 (car ?method-table)
					       (set! ?method-table (cdr ?method-table)))))))
	       (letrec* ((signature
			  (list (class-of ?arg-name) ...))

			 (applicable-primary-methods
			  (%compute-applicable-methods signature primary-method-table))

			 (applicable-before-methods
			  (%compute-applicable-methods signature before-method-table))

			 (applicable-after-methods
			  (%compute-applicable-methods signature after-method-table))

			 (applicable-around-methods
			  (%compute-applicable-methods signature around-method-table))

			 (primary-method-called?  #f)
			 (reject-recursive-calls? #f)

			 (is-a-next-method-available?
			  (lambda ()
			    (not (if primary-method-called?
				     (null? applicable-primary-methods)
				   (and (null? applicable-around-methods)
					(null? applicable-primary-methods))))))

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
			      (when (null? applicable-primary-methods)
				(assertion-violation 'call-methods
				  "called next method but no more :primary methods available"))
			      (apply-function/stx (consume-method applicable-primary-methods)))

			     ((null? applicable-primary-methods)
			      ;;Raise   an  error  if   no  applicable
			      ;;methods.
			      (assertion-violation 'call-methods
				"no method defined for these argument classes"))

			     ((not (null? applicable-around-methods))
			      ;;If around  methods exist: we  apply them
			      ;;first.   It is  expected that  an around
			      ;;method   invokes   CALL-NEXT-METHOD   to
			      ;;evaluate the primary methods.
			      (apply-function/stx (consume-method applicable-around-methods)))

			     (else
			      ;;Apply  the  methods: before,  primary,
			      ;;after.  Return the return value of the
			      ;;primary.
			      (set! reject-recursive-calls? #t)
			      (for-each
				  (lambda (f) (apply-function/stx f))
				applicable-before-methods)
			      (set! reject-recursive-calls? #f)
			      (set! primary-method-called? #t)
			      (begin0
				  (apply-function/stx (consume-method applicable-primary-methods))
				(set! reject-recursive-calls? #t)
				(for-each
				    (lambda (f) (apply-function/stx f))
				  applicable-after-methods)))))))
		 (parameterize ((next-method-func-parm call-methods)
				(next-method-pred-parm is-a-next-method-available?))
		   (call-methods)))))))))))


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
  ;;The process is split into three different macros for readability.
  ;;
  ;;  The ?QUALIFIER pattern variable is one of the literals:
  ;;
  ;;     :primary
  ;;     :before
  ;;     :after
  ;;     :around
  ;;
  ;;it  defaults to ":primary".
  ;;
  (syntax-rules (:primary :before :after :around)
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
     ;;Matches  the form  when all  the arguments  have  been processed.
     ;;This MUST come before the one below.
     (add-method ?generic-function ?qualifier (?class ...)
		 (lambda (?arg ...) . ?body)))

    ((_ ?generic-function ?qualifier ?rest (?class ...) (?arg ...) . ?body)
     ;;Matches the form  when all the arguments have  been processed and
     ;;only the  rest argument is there.   This MUST come  after the one
     ;;above.
     (add-method ?generic-function ?qualifier (?class ...)
		 (lambda (?arg ... . ?rest) . ?body)))))

(define-syntax add-method
  (lambda (stx)
    (syntax-case stx (:primary :before :after :around)
      ((_ ?generic-function ?qualifier (?class ...) ?closure)
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
	 ?generic-function (list ?class ...) ?closure))))))

;;The  following  exists only  for  reference on  "how  to  do it".   It
;;produces the slot name using a subfunction.  All the nested forms LET,
;;WITH-SYNTAX and DATUM->SYNTAX are needed to make it work.  It does not
;;make the code more readable, so the version above is preferred.
;;
#;(define-syntax %dispatch-method-to-adder
  (lambda (stx)
    (define (qualifier->slot-name qualifier)
      (case qualifier
	((:primary)	':add-primary-method)
	((:before)	':add-before-method)
	((:after)	':add-after-method)
	((:around)	':add-around-method)
	(else
	 (syntax-violation 'define-method "bad method qualifier" qualifier))))
    (syntax-case stx (:primary :before :after :around)
      ((k ?generic-function ?qualifier (?class ...) (?arg ...) . ?body)
       (let ((qualifier (syntax->datum (syntax ?qualifier))))
	 (with-syntax ((?slot-name (datum->syntax (syntax k)
						  (qualifier->slot-name qualifier))))
	   (syntax
	    (%add-method-to-generic-function (quote ?slot-name)
					     ?generic-function (list ?class ...)
					     (lambda (?arg ... ) . ?body)))))))))

(define-syntax %add-method-to-generic-function
  ;;Extract  the <generic> object  associated to  ?GENERIC-FUNCTION from
  ;;the  global table; extract  from the  appropriate slot,  the closure
  ;;used  to add  a  method; apply  the  closure to  the ?SIGNATURE  and
  ;;?FUNCTION.
  ;;
  ;;The ?SLOT-NAME value can be a symbol among:
  ;;
  ;;	:add-primary-method
  ;;	:add-before-method
  ;;	:add-after-method
  ;;	:add-around-method
  ;;
  (syntax-rules ()
    ((_ ?slot-name ?generic-function ?signature ?function)
     ((slot-ref (hashtable-ref *generic-functions* ?generic-function #f) ?slot-name)
      ?signature ?function))))


;;;; done

)

;;; end of file
