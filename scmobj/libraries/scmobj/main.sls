;;;
;;;Part of: Nausicaa-ScmObj
;;;Contents: object system for Scheme
;;;Date: Tue Nov 11, 2008
;;;Time-stamp: <2008-11-22 18:27:53 marco>
;;;
;;;Abstract
;;;
;;;	This  is a port  to R6RS  and Ikarus  of ScmObj  by Dorai
;;;	Sitaram.  The original code is available at:
;;;
;;;	 <http://www.ccs.neu.edu/home/dorai/scmobj/scmobj.html>
;;;
;;;	(last checked  Thu Nov 13, 2008).  The  original code has
;;;	been  a  little overhauled  to  make  it  work with  R6RS
;;;	libraries.
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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


;;;; Setup.

(library (scmobj)
  (export
    ;;Built in classes.
    <class> <entity-class>

    <circular-list> <dotted-list> <proper-list> <list> <pair>
    <vector> <bytevector> <hashtable> <record> <condition>
    <binary-port> <textual-port> <input-port> <output-port> <port>
    <fixnum> <flonum> <integer> <integer-valued>
    <rational> <rational-valued> <real> <real-valued>
    <complex> <number>

    ;; Constructors.
    define-class define-generic define-method
    make-class make make-generic-function

    ;;Class inspection.
    class-of
    class-definition-name class-precedence-list
    class-slots class-direct-slots
    class? instance? is-a? subclass?

    ;;Slot accessors.
    slot-ref slot-set!

    ;;Next method interface.
    call-next-method next-method?)
  (import (rnrs)
    (rnrs mutable-pairs (6))
;;    (only (ikarus) pretty-print printf)
    (srfi lists)
    (srfi parameters))


;;;; Helper functions and syntaxes: generic routines.

(define-syntax position
  (syntax-rules ()
    ((_ ?element ?list)
     (list-index
	 (lambda (elm)
	   (eq? ?element elm))
       ?list))))

(define-syntax begin0
  (syntax-rules ()
    [(_ ?expr1 ?expr2 ...)
     (call-with-values
	 (lambda () ?expr1)
       (lambda x
	 ?expr2 ...
	 (apply values x)))]))


;;;; Helper functions and syntaxes: class instantiation.

;;Given a list of superclasses for class <x>, build and return the class
;;precedence list for <x> to be used in multimethod dispatching.
;;
(define (build-class-precedence-list . superclasses)
  (if (null? superclasses)
      superclasses
    (delete-duplicates
     (concatenate
      (map
	  (lambda (super)
	    (cons super (class-precedence-list super)))
	superclasses))
     eq?)))

;;Given the  list of  direct slot names  for class  <x> and its  list of
;;superclasses: build and return the class precedence list for <x> to be
;;used in multimethod dispatching.
;;
(define (build-slot-list direct-slots . superclasses)
  (if (null? superclasses)
      direct-slots
    (delete-duplicates
     (concatenate (cons direct-slots
			(map (lambda (s)
			       (class-slots s))
			  superclasses)))
     eq?)))


;;;; Access to slots.

;;Slot access  should be as  fast as possible,  for this reason  we make
;;this a syntax (it gets expanded in the function).
(define-syntax get-slot
  (syntax-rules ()
    ((_ ?caller ?object ?slot-name)
     (or (assq ?slot-name ?object)
	 (assertion-violation ?caller
	   "trying to access nonexistent slot" ?slot-name)))))

(define (slot-ref object slot-name)
  (cdr (get-slot 'slot-ref object slot-name)))

(define (slot-set! object slot-name value)
  (set-cdr! (get-slot 'slot-set! object slot-name) value))


;;;; Class inspection functions.

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
	 (if (memq <entity-class> full-class-list)
	     #t
	   ;;Check that all the slots are here.
	   (let ((object-slots (cdr (map car object))))
	     (every
		 (lambda (slot-name)
		   (let ((r (memq slot-name object-slots)))
		     r))
	       (class-slots class))))
	 ;;Make sure that it returns #t not a generic true.
	 #t)))

;;; --------------------------------------------------------------------

;;It has to be:
;;
;;   (:class . class-object)
;;
(define (first-class-slot? value)
  (and (pair? value)
       (eq? ':class (car value))
       (eq? <class> (cdr value))))

;;It has to be:
;;
;;   (:class-definition-name . symbol)
;;
(define (second-class-slot? value)
  (and (pair? value)
       (eq? ':class-definition-name (car value))
       (symbol? (cdr value))))

;; It has to be:
;;
;;   (:class-precedence-list . (... classes ...))
;;
(define (third-class-slot? value)
  (and (pair? value)
       (eq? ':class-precedence-list (car value))
       (let ((v (cdr value)))
	 (and (proper-list? v)
	      (every class? v)))))

;;It has to be:
;;
;;   (:slots . (... symbols ...))
;;
(define (fourth-class-slot? value)
  (and (pair? value)
       (eq? ':slots (car value))
       (let ((v (cdr value)))
	 (and (proper-list? v)
	      (every symbol? v)))))

;;It has to be:
;;
;;   (:direct-slots . (... symbols ...))
;;
(define (fifth-class-slot? value)
  (and (pair? value)
       (eq? ':direct-slots (car value))
       (let ((v (cdr value)))
	 (and (proper-list? v)
	      (every symbol? v)))))


;;;; Built in classes.

(define <class>
  '#0=((:class . #0#)
       (:class-definition-name . <class>)
       (:class-precedence-list . ())
       (:slots . (:class-definition-name
		  :class-precedence-list :slots :direct-slots))
       (:direct-slots . (:class-definition-name
			 :class-precedence-list :slots :direct-slots))))

(define <entity-class>
  `((:class . ,<class>)
    (:class-definition-name . <entity-class>)
    (:class-precedence-list . ())
    (:slots . ())
    (:direct-slots . ())))

;;; --------------------------------------------------------------------

(define-syntax define-entity-class
  (syntax-rules ()
    ((_ ?name)
     (define-entity-class ?name ()))
    ((_ ?name (?superclass ...))
     (define ?name
       `((:class . ,<entity-class>)
	 (:class-definition-name . ?name)
	 (:class-precedence-list . ,(build-class-precedence-list ?superclass ...))
	 (:slots . ())
	 (:direct-slots . ()))))
    ((_ ?name ?superclass)
     (define-entity-class ?name (?superclass)))))

(define-entity-class <pair>)
(define-entity-class <list>		<pair>)
(define-entity-class <circular-list>	<list>)
(define-entity-class <dotted-list>	<list>)
(define-entity-class <proper-list>	<list>)
(define-entity-class <vector>)
(define-entity-class <hashtable>)

(define-entity-class <port>)
(define-entity-class <input-port>	<port>)
(define-entity-class <output-port>	<port>)
(define-entity-class <binary-port>	<port>)
(define-entity-class <textual-port>	<port>)

(define-entity-class <record>)
(define-entity-class <condition>	<record>)
(define-entity-class <bytevector>)

(define-entity-class <number>)
(define-entity-class <complex>		<number>)
(define-entity-class <real-valued>	<complex>)
(define-entity-class <real>		<real-valued>)
(define-entity-class <rational-valued>	<real>)
(define-entity-class <flonum>		<real>)
(define-entity-class <rational>		<rational-valued>)
(define-entity-class <integer-valued>	<rational-valued>)
(define-entity-class <integer>		<integer-valued>)
(define-entity-class <fixnum>		<integer>)

;;Other possible classes that require more library loading:
;;
;;	<stream>	stream?
;;


;;;; Class and instance constructors.

;;This is a "standard" make function, in style with CLOS.
;;
(define (make class . init-args)
  (let ((instance (allocate-instance class)))
    (initialise instance init-args)
    instance))

;;Build  a  new alist  initialising  all  the  slots, but  ":class",  to
;;":uninitialized".  The  ":class" pair has  to be the first  element in
;;the alist.
;;
;;The form of this function is  one of the reasons why the ":class" slot
;;is not in the list of slots.
;;
(define (allocate-instance class)
  (cons (cons ':class class)
	(map (lambda (x)
	       (cons x ':uninitialized))
	  (class-slots class))))

;;Interpret SLOT-VALUES  as list of alternate symbols  and values, where
;;the symbols are slot names.
;;
;;We are not  asserting (as we should) that:  (1) "(car slot-values)" is
;;not ":class"; (2) SLOT-VALUES has an even number of elements.  Because
;;of this errors with unclear message may happen.
;;
(define (initialise instance slot-values)
  (unless (null? slot-values)
    (slot-set! instance (car slot-values) (cadr slot-values))
    (initialise instance (cddr slot-values))))

;;It  is  possible  for  a  class  to add  no  new  slots:  this  allows
;;subclassing for the only purpose of method dispatching.
;;
;;Notice that the  class precedence list does not  include the new class
;;itself.
;;
(define-syntax make-class
  (syntax-rules ()
    ((_)
     (make-class ()))
    ((_ (?superclass ...) ?slot-spec ...)
     `((:class . ,<class>)
       (:class-definition-name . :uninitialized)
       (:class-precedence-list
	. ,(build-class-precedence-list ?superclass ...))
       (:slots
	. ,(build-slot-list '(?slot-spec ...) ?superclass ...))
       (:direct-slots . (?slot-spec ...))))))

;;Define a binding for a class, giving it a name.
;;
(define-syntax define-class
  (syntax-rules ()
    ((_ ?name)
     (define-class ?name ()))
    ((_ ?name (?superclass ...) ?slot-spec ...)
     (define ?name
       ;;This is  the same as  MAKE-CLASS, but we also  store the
       ;;class definition name in its slot.
       `((:class . ,<class>)
	 (:class-definition-name . ?name)
	 (:class-precedence-list
	  . ,(build-class-precedence-list ?superclass ...))
	 (:slots
	  . ,(build-slot-list '(?slot-spec ...) ?superclass ...))
	 (:direct-slots . (?slot-spec ...)))))))


;;;; class inspection

(define (subclass? c1 c2)
  (cond ((eq? c1 c2) #t)
	((eq? c1 #t) #f)
	((eq? c2 #t) #t)
	((memq c2 (class-precedence-list c1)) #t)
	(else #f)))

(define (class-of value)
  (cond
   ((and (pair? value)
	 (pair? (car value))
	 (eq? ':class (caar value)))
    (cdar value))

   ;;Order does matter here!!!
   ((fixnum?	value)		<fixnum>)
   ((integer?	value)		<integer>)
   ((rational?	value)		<rational>)
   ((integer-valued? value)	<integer-valued>)
   ((rational-valued? value)	<rational-valued>)
   ((flonum?	value)		<flonum>)
   ((real?	value)		<real>)
   ((real-valued? value)	<real-valued>)
   ((complex?	value)		<complex>)
   ((number?	value)		<number>)

   ((vector?	value)		<vector>)
   ((hashtable? value)		<hashtable>)

   ((input-port? value)		<input-port>)
   ((output-port? value)	<output-port>)
   ((binary-port? value)	<binary-port>)
   ((textual-port? value)	<textual-port>)
   ((port? value)		<port>)

   ((condition? value)		<condition>)
   ((record? value)		<record>)
   ((bytevector? value)		<bytevector>)

   ((circular-list value)	<circular-list>)
   ((dotted-list? value)	<dotted-list>)
   ((proper-list? value)	<proper-list>)
   ((list? value)		<list>)
   ((pair? value)		<pair>)
   (else			#t)))


;;;; methods dispatching

(define (more-specific-method method1 method2 call-signature)
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

(define (compute-applicable-methods call-signature method-table)
  (map cdr
    (list-sort
     (lambda (method1 method2)
       (more-specific-method method1 method2 call-signature))
     (filter
	 (lambda (method)
	   (every subclass? call-signature (car method)))
       method-table))))


;;;; Next method implementation.

(define-syntax call-next-method
  (syntax-rules ()
    ((_)
     (when (scmobj:the-next-method-func)
       ((scmobj:the-next-method-func))))))

(define-syntax next-method?
  (syntax-rules ()
    ((_)
     (when (scmobj:the-next-method-pred)
       ((scmobj:the-next-method-pred))))))

(define scmobj:the-next-method-func (make-parameter #f))
(define scmobj:the-next-method-pred (make-parameter #f))


;;;; Generic functions.

;;A 'generic  function' is  basically a couple  of values:  an interface
;;procedure and an object of class <generic>.
;;
;;The interface procedure is  stored in the :interface-procedure slot of
;;the object  and is  used to apply  the generic  function to a  list of
;;arguments.
;;
(define-class <generic> ()
  :interface-procedure
  :add-primary-method
  :add-before-method
  :add-after-method
  :add-around-method)

;;This  is  an alist  that  will hold  all  the  generic functions  ever
;;created.  The  keys are the  interface procedures, the values  are the
;;<generic> objects.
(define *generic-procedures* (make-eq-hashtable))

;;Helper  function  that  adds  a  signature/func pointed  list  to  the
;;appropriate  alist  of methods  (the  METHOD-TABLE  argument).  A  new
;;method is added only if no method with the signature already exists.
(define (add-method-to-method-table
	 method-table method-signature method-func)
  (unless (any (lambda (signature.function)
		 ;;If  a  method  with  the  signature  already  exists,
		 ;;overwrite its function with the new one.
		 (and (every eq? method-signature
			     (car signature.function))
		      (begin
			(set-cdr! signature.function method-func)
			#t)))
	    method-table)
    (set! method-table
	  (alist-cons method-signature method-func method-table)))
  method-table)

;;Helper syntax for the definition of  the closure that adds a method to
;;the appropriate method table.
(define-syntax method-adder
  (syntax-rules ()
    ((_ ?method-table)
     (lambda (method-signature method-func)
       (set! ?method-table
	     (add-method-to-method-table
	      ?method-table method-signature method-func))))))

(define-syntax create-generic-procedure
  (syntax-rules ()
    ((create-generic-procedure ?arg-name ...)
     (let ((primary-method-table '())
	   (before-method-table '())
	   (after-method-table '())
	   (around-method-table '()))
       (make <generic>
	 ':add-primary-method (method-adder primary-method-table)
	 ':add-before-method  (method-adder before-method-table)
	 ':add-after-method   (method-adder after-method-table)
	 ':add-around-method  (method-adder around-method-table)
	 ':interface-procedure
	 (lambda (?arg-name ... . rest)
	   (letrec
	       ((signature
		 (list (class-of ?arg-name) ...))

		(applicable-primary-methods
		 (compute-applicable-methods signature primary-method-table))

		(applicable-before-methods
		 (compute-applicable-methods signature before-method-table))

		(applicable-after-methods
		 (reverse
		  (compute-applicable-methods signature after-method-table)))

		(applicable-around-methods
		 (compute-applicable-methods signature around-method-table))

		(primary-method-called #f)

		(apply-function
		 (lambda (f) (apply f ?arg-name ... rest)))

		(next-method-pred
		 (lambda ()
		   (if primary-method-called
		       (not (null? applicable-primary-methods))
		     (or (not (null? applicable-around-methods))
			 (not (null? applicable-primary-methods))))))

		(next-method-func
		 (lambda ()
		   (let-syntax
		       ((consume-method
			 (syntax-rules ()
			   ((_ ?method-table)
			    (begin0
				(car ?method-table)
			      (set! ?method-table
				    (cdr ?method-table)))))))
		     (cond

		      ;;We enter here only  if a primary method has been
		      ;;called   and,   in   its   body,   a   call   to
		      ;;CALL-NEXT-METHOD if performed.
		      (primary-method-called
		       (apply-function
			(consume-method applicable-primary-methods)))

		      ;;If around  methods exists: we  apply them rather
		      ;;than the  primary ones.  It is  expected that an
		      ;;around method invokes CALL-NEXT-METHOD.
		      ((not (null? applicable-around-methods))
		       (apply-function
			(consume-method applicable-around-methods)))

		      ;;Raise an error if no applicable methods.
		      ((null? applicable-primary-methods)
		       (assertion-violation
			   'next-method-func
			 "no method defined for these argument classes"))

		      ;;Apply  the   methods:  before,  primary,  after.
		      ;;Return the return value of the primary.
		      (else (set! primary-method-called #t)
			    (for-each
				apply-function
			      applicable-before-methods)
			    (begin0
				(apply-function
				 (consume-method applicable-primary-methods))
			      (for-each
				  apply-function
				applicable-after-methods))))))))
	     (parameterize ((scmobj:the-next-method-func next-method-func)
			    (scmobj:the-next-method-pred next-method-pred))
	       (next-method-func)))))))))

;;; --------------------------------------------------------------------

;;Helper   function  that   adds   a  new   generic   function  to   the
;;*GENERIC-PROCEDURES*  table.  This  function  is not  expanded in  the
;;MAKE-GENERIC-FUNCTION (as it was  in the original ScmObj code) because
;;doing so would modify a  variable exported from this library (and this
;;is forbidden by R6RS or Ikarus).
(define (register-new-generic-function interface-procedure generic-object)
  (hashtable-set! *generic-procedures* interface-procedure generic-object))

(define-syntax make-generic-function
  (syntax-rules ()
    ((make-generic-function ?arg ...)
     (let* ((generic-object
	     (create-generic-procedure ?arg ...))
            (interface-procedure
	     (slot-ref generic-object ':interface-procedure)))
       (register-new-generic-function interface-procedure generic-object)
       interface-procedure))))

(define-syntax define-generic
  (syntax-rules ()
    ((_ ?name ?arg ...)
     (define ?name (make-generic-function ?arg ...)))))


;;;; Methods.

;;What follows  is the documentation of the  DEFINE-METHOD syntax below.
;;The pattern matching has tree phases:
;;
;;1. the method  is recognised as primary, before,  after or around, and
;;two accumulator lists are initialised to nil;
;;
;;2. the method arguments are accumulated  in a list of "with class" and
;;a list of "without class";
;;
;;3. the  method is added to  the appropriate collection  in the generic
;;function.
;;
;;The ?QUALIFIER pattern variable is one of the literals:
;;
;;	:primary :before :after :around
;;
;;it defaults to ":primary".
;;
;;The  ?SARGS pattern variable  is a  list accumulator  for specialising
;;arguments.  A specialising  argument is a method argument  for which a
;;class was specified.
;;
;;The   ?NSARGS   pattern   variable   is   a   list   accumulator   for
;;non-specialising arguments.   A non-specialising argument  is a method
;;argument for which a class was NOT specified.
;;
;;The  ?REST pattern  variable is  used  to hold  the name  of the  rest
;;argument.
;;
;;The  ?ARG pattern  variable is  the  next argument  to be  accumulated
;;somewhere.
;;
;;The  ?SA   pattern  variable   is  a  specialising   argument  already
;;accumulated.
;;
;;The  ?NSA  pattern variable  is  a  non-specialising argument  already
;;accumulated.
;;
;;Example:
;;
;;  (define-method swirl-vector ((vec <vector>) idx . args)
;;	---)
;;
;;here  VEC is  a  specialising argument  of  class <vector>,  IDX is  a
;;non-specialising argument, ARGS is the name of the rest argument.
;;
;;A 'signature' is  a list of classes: given a  list of generic function
;;call arguments, the arguments must  match these classes for the method
;;to be applicable.  Example:
;;
;;  (define-method twist-vector ((vec <vector>) (idx <int>))
;;     ---)
;;
;;the signature is: "(list <vector> <int>)".
;;

(define (add-method-to-generic-function
	 slot-name generic-function method-signature method-func)
  ((slot-ref (hashtable-ref *generic-procedures* generic-function #f) slot-name)
   method-signature method-func))

;;; --------------------------------------------------------------------

(define-syntax define-method
  (syntax-rules (:primary :before :after :around)

    ((_ 1 ?generic-function ?qualifier ?args . ?body)
     (define-method 2 ?generic-function ?qualifier ?args
       ()  ;;specialising args
       ()  ;;non-specialising args
       . ?body))

    ;;Matches  the form when  the next  argument to  be processed  has a
    ;;class.
    ((_ 2 ?generic-function ?qualifier ((?arg ?class) . ?args) (?sa ...) ?nsargs . ?body)
     (define-method 2 ?generic-function ?qualifier
       ?args (?sa ... (?arg ?class)) ?nsargs . ?body))

    ;;Matches the  form when  the next argument  to be processed  has no
    ;;class.
    ((_ 2 ?generic-function ?qualifier (?arg . ?args) ?sargs (?nsa ...) . ?body)
     (define-method 2 ?generic-function ?qualifier
       ?args ?sargs (?nsa ... ?arg)  . ?body))

    ;;Matches the form when all the arguments have been processed.
    ((_ 2 ?generic-function ?qualifier () ?sargs ?nsargs . ?body)
     (define-method 3 ?generic-function ?qualifier
       ?sargs ?nsargs . ?body))

    ;;Matches the  form when all  the arguments have been  processed and
    ;;only the rest argument is there.
    ((_ 2 ?generic-function ?qualifier ?rest ?sargs (?nsa ...) . ?body)
     (define-method 3 ?generic-function ?qualifier
       ?sargs (?nsa ... . ?rest) . ?body))

    ((_ 3 ?generic-function :primary ((?arg ?class) ...) ?nsargs . ?body)
     (add-method-to-generic-function ':add-primary-method
      ?generic-function (list ?class ...)
      (lambda (?arg ... . ?nsargs) . ?body)))

    ((_ 3 ?generic-function :before ((?arg ?class) ...) ?nsargs . ?body)
     (add-method-to-generic-function ':add-before-method
      ?generic-function (list ?class ...)
      (lambda (?arg ... . ?nsargs) . ?body)))

    ((_ 3 ?generic-function :after ((?arg ?class) ...) ?nsargs . ?body)
     (add-method-to-generic-function ':add-after-method
      ?generic-function (list ?class ...)
      (lambda (?arg ... . ?nsargs) . ?body)))

    ((_ 3 ?generic-function :around ((?arg ?class) ...) ?nsargs . ?body)
     (add-method-to-generic-function ':add-around-method
      (list ?class ...)
      (lambda (?arg ... . ?nsargs) . ?body)))

    ((_ ?generic-function :primary ?args . ?body)
     (define-method 1 ?generic-function :primary ?args . ?body))

    ((_ ?generic-function :before ?args . ?body)
     (define-method 1 ?generic-function :before ?args . ?body))

    ((_ ?generic-function :after ?args . ?body)
     (define-method 1 ?generic-function :after ?args . ?body))

    ((_ ?generic-function :around ?args . ?body)
     (define-method 1 ?generic-function :around ?args . ?body))

    ((_ ?generic-function ?args . ?body)
     (define-method 1 ?generic-function :primary ?args . ?body))))


;;;; Done.

) ;; end of library form

;;; end of file
