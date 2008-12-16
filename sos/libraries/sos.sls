;;;
;;;Part of: Nausicaa/SOS
;;;Contents: object system for R6RS
;;;Date: Wed Dec 10, 2008
;;;Time-stamp: <2008-12-16 10:11:16 marco>
;;;
;;;Abstract
;;;
;;;	This  is  a  Scheme  library implementing  a  somewhat-CLOS-like
;;;	object  system with  metaobject protocol.   It has  been written
;;;	with inspiration  from ScmObj by Dorai Sitaram  and Tiny-CLOS by
;;;	Gregor Kiczales at Xerox Parc.
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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



;;;; setup

(library (sos)
  (export
    make initialize

    ;; class inspection
    class-definition-name
    class-direct-supers
    class-precedence-list
    class-direct-slots
    class-slots

    )
  (import (r6rs)
    (srfi lists))



;;;; object records

(define-record-type object-record
  (fields (mutable class)))

(define-record-type instance-record
  (parent object-record)
  (fields (immutable class)))

(define-record-type instance-with-slots-record
  (parent instance-record))

(define-record-type class-record
  (parent object-record)
  (fields (immutable definition-name	class-definition-name)
	  (immutable direct-supers	class-direct-supers)
	  (immutable all-the-supers	class-precedence-list)))

(define-record-type base-class-record
  (parent class-record)
  (fields (immutable direct-slots	class-direct-slots)
	  (immutable all-the-slots	class-slots)))

(define-record-type primitive-class-record
  (parent class-record))

(define-record-type slot-record
  (fields (immutable name)
	  (immutable getter)
	  (immutable setter)
	  (immutable init-value)
	  (immutable init-form)
	  (immutable init-thunk)))

(define-record-type method
  (fields (immutable signature method-signature)
	  (immutable closure method-closure)))

(define-record-type methods-table
  (fields (mutable list-of-methods)
	  (mutable cache-of-applications)))

(define-record-type generic-record
  (fields (immutable primary-method-table	primary-method-table)
	  (immutable before-method-table	before-method-table)
	  (immutable after-method-table		after-method-table)
	  (immutable around-method-table	around-method-table)))


;;;; method tables

(define (add-method-to-method-table method-table method)
  (define-syntax register-method
    (syntax-rules ()
      ((_ ?method)
       (method-table-list-of-methods
	?method-table (cons ?method (method-table-list-of-mehtods ?method-table))))))

  (define-syntax invalidate-cache-of-applications
    (syntax-rules ()
      ((_ ?method-table)
       (method-table-cache-of-applications method-table #f))))

  (define-syntax replace-method-closure
    (syntax-rules ()
      ((_ ?stored-method ?method)
       (method-closure stored-method (method-closure method))
       #t)))

  (let ((signature (method-signature method)))
    (unless (any (lambda (stored-method)
		   (and (every eq? signature (method-signature stored-method))
			(replace-method-closure stored-method method)))
	      (method-table-list-of-methods method-table))
      (register-method method)
      (invalidate-cache-of-applications method-table))))

(define (more-specific-method? method1 method2 call-signature)
  (let loop ((signature1	(method-signature method1))
	     (signature2	(method-signature method2))
	     (call-signature	call-signature))
    (let ((class1 (car signature1))
	  (class2 (car signature2)))
      (cond
       ((eq? class1 class2)
	(loop (cdr signature1) (cdr signature2) (cdr call-signature)))
       ((subclass? class1 class2) #t)
       ((subclass? class2 class1) #f)
       (else
	(let* ((class (car call-signature))
	       (cpl (if (eq? class <object>)
			'()
		      (cons class (class-precedence-list class)))))
	  (< (position class1 cpl)
	     (position class2 cpl))))))))

(define (compute-applicable-methods call-signature method-table)
  (list-sort (lambda (method1 method2)
	       (more-specific-method? method1 method2 call-signature))
	     (filter
		 (lambda (method)
		   (every subclass? call-signature (method-signature method)))
	       (method-table-list-of-methods method-table))))


;;;; Next method implementation.

(define-syntax call-next-method
  (syntax-rules ()
    ((_)
     (when (the-next-method-func)
       ((the-next-method-func))))))

(define-syntax next-method?
  (syntax-rules ()
    ((_)
     (when (the-next-method-pred)
       ((the-next-method-pred))))))

(define the-next-method-func (make-parameter #f))
(define the-next-method-pred (make-parameter #f))


;;;; Generic functions.

(define make-generic
  (let ((record (make-generic-record (make-eq-hashtable)
				     (make-eq-hashtable)
				     (make-eq-hashtable)
				     (make-eq-hashtable))))
    (case-lambda
     (()
      record)
     ((args)
      (letrec*
	  ((signature (map class-of args))

	   (applicable-primary-methods
	    (compute-applicable-methods signature (primary-method-table record)))

	   (applicable-before-methods
	    (compute-applicable-methods signature (before-method-table record)))

	   (applicable-after-methods
	    (reverse
	     (compute-applicable-methods signature (after-method-table record))))

	   (applicable-around-methods
	    (compute-applicable-methods signature (around-method-table record)))

	   (primary-method-called #f)

	   (apply-function
	    (lambda (f) (apply f args)))

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

		 ;;We  enter here  only  if a  primary  method has  been
		 ;;called and,  in its body, a  call to CALL-NEXT-METHOD
		 ;;if performed.
		 (primary-method-called
		  (apply-function
		   (consume-method applicable-primary-methods)))

		 ;;If around  methods exists: we apply  them rather than
		 ;;the  primary ones.   It  is expected  that an  around
		 ;;method invokes CALL-NEXT-METHOD.
		 ((not (null? applicable-around-methods))
		  (apply-function
		   (consume-method applicable-around-methods)))

		 ;;Raise an error if no applicable methods.
		 ((null? applicable-primary-methods)
		  (assertion-violation
		      'next-method-func
		    "no method defined for these argument classes"))

		 ;;Apply  the methods:  before, primary,  after.  Return
		 ;;the return value of the primary.
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
	(parameterize ((the-next-method-func next-method-func)
		       (the-next-method-pred next-method-pred))
	  (next-method-func)))))))

(define-syntax define-generic
  (syntax-rules ()
    ((_ ?name ?arg ...)
     (define ?name (make-generic ?arg ...)))))



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

(define-syntax make-method-adder
  (syntax-rules ()
    ((_ ?table-getter)
     (define (add-primary-method generic-function signature closure)
       (let ((generic-record (generic-function)))
	 (hashtable-set! (?table-getter generic-record) signature
			 (make-method-record signature closure)))))))

(make-method-adder primary-method-table)
(make-method-adder before-method-table)
(make-method-adder after-method-table)
(make-method-adder around-method-table)

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



;;;; predefined classes

(define <class>
  (let ((slots (vector
		(make-slot-record ':class
				  object-record-class #f #f #f #f)
		(make-slot-record ':class-definition-name
				  class-definition-name #f #f #f #f)
		(make-slot-record ':class-direct-supers
				  class-direct-supers #f #f #f #f)
		(make-slot-record ':class-precedence-list
				  class-precedence-list #f #f #f #f)
		(make-slot-record ':class-direct-slots
				  class-direct-slots #f #f #f #f)
		(make-slot-record ':class-slots
				  class-slots #f #f #f #f)))
	(r (make-base-class-record
	    #f			       ;; class
	    '<class>		       ;; class-definition-name
	    '()			       ;; direct-supers
	    '()			       ;; class-precedence-list
	    base-class-record-slots    ;; direct-slots
	    base-class-record-slots))) ;; all-the-slots
    (object-record-class-set! r r)
    r))

(define <primitive-class>
  (make-base-class-record
   <class>	      ;; class
   '<primitive-class> ;; class-definition-name
   '()		      ;; direct-supers
   '()		      ;; class-precedence-list
   '()		      ;; direct-slots
   '()))	      ;; all-the-slots

(define-syntax define-primitive-class
  (syntax-rules ()
    ((_ ?name)
     (define-primitive-class ?name ()))
    ((_ ?name (?superclass ...))
     (define ?name
       (make-primitive-class-record
	<primitive-class>
	?name
	(list ?superclass ...)
	(build-class-precedence-list ?superclass ...))))
    ((_ ?name ?superclass)
     (define-primitive-class ?name (?superclass)))))

(define-primitive-class <pair>)
(define-primitive-class <list>			<pair>)
(define-primitive-class <circular-list>		<list>)
(define-primitive-class <dotted-list>		<list>)
(define-primitive-class <proper-list>		<list>)
(define-primitive-class <vector>)
(define-primitive-class <hashtable>)

(define-primitive-class <port>)
(define-primitive-class <input-port>		<port>)
(define-primitive-class <output-port>		<port>)
(define-primitive-class <binary-port>		<port>)
(define-primitive-class <textual-port>		<port>)

(define-primitive-class <record>)
(define-primitive-class <condition>		<record>)
(define-primitive-class <bytevector>)

(define-primitive-class <number>)
(define-primitive-class <complex>		<number>)
(define-primitive-class <real-valued>		<complex>)
(define-primitive-class <real>			<real-valued>)
(define-primitive-class <rational-valued>	<real>)
(define-primitive-class <flonum>		<real>)
(define-primitive-class <rational>		<rational-valued>)
(define-primitive-class <integer-valued>	<rational-valued>)
(define-primitive-class <integer>		<integer-valued>)
(define-primitive-class <fixnum>		<integer>)

;;Other possible classes that require more library loading:
;;
;;	<stream>	stream?
;;



;;;; inspection functions





;;;; code

(define make #f)
(define initialize #f)


;;;; slot access

(define (slot-setter-ref class slot-name)
  #f)

(define-syntax slot-set!
  (syntax-rules ()
    ((_ ?object (quote ?slot-name) ?value)
     (let ((object ?object)
	   (class (class-of ?object)))
       ((slot-setter-ref class (quote ?slot-name)) object ?value)))))

(define-syntax slot-ref
  (syntax-rules ()
    ((_ ?object (quote ?slot-name))
     (let ((object ?object)
	   (class (class-of ?object)))
       ((slot-getter-ref class (quote ?slot-name)) object)))))



;;;; done

)

;;; end of file
