;;;
;;;Part of: Nausicaa-ScmObj
;;;Contents: object system for Scheme
;;;Date: Tue Nov 11, 2008
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
;;;This is  free software; you can redistribute  it and/or modify
;;;it under the terms of the GNU Lesser General Public License as
;;;published by the Free  Software Foundation; either version 2.1
;;;of the License, or (at your option) any later version.
;;;
;;;This  library is  distributed  in  the hope  that  it will  be
;;;useful,  but WITHOUT  ANY WARRANTY;  without even  the implied
;;;warranty  of  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR
;;;PURPOSE.  See  the GNU Lesser General Public  License for more
;;;details.
;;;
;;;You  should have  received a  copy of  the GNU  Lesser General
;;;Public License along  with this library; if not,  write to the
;;;Free Software  Foundation, Inc.,  59 Temple Place,  Suite 330,
;;;Boston, MA 02111-1307 USA.
;;;


;;;page
;;; ------------------------------------------------------------
;;; Setup.
;;; ------------------------------------------------------------

(library (scmobj)
  (export
      ;;Built in classes.
      <class> <entity-class>

    <circular-list> <dotted-list> <proper-list> <list> <pair>
    <vector> <hashtable> <record>
    <input-port> <output-port> <port>
    <condition> <bytevector>
    <fixnum> <flonum> <integer> <integer-valued>
    <rational> <rational-valued> <real> <real-valued>
    <complex> <number>

    ;; Constructors.
    define-class define-generic define-method
    make-class make make-generic-function

    ;;Class inspection.
    class-of subclass?
    class-definition-name class-precedence-list
    list-of-instance-slots list-of-slots

    ;;Slot accessors.
    slot-ref slot-set! 

    ;;Next method interface.
    call-next-method next-method?
    scmobj:the-next-method-func scmobj:the-next-method-pred)
  (import (rnrs)
	  (rnrs mutable-pairs (6))
	  (except (srfi lists))
	  (srfi parameters))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Helper functions and syntaxes.
;;; ------------------------------------------------------------

(define mapcan
  (lambda (f l . ll)
    ;;Maps f columnwise across the l's, returning
    ;;the spliced list of the result.
    (let loop ((l l) (ll ll))
      (if (null? l) '()
	(append! (apply f (car l) (map car ll))
		 (loop (cdr l) (map cdr ll)))))))

(define-syntax position
  (syntax-rules ()
    ((_ ?element ?list)
     (list-index
	 (lambda (elm)
	   (eq? ?element elm))
       ?list))))

(define-syntax sort
  (syntax-rules ()
    ((_ ?list ?less-pred)
     (list-sort ?less-pred ?list))))

;;;This is the K combinator.
(define-syntax begin0
  (syntax-rules ()
    [(_ ?expr1 ?expr2 ...)
     (call-with-values
	 (lambda () ?expr1)
       (lambda x
	 ?expr2 ...
	 (apply values x)))]))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Access to slots.
;;; ------------------------------------------------------------

(define (get-slot caller object slot-name)
  (or (assq slot-name object)
      (assertion-violation caller
       "trying to access nonexistent slot" slot-name)))

(define (slot-ref object slot-name)
  (cdr (get-slot 'slot-ref object slot-name)))

(define (slot-set! object slot-name value)
  (set-cdr! (get-slot 'slot-set! object slot-name) value))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Class inspection functions.
;;; ------------------------------------------------------------

(define (class-definition-name class)
  (slot-ref class ':class-definition-name))

(define (class-precedence-list class)
  (slot-ref class ':class-precedence-list))

(define (list-of-slots object)
  (cons ':class (slot-ref object ':slots)))

(define (list-of-instance-slots object)
  (filter
      (lambda (c)
	(not (memq c '(:class-definition-name
		       :class-precedence-list
		       :slots))))
    (slot-ref object ':slots)))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Built in classes.
;;; ------------------------------------------------------------

;;;The  official definition  of class  is: an  alist  whose first
;;;dotted pair has the symbol ":class" as key.
;;;
;;;Notice that ":class" is NOT included in the list of slots.
;;;
;;;Notice  that the  class itself  is  not inclued  in the  class
;;;precedence list.
(define <class>
  (let ((c '((:class . #f)
	     (:class-definition-name . <class>)
	     (:class-precedence-list . ())
	     (:slots . (:class-definition-name
			:class-precedence-list
			:slots)))))
    (set-cdr! (car c) c)
    c))

(define <entity-class>
  (let ((c (alist-copy <class>)))
    (slot-set! c ':class-definition-name '<entity-class>)
    (slot-set! c ':class-precedence-list <class>)
    c))

;;; ------------------------------------------------------------

(define-syntax define-entity-class
  (syntax-rules ()
    ((_ ?name)
     (define-entity-class ?name (<entity-class> <class>)))
    ((_ ?name (?superclass ...))
     (define ?name
       (list (cons ':class <entity-class>)
	     '(:class-definition-name . ?name)
	     (list ':class-precedence-list .
		   (list ?superclass ... <entity-class> <class>))
	     '(:slots . (:class-definition-name
			 :class-precedence-list
			 :slots)))))))

(define-entity-class <pair>)
(define-entity-class <list>		(<pair>))
(define-entity-class <circular-list>	(<list> <pair>))
(define-entity-class <dotted-list>	(<list> <pair>))
(define-entity-class <proper-list>	(<list> <pair>))
(define-entity-class <vector>)
(define-entity-class <hashtable>)

;;There exist TEXTUAL-PORT? and BINARY-PORT? but these attributes
;;are not mutually exclusive with input and output port.  So they
;;cannot  be  made classes  without  adding  a  predicate in  the
;;definition of the  entity class, and then using  it in CLASS-OF
;;and SUBCLASS?.
(define-entity-class <port>)
(define-entity-class <input-port>	(<port>))
(define-entity-class <output-port>	(<port>))

(define-entity-class <record>)
(define-entity-class <condition>	(<record>))
(define-entity-class <bytevector>)

(define-entity-class <number>)
(define-entity-class <complex>		(<number>))
(define-entity-class <real-valued>	(<complex> <number>))
(define-entity-class <real>
  (<real-valued> <complex> <number>))
(define-entity-class <rational-valued>
  (<real> <real-valued> <complex> <number>))
(define-entity-class <flonum>
  (<real> <real-valued> <complex> <number>))
(define-entity-class <rational>
  (<rational-valued> <real> <real-valued> <complex> <number>))
(define-entity-class <integer-valued>
  (<rational-valued> <real> <real-valued> <complex> <number>))
(define-entity-class <integer>
  (<integer-valued> <rational> <rational-valued> <real> <real-valued> <complex> <number>))
(define-entity-class <fixnum>
  (<integer> <integer-valued> <rational> <rational-valued> <real> <real-valued> <complex> <number>))

;;;Other possible classes that require more library loading:
;;;
;;;	<stream>	stream?
;;;

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Class and instance constructors.
;;; ------------------------------------------------------------

;;;This is a "standard" make function, in style with CLOS.
;;;
(define (make class . init-args)
  (let ((instance (allocate-instance class)))
    (initialise instance init-args)
    instance))

;;;Build a new alist initialising all the slots, but ":class", to
;;;":uninitialized".   The  ":class" pair  has  to  be the  first
;;;element in the alist.
;;;
;;;The  form of  this  function is  one  of the  reasons why  the
;;;":class" slot is not in the list of slots.
;;;
(define (allocate-instance class)
  (cons (cons ':class class)
	(map (lambda (x)
	       (cons x ':uninitialized))
	  (slot-ref class ':slots))))

;;;Interpret SLOT-VALUES as list of alternate symbols and values,
;;;where the symbols are slot names.
;;;
;;;We are not asserting  (as we should) that: "(car slot-values)"
;;;is not  ":class"; SLOT-VALUES has an even  number of elements.
;;;Because of this errors with unclear message may happen.
;;;
(define (initialise instance slot-values)
  (unless (null? slot-values)
    (slot-set! instance (car slot-values) (cadr slot-values))
    (initialise instance (cddr slot-values))))

;;;It is  possible for a class  to add no new  slots: this allows
;;;subclassing for the only purpose of method dispatching.
;;;
;;;Notice that the class precedence list does not include the new
;;;class itself.
;;;
(define-syntax make-class
  (syntax-rules ()
    ((make-class () (?slot ...))
     (make-class (<class>) (?slot ...)))
    ((make-class (?superclass ...) (?slot ...))
     (let ((superclasses (list ?superclass ...)))
       (make <class>
         ':class-precedence-list (build-class-precedence-list superclasses)
         ':slots (build-slot-list '(?slot ...) superclasses))))))

;;;Example: let's say that the super classes are:
;;;
;;;	(<a0>
;;;	 <b0>
;;;	 <c0>)
;;;
;;;we take the list of their superclasses:
;;;
;;;	((<a0> <a1> <a2>)
;;;	 (<b0> <b1>)
;;;	 (<c0> <c1> <c2>))
;;;
;;;and compose them to have:
;;;
;;;	(<a0> <b0> <c0>  <a1> <b1> <c1>  <a2> <c2>)
;;;
;;;at the end we remove the duplicates.
;;;
;;;Remember  that a  list of  superclasses for  a class  does not
;;;include the class itself.
(define (build-class-precedence-list superclasses)
  (delete-duplicates
   (mapcan
    (lambda (s)
      (cons s
	    (append (slot-ref s ':class-precedence-list) '())))
    superclasses)
   eq?))

;;;Put all the slot names into a single list.
;;;
;;;The  form  of this  function  is one  of  the  reasons why  the
;;;":class" slot is not in the list of slots.
;;;
(define (build-slot-list direct-slots superclasses)
  (delete-duplicates
   (append direct-slots
	   (concatenate
	    (map (lambda (s)
		   ;;FIXME This was in the original code:
		   ;;
		   ;;(append (slot-ref s ':slots) '())
		   ;;
		   ;;I  cannot  figure  out  why:  the  value  of
		   ;;":slots" is always a proper list.
		   (slot-ref s ':slots))
	      superclasses)))
   eq?))

;;;Define a binding for a class, giving it a name.
;;;
(define-syntax define-class
  (syntax-rules ()
    ((_ ?name ?superclasses (?slot ...))
     (define-class ?name ?superclasses ?slot ...))
    ((_ ?name ?superclasses ?slot ...)
     (define ?name
       (let ((c (make-class ?superclasses (?slot ...))))
	 (slot-set! c ':class-definition-name (quote ?name))
	 c)))))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Class inspection.
;;; ------------------------------------------------------------

(define (subclass? c1 c2)
  (cond ((eq? c1 c2) #t)
	((eq? c1 #t) #f)
	((eq? c2 #t) #t)
	((memq c2 (slot-ref c1 ':class-precedence-list)) #t)
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
   ((integer-valued? value)	<integer-valued>)
   ((rational?	value)		<rational>)
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


;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Methods dispatching.
;;; ------------------------------------------------------------

(define (more-specific-method
	 signature.function-1 signature.function-2 signature)
  (let loop ((signature1 (car signature.function-1))
	     (signature2 (car signature.function-2))
	     (signature signature))
    (if (null? signature)
	(assertion-violation
	    'more-specific-method
	  "unknown error comparing methods")
      (let ((class1 (car signature1))
	    (class2 (car signature2)))
	(cond
	 ((eq? class1 class2)
	  (loop (cdr signature1) (cdr signature2) (cdr signature)))
	 ((subclass? class1 class2) #t)
	 ((subclass? class2 class1) #f)
	 (else
	  (let ((c (car signature)))
	    (let ((cpl (if (eq? c #t)
			   '()
			 (slot-ref c ':class-precedence-list))))
	      (let ((i1 (position class1 cpl))
		    (i2 (position class2 cpl)))
		(if (and i1 i2)
		    (< i1 i2)
		  (assertion-violation
		      'more-specific-method
		    "unknown error comparing methods")))))))))))

(define (compute-applicable-methods signature method-table)
  (let loop ((methods method-table)
	     (the-applicable-methods '()))
    (if (null? methods)
        (map cdr
          (sort the-applicable-methods
		(lambda (signature.function-1 signature.function-2)
		  (more-specific-method signature.function-1
					signature.function-2
					signature))))
      (loop (cdr methods)
	    (let ((signature.function (car methods)))
	      (if (every subclass? signature (car signature.function))
		  (cons signature.function the-applicable-methods)
		the-applicable-methods))))))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Next method implementation.
;;; ------------------------------------------------------------

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

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Generic functions.
;;; ------------------------------------------------------------

;;;A  'generic function'  is  basically a  couple  of values:  an
;;;interface procedure and an object of class <generic>.
;;;
;;;The interface procedure  is stored in the :interface-procedure
;;;slot of the  object and is used to  apply the generic function
;;;to a list of arguments.
;;;
(define-class <generic> ()
  :interface-procedure
  :add-primary-method
  :add-before-method
  :add-after-method
  :add-around-method)

;;;This is an alist that will hold all the generic functions ever
;;;created.  The  keys are  the interface procedures,  the values
;;;are the <generic> objects.
(define *generic-procedures* '())

;;;Helper function that adds a signature/func pointed list to the
;;;appropriate alist  of methods (the  METHOD-TABLE argument).  A
;;;new  method is  added only  if  no method  with the  signature
;;;already exists.
(define (add-method-to-method-table
	 method-table method-signature method-func)
  (unless (any
	      (lambda (signature.function)
		;;If a method  with the signature already exists,
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

;;;Helper syntax  for the definition  of the closure that  adds a
;;;method to the appropriate method table.
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

		      ;;We  enter here  only  if a  primary
		      ;;method has been  called and, in its
		      ;;body, a call to CALL-NEXT-METHOD if
		      ;;performed.
		      (primary-method-called
		       (apply-function
			(consume-method applicable-primary-methods)))

		      ;;If around  methods exists: we apply
		      ;;them rather  than the primary ones.
		      ;;It  is   expected  that  an  around
		      ;;method invokes CALL-NEXT-METHOD.
		      ((not (null? applicable-around-methods))
		       (apply-function
			(consume-method applicable-around-methods)))

		      ;;Raise an error if no applicable methods.
		      ((null? applicable-primary-methods)
		       (assertion-violation
			   'next-method-func
			 "no method defined for these argument classes"))

		      ;;Apply the methods: before, primary,
		      ;;after.  Return  the return value of
		      ;;the primary.
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
	       (next-method-func))
	     )))))))

;;; ------------------------------------------------------------

;;;Helper  function  that adds  a  new  generic  function to  the
;;;*GENERIC-PROCEDURES* alist.  This  function is not expanded in
;;;the MAKE-GENERIC-FUNCTION  (as it was in  the original ScmObj
;;;code) because  doing so would modify a  variable exported from
;;;this library (and this is forbidden by R6RS).
(define (register-new-generic-function interface-procedure generic-object)
  (set! *generic-procedures*
	(alist-cons interface-procedure generic-object
		    *generic-procedures*)))

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

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Methods.
;;; ------------------------------------------------------------

;;;What follows is the  documentation of the DEFINE-METHOD syntax
;;;below.  The pattern matching has tree phases:
;;;
;;;1..the  method  is recognised  as  primary,  before, after  or
;;;   around, and two accumulator lists are initialised to nil;
;;;
;;;2..the  method arguments are  accumulated in  a list  of "with
;;;   class" and a list of "without class";
;;;
;;;3..the method  is added to  the appropriate collection  in the
;;;   generic function.
;;;
;;;The ?QUALIFIER pattern variable is one of the literals:
;;;
;;;	:primary :before :after :around
;;;
;;;it defaults to ":primary".
;;;
;;;The  ?SARGS  pattern  variable   is  a  list  accumulator  for
;;;specialising arguments.   A specialising argument  is a method
;;;argument for which a class was specified.
;;;
;;;The  ?NSARGS  pattern  variable  is  a  list  accumulator  for
;;;non-specialising arguments.  A  non-specialising argument is a
;;;method argument for which a class was NOT specified.
;;;
;;;The ?REST  pattern variable  is used to  hold the name  of the
;;;rest argument.
;;;
;;;The  ?ARG  pattern  variable   is  the  next  argument  to  be
;;;accumulated somewhere.
;;;
;;;The ?SA  pattern variable  is a specialising  argument already
;;;accumulated.
;;;
;;;The  ?NSA  pattern  variable  is a  non-specialising  argument
;;;already accumulated.
;;;
;;;Example:
;;;
;;;  (define-method swirl-vector ((vec <vector>) idx . args)
;;;	---)
;;;
;;;here VEC is a specialising  argument of class <vector>, IDX is
;;;a  non-specialising argument,  ARGS is  the name  of  the rest
;;;argument.
;;;
;;;A 'signature'  is a list of  classes: given a  list of generic
;;;function  call  arguments,  the  arguments  must  match  these
;;;classes for the method to be applicable.  Example:
;;;
;;;  (define-method twist-vector ((vec <vector>) (idx <int>))
;;;     ---)
;;;
;;;the signature is: "(list <vector> <int>)".
;;;

(define (add-method-to-generic-function
	 slot-name generic-function method-signature method-func)
  ((slot-ref (cdr (assq generic-function *generic-procedures*))
	     slot-name)
   method-signature method-func))

;;; ------------------------------------------------------------

(define-syntax define-method
  (syntax-rules (:primary :before :after :around)

    ((_ 1 ?generic-function ?qualifier ?args . ?body)
     (define-method 2 ?generic-function ?qualifier ?args
       ()  ;;specialising args
       ()  ;;non-specialising args
       . ?body))

    ;;Matches the form when the next argument to be processed has
    ;;a class.
    ((_ 2 ?generic-function ?qualifier
	((?arg ?class) . ?args) (?sa ...) () . ?body)
     (define-method 2 ?generic-function ?qualifier
       ?args (?sa ... (?arg ?class)) ()  . ?body))

    ;;Matches the form when the next argument to be processed has
    ;;no class.
    ((_ 2 ?generic-function ?qualifier
	(?arg . ?args) ?sargs (?nsa ...) . ?body)
     (define-method 2 ?generic-function ?qualifier
       ?args ?sargs (?nsa ... ?arg)  . ?body))

    ;;Matches  the   form  when  all  the   arguments  have  been
    ;;processed.
    ((_ 2 ?generic-function ?qualifier () ?sargs ?nsargs . ?body)
     (define-method 3 ?generic-function ?qualifier
       ?sargs ?nsargs . ?body))

    ;;Matches the form when all the arguments have been processed
    ;;and only the rest argument is there.
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

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Done.
;;; ------------------------------------------------------------

) ;; end of library form

;;; end of file
