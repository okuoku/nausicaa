;;;
;;;Part of: Nausicaa-ScmObj
;;;Contents: object system for Scheme
;;;Date: Tue Nov 11, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 1996 Dorai Sitaram
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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
    (export define-class make-class make <class>
	    class-of subclass?
	    class-definition-name class-precedence-list
	    slot-ref slot-set! list-of-slots
	    make-generic-function
	    define-generic define-method)
    (import ;;(rnrs)
	    (ikarus);;GOTCHA, temporarily to have PRETTY-PRINT
	    (rnrs mutable-pairs (6))
	    (except (srfi lists) delete-duplicates cons))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Helper functions.
;;; ------------------------------------------------------------

(define mapcan
  (lambda (f l . ll)
    ;;Maps f columnwise across the l's, returning
    ;;the spliced list of the result.
    (let loop ((l l) (ll ll))
      (if (null? l) '()
	(append! (apply f (car l) (map car ll))
		 (loop (cdr l) (map cdr ll)))))))

(define delete-duplicates
  (lambda (l)
    ;if l contains multiple copies of any elt,
    ;then all but the last copy are discarded
    (let loop ((l l))
      (if (pair? l)
          (let ((d (loop (cdr l))))
            (if (memv (car l) d)
                (begin (set-car! l (car d))
                       (set-cdr! l (cdr d))))))
      l)))

(define (nconc . list-of-lists)
  (concatenate list-of-lists))

(define (position element the-list)
  (list-index (lambda (elm) (eq? element elm)) the-list))

(define (sort! the-list <)
  (list-sort < the-list))

(define (some pred the-list)
  (let loop ((value (car the-list))
	     (ell   (cdr the-list)))
    (or value (if (null? ell)
		  #f
		(some (car ell) (cdr ell))))))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Access to slots and inspection.
;;; ------------------------------------------------------------

(define slot-ref
  (lambda (al f)
    (let ((c (assq f al)))
      (if c (cdr c)
        (assertion-violation 'slot-ref
			     "trying to access nonexistent slot" f)))))

(define slot-set!
  (lambda (al slot-name value)
    (let ((pair (assq slot-name al)))
;;;GOTCHA
;;      (write 'there)
;;      (pretty-print pair)
      (if pair
	  (set-cdr! pair value)
        (assertion-violation 'slot-set!
			     "trying to set nonexistent slot" slot-name)))))

;;; ------------------------------------------------------------

(define (class-definition-name class)
  (slot-ref class ':class-definition-name))

(define (class-precedence-list class)
  (cons class (slot-ref class ':class-precedence-list)))

(define (list-of-slots object)
  (if (eq? object <class>)
      (cons ':class (slot-ref object ':slots))
    (filter (lambda (name)
	      (not (memq name
			 '(:class-definition-name
			   :class-precedence-list :slots))))
	    (slot-ref object ':slots))))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Class and instance constructors.
;;; ------------------------------------------------------------

(define <class>
  ;;All classes are instances of <class>. 
  '((:class . look-at-the-end-of-the-file-for-the-initialisation-of-this-slot)
    (:class-definition-name . <class>)
    (:class-precedence-list . ())
    (:slots . (:class-definition-name :class-precedence-list :slots))))

(define make
  (lambda (c . svsv)
    (let ((i (cons (cons ':class c)
	       (map (lambda (x)
		      (cons x ':uninitialized))
		 (slot-ref c ':slots)))))
;;;GOTCHA
;;        (write i)
;;        (write svsv)
;;       (exit)
      (let loop ((svsv svsv))
	(if (null? svsv) i
	  (begin
	    (slot-set! i (car svsv) (cadr svsv))
	    (loop (cddr svsv))))))))

(define-syntax make-class
  (syntax-rules ()
    ((make-class () (?slot ...))
     (make-class (<class>) (?slot ...)))
    ((make-class (?superclass ...) (?slot ...))
     (let ((%superclasses (list ?superclass ...)))
       (make <class>
         ':class-precedence-list
	 (delete-duplicates
	  (mapcan
	   (lambda (s)
	     (cons s
		   (append (slot-ref s ':class-precedence-list) '())))
	   %superclasses))
         ':slots
	 (delete-duplicates
	  (nconc '(?slot ...)
		 (mapcan
		  (lambda (s)
		    (append (slot-ref s ':slots) '()))
		  %superclasses))))))))

(define-syntax define-class
  (syntax-rules ()
    ((_ ?name ?superclasses ?slot0 ?slot ...)
     (define ?name
       (let ((c (make-class ?superclasses (?slot0 ?slot ...))))
	 (slot-set! c ':class-definition-name (quote ?name))
	 c)))))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Class inspection.
;;; ------------------------------------------------------------

(define subclass?
  (lambda (c1 c2)
    (cond ((eq? c1 c2) #t)
          ((eq? c1 #t) #f)
          ((eq? c2 #t) #t)
          ((memq c2 (slot-ref c1 ':class-precedence-list)) #t)
          (else #f))))

(define class-of
  (lambda (o)
    (if (pair? o)
      (let ((a (car o)))
        (if (pair? a)
          (if (eq? (car a) ':class) (cdr a) #t)
          #t))
      #t)))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Methods dispatching.
;;; ------------------------------------------------------------

(define (more-specific-method m1 m2 cc)
  (let loop ((cc1 (car m1))
	     (cc2 (car m2))
	     (cc cc))
    (if (null? cc)
	(assertion-violation 'more-specific-method
			     "unknown error comparing methods")
      (let ((c1 (car cc1))
	    (c2 (car cc2)))
	(cond ((eq? c1 c2)
	       (loop (cdr cc1) (cdr cc2) (cdr cc)))
	      ((subclass? c1 c2) #t)
	      ((subclass? c2 c1) #f)
	      (else
	       (let ((c (car cc)))
		 (let ((cpl (if (eq? c #t) '()
			      (slot-ref c ':class-precedence-list))))
		   (let ((i1 (position c1 cpl))
			 (i2 (position c2 cpl)))
		     (if (and i1 i2)
			 (< i1 i2)
		       (assertion-violation 'more-specific-method
					    "unknown error comparing methods")))))))))))

(define (compute-applicable-methods list-of-classes method-table)
  (let loop ((methods method-table)
	     (the-applicable-methods '()))
    (if (null? methods)
        (map cdr
          (sort! the-applicable-methods
		 (lambda (m1 m2)
		   (more-specific-method m1 m2 list-of-classes))))
      (loop (cdr methods)
	    (let ((method (car methods)))
	      (if (every subclass? list-of-classes (car method))
		  (cons method the-applicable-methods)
		the-applicable-methods))))))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Generic functions.
;;; ------------------------------------------------------------

;;;A  'generic function'  is  basically a  couple  of values:  an
;;;interface procedure and an object of class <generic>.
;;;
;;;The interface procedure  is stored in the ":interface-procedure"
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

;;;Helper function  that adds a signature/method  pointed list to
;;;the appropriate alist of methods (the METHOD-TABLE argument).
(define (add-method-to-generic-function method-table specializer-classes m)
  (or (some (lambda (c)
	      (and (every eq? specializer-classes (car c))
		   (begin
		     (set-cdr! c m)
		     #t)))
	    method-table)
      (set! method-table (alist-cons specializer-classes m method-table)))
  #t)

;;;Helper syntax  for the definition  of the closure that  adds a
;;;method to the appropriate method table.
(define-syntax method-adder
  (syntax-rules ()
    ((_ ?method-table)
     (lambda (specializer-classes m)
       (add-method-to-generic-function ?method-table specializer-classes m)))))

(define-syntax create-generic-procedure
  (syntax-rules ()
    ((create-generic-procedure ?arg-name ...)
     (let ((%primary-method-table '())
	   (%before-method-table '())
	   (%after-method-table '())
	   (%around-method-table '()))
       (make <generic>
	 ':add-primary-method (method-adder %primary-method-table)
	 ':add-before-method  (method-adder %before-method-table)
	 ':add-after-method   (method-adder %after-method-table)
	 ':add-around-method  (method-adder %around-method-table)
	 ':interface-procedure
	 (lambda (?arg-name ... . %rest)
	   (let ((%arg-classes (list (class-of ?arg-name) ...)))
	     (let ((%applicable-primary-methods
		    (compute-applicable-methods %arg-classes %primary-method-table))
		   (%applicable-before-methods
		    (compute-applicable-methods %arg-classes %before-method-table))
		   (%applicable-after-methods
		    (reverse
		     (compute-applicable-methods %arg-classes %after-method-table)))
		   (%applicable-around-methods
		    (compute-applicable-methods %arg-classes %around-method-table))
		   (%primary-method-called #f))
	       (letrec
		   ((%next-method?
		     (lambda ()
		       (if %primary-method-called
			   (not (null? %applicable-primary-methods))
			 (or (not (null? %applicable-around-methods))
			     (not (null? %applicable-primary-methods))))))
		    (%call-next-method
		     (lambda ()
		       (cond (%primary-method-called
			      (apply
			       (let ((m (car %applicable-primary-methods)))
				 (set! %applicable-primary-methods
				       (cdr %applicable-primary-methods))
				 m)
			       %next-method?
			       %call-next-method
			       ?arg-name ... %rest))
			     ((not (null? %applicable-around-methods))
			      (apply
			       (let ((m (car %applicable-around-methods)))
				 (set! %applicable-around-methods
				       (cdr %applicable-around-methods))
				 m)
			       %next-method?
			       %call-next-method
			       ?arg-name ... %rest))
			     ((null? %applicable-primary-methods)
			      (assertion-violation 'create-generic-procedure
						   "no method defined for these argument classes"))
			     (else
			      (set! %primary-method-called #t)
			      (for-each
				  (lambda (%before-method)
				    (apply %before-method ?arg-name ... %rest))
				%applicable-before-methods)
			      (let ((%res
				     (apply
				      (let
					  ((m (car
					       %applicable-primary-methods)))
					(set! %applicable-primary-methods
					      (cdr %applicable-primary-methods))
					m)
				      %next-method?
				      %call-next-method
				      ?arg-name ... %rest)))
				(for-each
				    (lambda (%after-method)
				      (apply %after-method ?arg-name ... %rest))
				  %applicable-after-methods)
				%res))))))
		 (%call-next-method))))))))))

;;; ------------------------------------------------------------

;;;GOTCHA, service function for testing
(define (for-test--alist? what ell)
  (printf "*** Validating ~a as alist:\n" what)
;  (pretty-print ell)
  (write ell)(newline)
  (let ((r (list? ell)))
    (printf "  * Is it a list? ~s\n" r)
    (when r
      (let ((r (proper-list? ell)))
	(printf "  * Is it a proper list? ~s\n" r)
	(when r
	  (let ((r (map (lambda (l)
			  (and (pair? l)
			       (dotted-list? l))) ell)))
	    (printf "  * Are the elements pairs and dotted-lists? ~s\n" r))
	  )))))

;;;Helper  function  that adds  a  new  generic  function to  the
;;;*GENERIC-PROCEDURES* alist.  This  function is not expanded in
;;;the MAKE-GENERIC-FUNCTION  (as it was in  the original ScmObj
;;;code) because  doing so would modify a  variable exported from
;;;this library (and this is forbidden by R6RS).
(define (register-new-generic-function interface-procedure generic-object)
;;;GOTCHA
;;  (pretty-print *generic-procedures*)
  (set! *generic-procedures*
	(alist-cons interface-procedure generic-object *generic-procedures*))
  (display "-- The alist of generic procedures is:\n")
  (pretty-print *generic-procedures*)
  (for-test--alist? "list of generic procedures" *generic-procedures*)
  (for-test--alist? "first method class" (cdar *generic-procedures*))
  )

(define-syntax make-generic-function
  (syntax-rules ()
    ((make-generic-function ?arg ...)
     (let* ((generic-object      (create-generic-procedure ?arg ...))
            (interface-procedure (slot-ref generic-object ':interface-procedure)))
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

(define (add-before-method-to-generic-function generic-function signature func)
  ((slot-ref (cdr (assq generic-function *generic-procedures*))
	     ':add-before-method)
   signature func))

(define (add-after-method-to-generic-function generic-function signature func)
  ((slot-ref (cdr (assq generic-function *generic-procedures*))
	     ':add-after-method)
   signature func))

(define (add-primary-method-to-generic-function generic-function signature func)
;;;GOTCHA - hurt me!
;;;  (pretty-print generic-function)
;;;  (display "-- The method element in the alist of generic procedures is:\n")
;;;  (pretty-print (assq generic-function *generic-procedures*))
  ((slot-ref (cdr (assq generic-function *generic-procedures*))
	     ':add-primary-method)
   signature func)
  )

(define (add-around-method-to-generic-function generic-function signature func)
  ((slot-ref (cdr (assq generic-function *generic-procedures*))
	     ':add-around-method)
   signature func))

;;; ------------------------------------------------------------

(define-syntax define-method
  (syntax-rules (:primary :before :after :around)

    ((_ 1 ?generic-function ?qualifier ?args . ?body)
     (define-method 2 ?generic-function ?qualifier ?args
       ()  ;;specialising args
       ()  ;;non-specialising args
       . ?body))

					;Matches  the  form  when
					;the next  argument to be
					;processed has a class.
    ((_ 2 ?generic-function ?qualifier ((?arg ?class) . ?args) (?sa ...) () . ?body)
     (define-method 2 ?generic-function ?qualifier ?args (?sa ... (?arg ?class)) ()  . ?body))

					;Matches  the  form  when
					;the next  argument to be
					;processed has no class.
    ((_ 2 ?generic-function ?qualifier (?arg . ?args) ?sargs (?nsa ...) . ?body)
     (define-method 2 ?generic-function ?qualifier ?args ?sargs (?nsa ... ?arg)  . ?body))

					;Matches  the  form  when
					;all  the  arguments have
					;been processed.
    ((_ 2 ?generic-function ?qualifier () ?sargs ?nsargs . ?body)
     (define-method 3 ?generic-function ?qualifier ?sargs ?nsargs . ?body))

					;Matches  the  form  when
					;all  the  arguments have
					;been  processed and only
					;the   rest  argument  is
					;there.
    ((_ 2 ?generic-function ?qualifier ?rest ?sargs (?nsa ...) . ?body)
     (define-method 3 ?generic-function ?qualifier ?sargs (?nsa ... . ?rest) . ?body))

    ;;Originally it was:
    ;;
    ;; ((define-method 3 generic-function :primary ((x c) ...) nsargs . body)
    ;;  ((slot-ref (cdr (assq generic-function *generic-procedures*))
    ;;             ':add-primary-method-to-generic-function)
    ;;   (list c ...)
    ;;   (lambda (next-method? call-next-method x ... . nsargs)
    ;;     . body)))
    ((_ 3 ?generic-function :primary ((?arg ?class) ...) ?nsargs . ?body)
     (add-primary-method-to-generic-function
      ?generic-function (list ?class ...)
      (lambda (next-method? call-next-method ?arg ... . ?nsargs) . ?body)))

    ;;Originally it was:
    ;;
    ;; ((define-method 3 generic-function :before ((x c) ...) nsargs . body)
    ;;  ((slot-ref (cdr (assq generic-function *generic-procedures*))
    ;;             ':add-before-method-to-generic-function)
    ;;   (list c ...)
    ;;   (lambda (x ... . nsargs)
    ;;     . body)))
    ((_ 3 ?generic-function :before ((?arg ?class) ...) ?nsargs . ?body)
     (add-before-method-to-generic-function
      ?generic-function (list ?class ...)
      (lambda (?arg ... . ?nsargs) . ?body)))

    ;;Originally it was:
    ;;
    ;; ((define-method 3 generic-function :after ((x c) ...) nsargs . body)
    ;;  ((slot-ref (cdr (assq generic-function *generic-procedures*))
    ;;             ':add-after-method-to-generic-function)
    ;;   (list c ...)
    ;;   (lambda (x ... . nsargs)
    ;;     . body)))
    ((_ 3 ?generic-function :after ((?arg ?class) ...) ?nsargs . ?body)
     (add-after-method-to-generic-function
      ?generic-function (list ?class ...)
      (lambda (?arg ... . ?nsargs) . ?body)))

    ;;Originally it was:
    ;;
    ;; ((define-method 3 generic-function :around ((x c) ...) nsargs . body)
    ;;  ((slot-ref (cdr (assq generic-function *generic-procedures*))
    ;;             ':add-around-method-to-generic-function)
    ;;   (list c ...)
    ;;   (lambda (next-method? call-next-method x ... . nsargs)
    ;;     . body)))
    ((_ 3 ?generic-function :around ((?arg ?class) ...) ?nsargs . ?body)
     (add-around-method-to-generic-function
      (list ?class ...)
      (lambda (next-method? call-next-method ?arg ... . ?nsargs) . ?body)))

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

(slot-set! <class> ':class <class>)

) ;; end of library form

;;; end of file
