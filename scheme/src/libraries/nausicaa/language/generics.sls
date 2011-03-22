;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: generic functions
;;;Date: Mon Jul  5, 2010
;;;
;;;Abstract
;;;--------
;;;
;;;	The ancestor of this library is ScmObj by Dorai Sitaram.
;;;
;;;What is a method's signature
;;;----------------------------
;;;
;;;	The "signature"  of a  method is a  list of lists,  each sublist
;;;	being  a list of  record type  UIDs.  The  first sublist  is the
;;;	hierarchy  of UIDs of  the first  method's argument,  the second
;;;	sublist  is the  hierarchy  of the  second  argument, etc.   For
;;;	example, a method defined as:
;;;
;;;	   (define-method (doit (a <complex>) (b <string>) (c <char>))
;;;          ---)
;;;
;;;	has the following signature:
;;;
;;;	   ((nausicaa:builtin:<complex> nausicaa:builtin:<number>
;;;          nausicaa:builtin:<builtin> nausicaa:builtin:<top>)
;;;		;first argument
;;;
;;;         (nausicaa:builtin:<string>  nausicaa:builtin:<builtin>
;;;          nausicaa:builtin:<top>)
;;;		;second argument
;;;
;;;         (nausicaa:builtin:<char>    nausicaa:builtin:<builtin>
;;;          nausicaa:builtin:<top>))
;;;		;third argument
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 1996 Dorai Sitaram
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


#!r6rs
(library (nausicaa language generics)
  (export
    define-generic		define-generic*
    define-generic/merge	define-generic*/merge
    define-method		add-method
    call-next-method		next-method?
    (rename (:uid-list-of uid-list-of:))
    :primary :before :after :around)
  (import (rnrs)
    ;;See the source file for the customisable interface to types.
    (prefix (nausicaa language generics types) type.)
    (nausicaa language parameters)
    (only (nausicaa language extensions)
	  define-auxiliary-syntaxes
	  define-syntax*
	  define-inline
	  begin0)
    (only (nausicaa language makers)
	  define-maker)
    (only (nausicaa symbols-tree)
	  tree-cons treeq)
    (for (prefix (only (nausicaa language syntax-utilities)
		       identifier-suffix)
		 sx.)
	 expand)
    (only (nausicaa language auxiliary-syntaxes)
	  :uid-list-of :primary :before :after :around)
    (rnrs mutable-pairs (6)))


;;;; helpers

(define-syntax list-copy
  (syntax-rules ()
    ((_ ?ell)
     (let loop ((ell ?ell))
       (if (pair? ell)
	   (cons (car ell) (loop (cdr ell)))
	 ell)))))

(define-auxiliary-syntaxes
  :number-of-arguments

  :primary-methods-alist
  :before-methods-alist
  :after-methods-alist
  :around-methods-alist

  :primary-method-add
  :before-method-add
  :after-method-add
  :around-method-add)

(define-syntax* (%define-methods-table stx)
  (syntax-case stx ()
    ((_ ?generic-function ?id ?init)
     (with-syntax ((ALIST	(sx.identifier-suffix #'?id "-methods-alist"))
		   (STORE	(sx.identifier-suffix #'?id "-cache-store"))
		   (REF		(sx.identifier-suffix #'?id "-cache-ref"))
		   (ADD		(sx.identifier-suffix #'?id "-method-add"))
		   (NUMARGS	(datum->syntax #'?id 'number-of-arguments)))
       #'(begin
	   (define ALIST ?init)
	   (define cache '()) ;symbols tree
	   (define (STORE signature methods)
	     (set! cache (tree-cons signature methods cache)))
	   (define (REF signature)
	     (treeq cache signature #f))
	   (define (ADD signature closure)
	     (let ((len (length signature)))
	       (if NUMARGS
		   (%wrong-num-args-in-method-definition '?generic-function signature len NUMARGS)
		 (set! NUMARGS len)))
	     (set! cache '())
	     (set! ALIST (%add-method-to-methods-alist ALIST signature closure)))
	   )))))

(define (%wrong-num-args-in-method-definition who signature mt-number-of-arguments gf-number-of-arguments)
  ;;Called when  adding a method to  a generic function  to validate the
  ;;number of  method arguments against  the number of  generic function
  ;;arguments.
  ;;
  (unless (= gf-number-of-arguments mt-number-of-arguments)
    (syntax-violation who
      (string-append "attempt to define method with wrong number of arguments, expected "
		     (number->string gf-number-of-arguments) " got "
		     (number->string mt-number-of-arguments))
      signature)))


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


;;;; ordinary generic functions definition

(define-maker (define-generic name)
  %define-generic ((:uid-list-of		type.uid-list-of)
		   (:primary-methods-alist	'())))

(define-syntax* (%define-generic stx)
  (syntax-case stx ()
    ((_ ?name . ?rest)
     (not (identifier? #'?name))
     (synner "expected identifier as generic function name" #'?name))

    ((_ ?name ?uid-list-of ?methods-alist)
     #'(begin
	 (%define-methods-table ?name the ?methods-alist)
	 (define number-of-arguments
	   (if (null? the-methods-alist)
	       #f
	     (length (caar the-methods-alist))))
	 (define (implementation . arguments)
	   (generic-function-implementation '?name the-methods-alist the-cache-ref the-cache-store
					    ?uid-list-of number-of-arguments arguments))
	 (define-syntax ?name
	   (lambda (stx)
	     (define-syntax synner
	       (syntax-rules ()
		 ((_ message)
		  (synner message #f))
		 ((_ message subform)
		  (syntax-violation '?name message (syntax->datum stx) (syntax->datum subform)))))
	     (syntax-case stx (:primary-method-add :primary-methods-alist :number-of-arguments)
	       ((_ :primary-method-add signature closure)
		#'(the-method-add signature closure))
	       ((_ :primary-methods-alist)
		#'the-methods-alist)
	       ((_ :number-of-arguments)
		#'number-of-arguments)
	       ((_ key signature closure)
		(and (identifier? #'key)
		     (or (free-identifier=? #'key #':before-method-add)
			 (free-identifier=? #'key #':after-method-add)
			 (free-identifier=? #'key #':around-method-add)))
		(synner "attempt to add method of invalid category \
                         to ordinary generic function" #'key))
	       ((_ key)
		(and (identifier? #'key)
		     (or (free-identifier=? #'key #':before-methods-alist)
			 (free-identifier=? #'key #':after-methods-alist)
			 (free-identifier=? #'key #':around-methods-alist)))
		(synner "attempt to extract method table of invalid category \
                         from ordinary generic function" #'key))
	       ((_ ?arg (... ...))
		#'(implementation ?arg (... ...)))
	       (_
		(synner "invalid arguments to generic function")))))
	 ))
    (_
     (synner "invalid arguments to generic function"))))


(define (generic-function-implementation who methods-alist cache-ref cache-store
					 uid-list-of number-of-arguments arguments)

  (define signature
    (map uid-list-of arguments))

  (define applicable-methods
    (or (cache-ref signature)
	(let ((methods (%compute-applicable-methods signature methods-alist)))
	  (cache-store signature methods)
	  methods)))

  (define method-called? #f)

  (define (is-a-next-method-available?)
    (null? applicable-methods))

  (define (call-methods)
    (cond ((not (null? applicable-methods))
	   (unless method-called?
	     (set! method-called? #t))
	   (let ((method-entry (car applicable-methods)))
	     (set! applicable-methods (cdr applicable-methods))
	     (apply (cdr method-entry) arguments)))
	  (method-called?
	   (assertion-violation who
	     "called next method but no more methods available"))
	  (else
	   (assertion-violation who
	     "no method defined for the argument's types"
	     signature))))

  (unless (= number-of-arguments (length signature))
    (assertion-violation who
      (string-append "wrong number of arguments, expected " (number->string number-of-arguments)
		     " given " (number->string (length signature)))
      arguments))

  (parametrise ((next-method-func-parm call-methods)
		(next-method-pred-parm is-a-next-method-available?))
    (call-methods)))


;;;; starred generic functions definition

(define-maker (define-generic* name)
  %define-generic*
  ((:uid-list-of		type.uid-list-of)
   (:primary-methods-alist	'())
   (:before-methods-alist	'())
   (:after-methods-alist	'())
   (:around-methods-alist	'())))

(define-syntax* (%define-generic* stx)
  (syntax-case stx ()

    ((_ ?name . ?rest)
     (not (identifier? #'?name))
     (synner "expected identifier as generic function name" #'?name))

    ((_ ?name ?uid-list-of
	?primary-methods-alist ?before-methods-alist ?after-methods-alist ?around-methods-alist)
     #'(begin
	 (%define-methods-table ?name primary ?primary-methods-alist)
	 (%define-methods-table ?name before  ?before-methods-alist)
	 (%define-methods-table ?name after   ?after-methods-alist)
	 (%define-methods-table ?name around  ?around-methods-alist)
	 (define number-of-arguments
	   (if (null? primary-methods-alist)
	       #f
	     (length (caar primary-methods-alist))))
	 (define (implementation . arguments)
	   (generic*-function-implementation
	    '?name
	    primary-methods-alist primary-cache-ref primary-cache-store
	    before-methods-alist  before-cache-ref  before-cache-store
	    after-methods-alist   after-cache-ref   after-cache-store
	    around-methods-alist  around-cache-ref  around-cache-store
	    ?uid-list-of number-of-arguments arguments))
	 (define-syntax ?name
	   (lambda (stx)
	     (syntax-case stx (:primary-method-add :primary-methods-alist
						   :after-method-add :after-methods-alist
						   :before-method-add :before-methods-alist
						   :around-method-add :around-methods-alist
						   :number-of-arguments)
	       ((_ :primary-method-add signature closure)
		#'(primary-method-add signature closure))
	       ((_ :after-method-add signature closure)
		#'(after-method-add signature closure))
	       ((_ :before-method-add signature closure)
		#'(before-method-add signature closure))
	       ((_ :around-method-add signature closure)
		#'(around-method-add signature closure))

	       ((_ :primary-methods-alist)	#'primary-methods-alist)
	       ((_ :before-methods-alist)	#'before-methods-alist)
	       ((_ :after-methods-alist)	#'after-methods-alist)
	       ((_ :around-methods-alist)	#'around-methods-alist)

	       ((_ :number-of-arguments)
		#'number-of-arguments)
	       ((_ ?arg (... ...))
		#'(implementation ?arg (... ...)))
	       (_
		(syntax-violation '?name "invalid arguments to generic function" (syntax->datum stx))))))
	 ))
    (_
     (synner "invalid arguments to generic function"))))


(define (generic*-function-implementation who
					  primary-methods-alist primary-cache-ref primary-cache-store
					  before-methods-alist  before-cache-ref  before-cache-store
					  after-methods-alist   after-cache-ref   after-cache-store
					  around-methods-alist  around-cache-ref  around-cache-store
					  uid-list-of number-of-arguments arguments)
  (define signature
    (let ((len (length arguments)))
      (unless (= number-of-arguments len)
	(assertion-violation who
	  (string-append "wrong number of arguments, expected " (number->string number-of-arguments)
			 " given " (number->string len))
	  arguments))
      (map uid-list-of arguments)))
  (define-inline (apply-function ?method)
    (apply ?method arguments))
  (define-inline (consume-method ?method-alist)
    (begin0
	(cdar ?method-alist)
      (set! ?method-alist (cdr ?method-alist))))
  (define-inline (define-applicable-methods NAME ALIST STORE REF)
    (define NAME
      (or (REF signature)
	  (let ((methods (%compute-applicable-methods signature ALIST)))
	    (STORE signature methods)
	    methods))))
  (define-applicable-methods applicable-around-methods
    around-methods-alist around-cache-store around-cache-ref)
  (define-applicable-methods applicable-primary-methods
    primary-methods-alist primary-cache-store primary-cache-ref)
  (define-applicable-methods applicable-before-methods
    before-methods-alist before-cache-store before-cache-ref)
  (define applicable-after-methods
    (or (after-cache-ref signature)
	(let ((methods (reverse	;!!! yes!
			(%compute-applicable-methods signature after-methods-alist))))
	  (after-cache-store signature methods)
	  methods)))

  (define primary-method-called? #f)
  (define reject-recursive-calls? #f)
  (define (is-a-next-method-available?)
    (not (if primary-method-called?
	     (null? applicable-primary-methods)
	   (and (null? applicable-around-methods)
		(null? applicable-primary-methods)))))
  (define (call-methods)
    (cond (reject-recursive-calls?
	   ;;Raise an  error if a  ":before" or ":after"  method invokes
	   ;;the next method.
	   (assertion-violation who ":before and :after methods are forbidden to call the next method"))

	  (primary-method-called?
	   ;;We enter here only if a primary method has been called and,
	   ;;in its body, a call to CALL-NEXT-METHOD is evaluated.
	   (when (null? applicable-primary-methods)
	     (assertion-violation who "called next method but no more :primary methods available"))
	   (apply-function (consume-method applicable-primary-methods)))

	  ((null? applicable-primary-methods)
	   ;;Raise an error if no applicable methods.
	   (assertion-violation who "no method defined for argument classes" signature))

	  ((not (null? applicable-around-methods))
	   ;;If  around  methods exist:  we  apply  them  first.  It  is
	   ;;expected that an  around method invokes CALL-NEXT-METHOD to
	   ;;evaluate the  next around  methods and finally  the primary
	   ;;methods.
	   (apply-function (consume-method applicable-around-methods)))

	  (else
	   ;;Apply  the  methods: before,  primary,  after.  Return  the
	   ;;return value of the primary.
	   (begin ;run before methods
	     (set! reject-recursive-calls? #t)
	     (let loop ((applicable-before-methods applicable-before-methods))
	       (unless (null? applicable-before-methods)
		 (apply-function (cdar applicable-before-methods))
		 (loop (cdr applicable-before-methods))))
	     (set! reject-recursive-calls? #f))
	   (set! primary-method-called? #t)
	   (begin0
	       (apply-function (consume-method applicable-primary-methods))
	     (begin ;run after methods
	       (set! reject-recursive-calls? #t)
	       (let loop ((applicable-after-methods applicable-after-methods))
		 (unless (null? applicable-after-methods)
		   (apply-function (cdar applicable-after-methods))
		   (loop (cdr applicable-after-methods)))))
	     ))))

  (parametrise ((next-method-func-parm call-methods)
		(next-method-pred-parm is-a-next-method-available?))
    (call-methods)))


;;;; syntaxes to define and add methods to generics

(define-syntax* (define-method stx)
  ;;Define  a new  starred  method and  store  it in  the given  starred
  ;;generic function.
  ;;
  (define (main generic-function-id table-key formals-stx body-stx)
    (let loop ((formals		formals-stx)
	       (arg-ids		'())
	       (type-ids	'()))
      (syntax-case formals ()
	(()
	 (with-syntax ((GF		generic-function-id)
		       (TABLE-KEY	table-key)
		       ((ARG ...)	(reverse arg-ids))
		       ((TYPE ...)	(reverse type-ids))
		       (BODY		body-stx))
	   #'(define dummy ;to make it a definition
	       (add-method GF TABLE-KEY (TYPE ...) (type.method-lambda ((ARG TYPE) ...) . BODY)))))
	(((?arg ?type) . ?formals)
	 (loop #'?formals (cons #'?arg arg-ids) (cons #'?type    type-ids)))
	((?arg . ?formals)
	 (loop #'?formals (cons #'?arg arg-ids) (cons #'type.top type-ids)))
	(?stuff
	 (synner "invalid formal arguments in method definition" #'?stuff)))))
  (syntax-case stx (:primary :before :after :around)
    ((_ :primary (?generic-function . ?formals) . ?body)
     (identifier? #'?generic-function)
     (main #'?generic-function #':primary #'?formals #'?body))
    ((_ :primary ?generic-function ?formals . ?body)
     (identifier? #'?generic-function)
     (main #'?generic-function #':primary #'?formals #'?body))
    ((_ ?generic-function :primary ?formals . ?body)
     (identifier? #'?generic-function)
     (main #'?generic-function #':primary #'?formals #'?body))

    ((_ :before (?generic-function . ?formals) . ?body)
     (identifier? #'?generic-function)
     (main #'?generic-function #':before #'?formals #'?body))
    ((_ :before ?generic-function ?formals . ?body)
     (identifier? #'?generic-function)
     (main #'?generic-function #':before #'?formals #'?body))
    ((_ ?generic-function :before ?formals . ?body)
     (identifier? #'?generic-function)
     (main #'?generic-function #':before #'?formals #'?body))

    ((_ :after (?generic-function . ?formals) . ?body)
     (identifier? #'?generic-function)
     (main #'?generic-function #':after #'?formals #'?body))
    ((_ :after ?generic-function ?formals . ?body)
     (identifier? #'?generic-function)
     (main #'?generic-function #':after #'?formals #'?body))
    ((_ ?generic-function :after ?formals . ?body)
     (identifier? #'?generic-function)
     (main #'?generic-function #':after #'?formals #'?body))

    ((_ :around (?generic-function . ?formals) . ?body)
     (identifier? #'?generic-function)
     (main #'?generic-function #':around #'?formals #'?body))
    ((_ :around ?generic-function ?formals . ?body)
     (identifier? #'?generic-function)
     (main #'?generic-function #':around #'?formals #'?body))
    ((_ ?generic-function :around ?formals . ?body)
     (identifier? #'?generic-function)
     (main #'?generic-function #':around #'?formals #'?body))

    ((_ (?generic-function . ?formals) . ?body)
     (identifier? #'?generic-function)
     (main #'?generic-function #':primary #'?formals #'?body))
    ((_ ?generic-function ?formals . ?body)
     (identifier? #'?generic-function)
     (main #'?generic-function #':primary #'?formals #'?body))

    (_
     (synner "invalid syntax in method definition"))))

(define-syntax add-method
  (syntax-rules (:primary :before :after :around)
    ((_ ?generic-function :primary (?type-id ...) ?closure)
     (?generic-function :primary-method-add
			(list (type.uid-list ?type-id) ...) ;this is the signature
			?closure))
    ((_ ?generic-function :before (?type-id ...) ?closure)
     (?generic-function :before-method-add
			(list (type.uid-list ?type-id) ...) ;this is the signature
			?closure))
    ((_ ?generic-function :after (?type-id ...) ?closure)
     (?generic-function :after-method-add
			(list (type.uid-list ?type-id) ...) ;this is the signature
			?closure))
    ((_ ?generic-function :around (?type-id ...) ?closure)
     (?generic-function :around-method-add
			(list (type.uid-list ?type-id) ...) ;this is the signature
			?closure))
    ((_ ?generic-function (?type-id ...) ?closure)
     (?generic-function :primary-method-add
			(list (type.uid-list ?type-id) ...) ;this is the signature
			?closure))
    ))


;;;; method alists
;;
;;The  collection of methods  in a  generic function  is an  alist; each
;;entry has the format:
;;
;;	(signature . closure)
;;
;;the key is the method's signature.
;;

(define (%add-method-to-methods-alist methods-alist signature closure)
  ;;Add a  method's entry to the  alist of methods;  return the modified
  ;;method alist.
  ;;
  ;;A new  method entry  is added  only if no  method with  the selected
  ;;signature already  exists.  If a  method with the  signature already
  ;;exists, its closure is overwritten with the new one.
  ;;
  (cond ((find (lambda (method-entry)
		 (for-all eq? signature (car method-entry)))
	       methods-alist)
	 => (lambda (method-entry)
	      (set-cdr! method-entry closure)
	      methods-alist))
	(else
	 (cons (cons signature closure) methods-alist))))

(define (%compute-applicable-methods call-signature methods-alist)
  ;;Filter out from METHODS-ALIST the  methods not applicable to a tuple
  ;;of arguments with types in  the tuple CALL-SIGNATURE.  Then sort the
  ;;list of applicable  methods so that the more  specific are the first
  ;;ones.  Return the sorted list of applicable method entries.
  ;;
  (list-sort
   (lambda (method-entry1 method-entry2)
     (%more-specific-signature? (car method-entry1) (car method-entry2) call-signature))
   (filter
       (lambda (method-entry)
	 (%applicable-method-signature? call-signature (car method-entry)))
     methods-alist)))

(define (%applicable-method-signature? call-signature method-signature)
  ;;Return true  if a method with  METHOD-SIGNATURE can be  applied to a
  ;;tuple of arguments having CALL-SIGNATURE.
  ;;
  (and (= (length call-signature) (length method-signature))
       (for-all (lambda (maybe-parent maybe-child)
		  (memq (car maybe-parent) maybe-child))
		method-signature call-signature)))

(define (%more-specific-signature? signature1 signature2 call-signature)
  ;;Return true if METHOD1 is more specific than METHOD2 with respect to
  ;;CALL-SIGNATURE.   This  function   must  be  applied  to  applicable
  ;;methods.  The longest signature is more specific, by definition.
  ;;
  (let next-argument-type ((signature1     signature1)
			   (signature2     signature2)
			   (call-signature call-signature))
    (if (null? signature1)

	;;If we are here: the  two signatures have EQ?  car values; this
	;;is an  error because %ADD-METHOD-TO-METHODS-ALIST  should have
	;;detected this and replaced one method's closure with the other
	;;in the alist of methods.
	(assertion-violation '%more-specific-signature?
	  "two methods with same signature in generic function"
	  signature1)

      (let ((uid-hierarchy-1 (car signature1))
	    (uid-hierarchy-2 (car signature2)))
	(cond ((eq? (car uid-hierarchy-1) (car uid-hierarchy-2))
	       (next-argument-type (cdr signature1) (cdr signature2) (cdr call-signature)))
	      ((memq (car uid-hierarchy-2) uid-hierarchy-1)
	       #t)
	      (else
	       #f))))))


;;;; generic functions merging

(define-maker (define-generic/merge name list-of-generic-functions)
  %define-generic/merge
  ((:uid-list-of		type.uid-list-of)
   (:primary-methods-alist	'())))

(define-syntax %define-generic/merge
  (syntax-rules ()
    ((_ ?name (?gf0 ?gf ...) ?uid-list-of ?methods-alist)
     (define-generic ?name
       (:uid-list-of		?uid-list-of)
       (:primary-methods-alist	(%merge-methods-alists (?gf0 :primary-methods-alist)
						       (?gf  :primary-methods-alist)
						       ...
						       ?methods-alist))))))

(define-maker (define-generic*/merge name list-of-generic-functions)
  %define-generic*/merge
  ((:uid-list-of		type.uid-list-of)
   (:primary-methods-alist	'())
   (:before-methods-alist	'())
   (:after-methods-alist	'())
   (:around-methods-alist	'())))

(define-syntax %define-generic*/merge
  (syntax-rules ()
    ((_ ?name (?gf0 ?gf ...) ?uid-list-of
	?primary-methods-alist ?before-methods-alist ?after-methods-alist ?around-methods-alist)
     (define-generic* ?name
       (:uid-list-of	?uid-list-of)
       (:primary-methods-alist	(%merge-methods-alists (?gf0 :primary-methods-alist)
						       (?gf  :primary-methods-alist)
						       ...
						       ?primary-methods-alist))
       (:before-methods-alist	(%merge-methods-alists (?gf0 :before-methods-alist)
						       (?gf  :before-methods-alist)
						       ...
						       ?before-methods-alist))
       (:after-methods-alist	(%merge-methods-alists (?gf0 :after-methods-alist)
						       (?gf  :after-methods-alist)
						       ...
						       ?after-methods-alist))
       (:around-methods-alist	(%merge-methods-alists (?gf0 :around-methods-alist)
						       (?gf  :around-methods-alist)
						       ...
						       ?around-methods-alist))
       ))))

(define (%merge-methods-alists methods-alist . list-of-methods-alists)
  (define-inline (main)
    (let loop ((methods-alist		(list-copy methods-alist))
	       (list-of-methods-alists	list-of-methods-alists))
      (if (null? list-of-methods-alists)
	  methods-alist
	(loop (merge-two-methods-alists methods-alist (car list-of-methods-alists))
	      (cdr list-of-methods-alists)))))

  (define-syntax merge-two-methods-alists
    ;;Merge ?METHODS-ALIST-1 into ?METHODS-ALIST-2 and return a new alist.
    ;;
    (syntax-rules ()
      ((_ ?methods-alist-1 ?methods-alist-2)
       (let loop ((methods-alist-1 ?methods-alist-1)
		  (methods-alist-2 ?methods-alist-2))
	 (if (null? methods-alist-2)
	     methods-alist-1
	   (loop (maybe-merge-method (car methods-alist-2) methods-alist-1) (cdr methods-alist-2)))))))

  (define-syntax maybe-merge-method
    ;;If  a method with  the same  signature does  not already  exist in
    ;;?METHODS-ALIST:  prepend  a  copy  of  ?CANDIDATE-METHOD-ENTRY  to
    ;;?METHODS-ALIST and return the new alist.
    ;;
    ;;?CANDIDATE-METHOD-ENTRY  is  copied because  the  original can  be
    ;;modified by ADD-METHOD.
    ;;
    (syntax-rules ()
      ((_ ?candidate-method-entry ?methods-alist)
       (let* ((candidate-method-entry	?candidate-method-entry)
	      (methods-alist		?methods-alist)
	      (signature			(car candidate-method-entry)))
	 (unless (find (lambda (method-entry)
			 (for-all eq? signature (car method-entry)))
		   methods-alist)
	   `((,(car candidate-method-entry) . ,(cdr candidate-method-entry))
	     . ,methods-alist))))))

  (main))


;;;; done

)

;;; end of file
