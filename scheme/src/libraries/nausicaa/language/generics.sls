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
    define-method		add-method
    call-next-method		next-method?
    uid-list-of			reverse-before-methods
    merge
    :primary			:around
    :before			:after)
  (import (rnrs)
    ;;See the source file for the customisable interface to types.
    (prefix (nausicaa language generics types) type.)
    (prefix (nausicaa language generics methods-table) mt.)
    (nausicaa language parameters)
    (only (nausicaa language extensions)
	  define-auxiliary-syntaxes
	  define-syntax*
	  define-inline
	  begin0
	  define-for-expansion-evaluation)
    (only (nausicaa language makers)
	  define-maker)
    (for (prefix (only (nausicaa language syntax-utilities)
		       identifier-suffix
		       identifier-memq
		       all-identifiers?
		       unwrap
		       case-identifier)
		 sx.)
	 expand)
    (only (nausicaa language auxiliary-syntaxes)
	  uid-list-of reverse-before-methods merge
	  :primary :before :after :around)
    (prefix (nausicaa language property-keywords) pk.)
    (for (prefix (nausicaa language identifier-properties) ip.) expand)
    (for (prefix (nausicaa language generics properties) prop.) expand))


;;;; helpers

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

(define-maker (define-generic name formals)
  %define-generic
  ((uid-list-of			type.uid-list-of)
   (merge			(list))))

(define-syntax* (%define-generic stx)
  (syntax-case stx (list)
    ((_ ?name . ?rest)
     (not (identifier? #'?name))
     (synner "expected identifier as generic function name" #'?name))

    ((_ ?name (?formal ...) ?uid-list-of (list ?generic ...))
     (sx.all-identifiers? #'(?formal ... ?generic ...))
     (let* ((number-of-arguments	(length (sx.unwrap #'(?formal ...))))
	    (generic-identifiers	(sx.unwrap #'(?generic ...)))
	    (methods-arguments
	     (fold-left
	      (lambda (knil id)
		(let ((prop (ip.ref id #'pk.generic-function)))
		  (unless (= number-of-arguments (prop.generic-number-of-arguments prop))
		    (synner "attempt to merge generic function with wrong number of arguments" id))
		  (append knil (prop.generic-methods-arguments prop))))
	      '()
	      generic-identifiers)))
       (with-syntax ((NUMBER-OF-ARGUMENTS	number-of-arguments)
		     (METHODS-ARGUMENTS		methods-arguments))
	 #'(begin
	     (define number-of-arguments NUMBER-OF-ARGUMENTS)
	     (mt.define-methods-table ?name
				      the-methods-alist the-method-add
				      the-cache-store the-cache-ref
				      (mt.merge-methods-alists (?generic :primary-methods-alist) ...))
	     (define (implementation . arguments)
	       (generic-function-implementation '?name the-methods-alist the-cache-store the-cache-ref
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
			 (sx.identifier-memq #'key (list #':before-method-add
							 #':after-method-add
							 #':around-method-add)))
		    (synner "attempt to add method of invalid category \
                             to ordinary generic function" #'key))
		   ((_ key)
		    (and (identifier? #'key)
			 (sx.identifier-memq #'key (list #':before-methods-alist
							 #':after-methods-alist
							 #':around-methods-alist)))
		    (synner "attempt to extract method table of invalid category \
                             from ordinary generic function" #'key))
   		   ((_ ?arg (... ...))
		    #'(implementation ?arg (... ...)))
		   (_
		    (synner "invalid arguments to generic function")))))
	     ;;Remember that  first we bind ?NAME and  then we associate
	     ;;properties to it.
	     (define-for-expansion-evaluation
	       (ip.define #'?name #'pk.generic-function
			  (prop.make-generic (sx.unwrap #'NUMBER-OF-ARGUMENTS)
					     (sx.unwrap #'METHODS-ARGUMENTS))))
	     ))))

  (_
   (synner "invalid arguments for generic function definition"))))


(define (generic-function-implementation who methods-alist cache-store cache-ref
					 uid-list-of number-of-arguments arguments)

  (define signature
    (let ((len (length arguments)))
      (unless number-of-arguments
	(assertion-violation who "called generic function with no methods"))
      (unless (= number-of-arguments len)
	(assertion-violation who
	  (string-append "wrong number of arguments, expected " (number->string number-of-arguments)
			 " given " (number->string len))
	  arguments))
      (map uid-list-of arguments)))
  (define applicable-methods
    (or (cache-ref signature)
	(let ((methods (mt.compute-applicable-methods signature methods-alist)))
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
  (parametrise ((next-method-func-parm call-methods)
		(next-method-pred-parm is-a-next-method-available?))
    (call-methods)))


;;;; starred generic functions definition

(define-maker (define-generic* name formals)
  %define-generic*
  ((uid-list-of			type.uid-list-of)
   (reverse-before-methods	#f)
   (merge			(list))))

(define-syntax* (%define-generic* stx)
  (syntax-case stx (list)
    ((_ ?name . ?rest)
     (not (identifier? #'?name))
     (synner "expected identifier as generic function name" #'?name))

    ((_ ?name (?formal ...) ?uid-list-of ?reverse-before-methods (list ?generic ...))
     (sx.all-identifiers? #'(?formal ... ?generic ...))
     (let* ((number-of-arguments	(length (sx.unwrap #'(?formal ...))))
	    (generic-identifiers	(sx.unwrap #'(?generic ...)))
	    (methods-arguments
	     (fold-left
	      (lambda (knil id)
		(let ((prop (ip.ref id #'pk.generic-function)))
		  (unless (= number-of-arguments (prop.generic-number-of-arguments prop))
		    (synner "attempt to merge generic function with wrong number of arguments" id))
		  (append knil (prop.generic-methods-arguments prop))))
	      '()
	      generic-identifiers)))
       (with-syntax ((NUMBER-OF-ARGUMENTS	number-of-arguments)
		     (METHODS-ARGUMENTS		methods-arguments))
	 #'(begin
	     (define number-of-arguments NUMBER-OF-ARGUMENTS)
	     (mt.define-methods-table ?name
				      primary-methods-alist primary-method-add
				      primary-cache-store primary-cache-ref
				      (mt.merge-methods-alists (?generic :primary-methods-alist) ...))
	     (mt.define-methods-table ?name
				      before-methods-alist before-method-add
				      before-cache-store before-cache-ref
				      (mt.merge-methods-alists (?generic :before-methods-alist)  ...))
	     (mt.define-methods-table ?name
				      after-methods-alist after-method-add
				      after-cache-store after-cache-ref
				      (mt.merge-methods-alists (?generic :after-methods-alist)   ...))
	     (mt.define-methods-table ?name
				      around-methods-alist around-method-add
				      around-cache-store around-cache-ref
				      (mt.merge-methods-alists (?generic :around-methods-alist)  ...))
	     (define reverse-before-methods ?reverse-before-methods)
	     (define (implementation . arguments)
	       (generic*-function-implementation
		'?name
		primary-methods-alist primary-cache-ref primary-cache-store
		before-methods-alist  before-cache-ref  before-cache-store
		after-methods-alist   after-cache-ref   after-cache-store
		around-methods-alist  around-cache-ref  around-cache-store
		?uid-list-of number-of-arguments reverse-before-methods arguments))
	     (define-syntax ?name
	       (lambda (stx)
		 (syntax-case stx (:primary-method-add :primary-methods-alist
						       :after-method-add :after-methods-alist
						       :before-method-add :before-methods-alist
						       :around-method-add :around-methods-alist
						       :number-of-arguments)
		   ((_ :primary-method-add	signature closure)
		    #'(primary-method-add	signature closure))
		   ((_ :after-method-add	signature closure)
		    #'(after-method-add	signature closure))
		   ((_ :before-method-add	signature closure)
		    #'(before-method-add	signature closure))
		   ((_ :around-method-add	signature closure)
		    #'(around-method-add	signature closure))

		   ((_ :primary-methods-alist)	#'primary-methods-alist)
		   ((_ :before-methods-alist)	#'before-methods-alist)
		   ((_ :after-methods-alist)	#'after-methods-alist)
		   ((_ :around-methods-alist)	#'around-methods-alist)

		   ((_ :number-of-arguments)
		    #'number-of-arguments)
		   ((_ ?arg (... ...))
		    #'(implementation ?arg (... ...)))
		   (_
		    (syntax-violation '?name
		      "invalid arguments to generic function"
		      (syntax->datum stx))))))
	     ;;Remember that  first we bind ?NAME and  then we associate
	     ;;properties to it.
	     (define-for-expansion-evaluation
	       (ip.define #'?name #'pk.generic-function
			  (prop.make-generic (sx.unwrap #'NUMBER-OF-ARGUMENTS)
					     (sx.unwrap #'METHODS-ARGUMENTS))))
	     ))))
    (_
     (synner "invalid arguments for generic function definition"))))


(define (generic*-function-implementation who
					  primary-methods-alist primary-cache-ref primary-cache-store
					  before-methods-alist  before-cache-ref  before-cache-store
					  after-methods-alist   after-cache-ref   after-cache-store
					  around-methods-alist  around-cache-ref  around-cache-store
					  uid-list-of number-of-arguments reverse-before-methods
					  arguments)
  (define signature
    (let ((len (length arguments)))
      (unless number-of-arguments
	(assertion-violation who "called generic function with no methods"))
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
  (define-syntax define-applicable-methods
    (syntax-rules ()
      ((_ NAME ALIST STORE REF)
       (define NAME
	 (or (REF signature)
	     (let ((methods (mt.compute-applicable-methods signature ALIST)))
	       (STORE signature methods)
	       methods))))
      ((_ NAME ALIST STORE REF REVERSE?)
       (define NAME
	 (or (REF signature)
	     (let* ((ell     (mt.compute-applicable-methods signature ALIST))
		    (methods (if REVERSE? (reverse ell) ell)))
	       (STORE signature methods)
	       methods))))
      ))
  (define-applicable-methods applicable-around-methods
    around-methods-alist around-cache-store around-cache-ref)
  (define-applicable-methods applicable-primary-methods
    primary-methods-alist primary-cache-store primary-cache-ref)
  (define-applicable-methods applicable-before-methods
    before-methods-alist before-cache-store before-cache-ref reverse-before-methods)
  (define-applicable-methods applicable-after-methods
    after-methods-alist after-cache-store after-cache-ref #t)
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

(define-syntax* (add-method stx)
  (define (%register-method generic-id arg-type-ids)
    (let* ((prop		(ip.ref generic-id #'pk.generic-function #f))
	   (arg-type-ids	(sx.unwrap arg-type-ids))
	   (gf-num-of-args	(prop.generic-number-of-arguments prop))
	   (mt-num-of-args	(length arg-type-ids)))
      (unless prop
	(synner "invalid identifier as generic function name" (syntax->datum generic-id)))
      (unless (= mt-num-of-args gf-num-of-args)
	(synner (string-append "attempt to define method with wrong number of arguments, expected "
			       (number->string gf-num-of-args) " got " (number->string mt-num-of-args))
		generic-id))
      (prop.generic-methods-arguments-set! prop (cons arg-type-ids
						      (prop.generic-methods-arguments prop)))))
  (syntax-case stx ()
    ((_ ?generic-function ?keyword (?type-id ...) ?closure)
     (sx.all-identifiers? #'(?generic-function ?keyword ?type-id ...))
     (with-syntax ((KEYWORD (sx.case-identifier #'?keyword
						((:primary)	#':primary-method-add)
						((:before)	#':before-method-add)
						((:after)	#':after-method-add)
						((:around)	#':around-method-add)
						(else
						 (synner "invalid generic function kind" #'?keyword)))))
       (%register-method #'?generic-function #'(?type-id ...))
       #'(?generic-function KEYWORD
			    (list (type.uid-list ?type-id) ...) ;this is the signature
			    ?closure)))
    ((_ ?generic-function (?type-id ...) ?closure)
     (sx.all-identifiers? #'(?generic-function ?type-id ...))
     (begin
       (%register-method #'?generic-function #'(?type-id ...))
       #'(?generic-function :primary-method-add
			    (list (type.uid-list ?type-id) ...) ;this is the signature
			    ?closure)))

    ;; ((_ ?generic-function :primary (?type-id ...) ?closure)
    ;;  (begin
    ;;    (%register-method #'generic-function #'(?type-id ...))
    ;;    #'(?generic-function :primary-method-add
    ;; 			    (list (type.uid-list ?type-id) ...) ;this is the signature
    ;; 			    ?closure)))
    ;; ((_ ?generic-function :before (?type-id ...) ?closure)
    ;;  (begin
    ;;    (%register-method #'generic-function #'(?type-id ...))
    ;;    #'(?generic-function :before-method-add
    ;; 			    (list (type.uid-list ?type-id) ...) ;this is the signature
    ;; 			    ?closure)))
    ;; ((_ ?generic-function :after (?type-id ...) ?closure)
    ;;  #'(?generic-function :after-method-add
    ;; 			  (list (type.uid-list ?type-id) ...) ;this is the signature
    ;; 			  ?closure))
    ;; ((_ ?generic-function :around (?type-id ...) ?closure)
    ;;  (begin
    ;;    (%register-method #'generic-function #'(?type-id ...))
    ;;    #'(?generic-function :around-method-add
    ;; 			    (list (type.uid-list ?type-id) ...) ;this is the signature
    ;; 			    ?closure)))
    ;; ((_ ?generic-function (?type-id ...) ?closure)
    ;;  (begin
    ;;    (%register-method #'generic-function #'(?type-id ...))
    ;;    #'(?generic-function :primary-method-add
    ;; 			    (list (type.uid-list ?type-id) ...) ;this is the signature
    ;; 			    ?closure)))
    ))


;;;; done

)

;;; end of file
