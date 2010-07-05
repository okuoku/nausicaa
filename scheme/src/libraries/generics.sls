;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: generic functions
;;;Date: Mon Jul  5, 2010
;;;
;;;Abstract
;;;
;;;	The ancestor of this library is ScmObj by Dorai Sitaram.
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (generics)
  (export
    define-generic define-method add-method define-generic/merge
    call-next-method next-method?)
  (import (rnrs)
    (rename (only (classes)
		  class-uid-list class-uid-list-of lambda/with-class <top>)
	    (class-uid-list	type-uid-list)
	    (class-uid-list-of	type-uid-list-of)
	    (lambda/with-class	method-lambda)
	    (<top>		top-type))
    (only (language-extensions)
	  begin0 begin0-let define-constant)
    (parameters)
    (rnrs mutable-pairs (6)))

;;;We need to define or import four bindings:
;;;
;;;type-uid-list - A syntax which, applied to a type identifier, expands
;;;into a list of symbols  representing the type hierarchy from subclass
;;;to  parent class.   All the  expansions of  this macro  for  the same
;;;identifier must yield the same list in the sense of EQ?.
;;;
;;;type-uid-list-of - A function which,  applied to any value, returns a
;;;list  of symbols  representing the  type hierarchy  from  subclass to
;;;parent class.   The returned value  must be EQ? to  the corresponding
;;;expansion of TYPE-UID-LIST.
;;;
;;;method-lambda -  A syntax which  must work like LAMBDA  but recognise
;;;arguments tagged with types.
;;;
;;;top-type - An identifier representing the topmost parent type for all
;;;the classes.
;;;


;;;; helpers

(define-syntax list-copy
  (syntax-rules ()
    ((_ ?ell)
     (let loop ((ell ?ell))
       (if (pair? ell)
	   (cons (car ell) (loop (cdr ell)))
	 ell)))))


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
;;
;;The "signature" of  a method is a list of lists,  each sublist being a
;;list of record type UIDs.  The  first sublist is the hierarchy of UIDs
;;of the first method's argument, the second sublist is the hierarchy of
;;the second arguments, etc.  For example, a method defined as:
;;
;;   (define-method (doit (a <complex>) (b <string>) (c <char>))
;;     ---)
;;
;;has the following signature:
;;
;;   ((nausicaa:builtin:<complex> nausicaa:builtin:<number>
;;     nausicaa:builtin:<builtin> nausicaa:builtin:<top>)
;;		;first argument
;;
;;    (nausicaa:builtin:<string>  nausicaa:builtin:<builtin>
;;     nausicaa:builtin:<top>)
;;		;second argument
;;
;;    (nausicaa:builtin:<char>    nausicaa:builtin:<builtin>
;;     nausicaa:builtin:<top>))
;;		;third argument
;;

(define-syntax :method-add		(syntax-rules ()))
(define-syntax :methods-alist		(syntax-rules ()))
(define-syntax :number-of-arguments	(syntax-rules ()))

(define-syntax define-generic
  (lambda (stx)
    (syntax-case stx ()

      ((_ ?name . ?rest)
       (not (identifier? #'?name))
       (syntax-violation 'define-generic
	 "expected identifier as generic function name"
	 (syntax->datum stx) (syntax->datum #'?name)))

      ((_ ?name)
       #'(define-generic ?name '()))

      ((_ ?name ?methods-alist)
       #'(begin
	   (define methods-alist ?methods-alist)
	   (define number-of-arguments
	     (if (null? methods-alist)
		 #f
	       (length (caar methods-alist))))
	   (define cache
	     (make-hashtable signature-hash eq?))
	   (define (method-add signature closure)
	     (let ((len (length signature)))
	       (if number-of-arguments
		   (unless (= number-of-arguments len)
		     (syntax-violation '?name
		       "attempt to define method with wrong number of arguments"
		       signature))
		 (set! number-of-arguments len)))
	     (hashtable-clear! cache)
	     (set! methods-alist (%add-method-to-methods-alist methods-alist signature closure)))
	   (define (implementation . arguments)
	     (generic-function-implementation '?name methods-alist cache number-of-arguments arguments))
	   (define-syntax ?name
	     (lambda (stx)
	       (syntax-case stx (:method-add :methods-alist :number-of-arguments)
		 ((_ :method-add signature closure)
		  #'(method-add signature closure))
		 ((_ :methods-alist)
		  #'methods-alist)
		 ((_ :number-of-arguments)
		  #'number-of-arguments)
		 ((_ ?arg (... ...))
		  #'(implementation ?arg (... ...)))
		 (_
		  (syntax-violation '?name
		    "invalid arguments to generic function" (syntax->datum stx))))))
	   ))
      (_
       (syntax-violation 'define-generic
	 "invalid arguments to generic function" (syntax->datum stx)))

      )))

(define (generic-function-implementation who methods-alist cache number-of-arguments arguments)

  (define signature
    (map type-uid-list-of arguments))

  (define applicable-methods
    (or (hashtable-ref cache signature #f)
	(begin0-let ((methods (%compute-applicable-methods signature methods-alist)))
	  (hashtable-set! cache signature methods))))

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
     (define dummy	;to make it a definition
       (%collect-types-and-arguments ?generic-function ?args () () . ?body)))

    ;;This is for the syntax:
    ;;
    ;;	(define-method doit ((a <alpha>) (b <beta>))
    ;;	  ---)
    ;;
    ((_ ?generic-function ?args . ?body)
     (define dummy	;to make it a definition
       (%collect-types-and-arguments ?generic-function ?args () () . ?body)))))

(define-syntax %collect-types-and-arguments
  ;;Analyse the list of method  arguments collecting a list of names and
  ;;a  list of types.   Finally call  the ADD-METHOD  syntax to  add the
  ;;method.
  ;;
  (syntax-rules ()

    ;;Matches  the form when  the next  argument to  be processed  has a
    ;;type.
    ((_ ?generic-function ((?next-arg-name ?next-type-name) . ?args)
	(?type-name ...)
	(?arg-name  ...) . ?body)
     (%collect-types-and-arguments ?generic-function ?args
				   (?type-name ... ?next-type-name)
				   (?arg-name  ... ?next-arg-name)
				   . ?body))

    ;;Matches the  form when  the next argument  to be processed  has no
    ;;type.
    ((_ ?generic-function (?next-arg-name . ?args) (?type-name ...) (?arg-name ...) . ?body)
     (%collect-types-and-arguments ?generic-function ?args
				   (?type-name ... top-type)
				   (?arg-name  ... ?next-arg-name)
				   . ?body))

    ;;Matches the form when all the arguments have been processed.
    ((_ ?generic-function () (?type-name ...) (?arg-name ...) . ?body)
     (add-method ?generic-function (?type-name ...)
		 (method-lambda ((?arg-name ?type-name) ...) . ?body)))))

(define-syntax add-method
  (syntax-rules ()
    ((_ ?generic-function (?type-name ...) ?closure)
     (?generic-function :method-add
			(list (type-uid-list ?type-name) ...) ;this is the signature
			?closure))))


;;;; method alists
;;
;;The  collection of methods  in a  generic function  is an  alist; each
;;entry has the format:
;;
;;	(signature . closure)
;;
;;the key is the method's signature.
;;

(define-constant $gf
  (greatest-fixnum))

(define (signature-hash signature)
  (if (null? (cdr signature))
      (symbol-hash (caar signature))
    (fold-left (lambda (nil uid-list)
		 (mod (+ nil (symbol-hash (car uid-list))) $gf))
	       0
	       signature)))

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

(define-syntax define-generic/merge
  (syntax-rules ()
    ((_ ?name ?gf0 ?gf ...)
     (define-generic ?name
       (merge-methods-alists (?gf0 :methods-alist)
			    (?gf  :methods-alist)
			    ...)))))

(define (merge-methods-alists methods-alist . list-of-methods-alists)
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
  ;;If  a method  with  the same  signature  does not  already exist  in
  ;;?METHODS-ALIST:  prepend   a  copy  of   ?CANDIDATE-METHOD-ENTRY  to
  ;;?METHODS-ALIST and return the new alist.
  ;;
  ;;?CANDIDATE-METHOD-ENTRY  is  copied  because  the  original  can  be
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


;;;; done

)

;;; end of file
