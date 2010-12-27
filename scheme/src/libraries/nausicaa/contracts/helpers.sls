;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: helper functions for the contracts library
;;;Date: Wed Oct 27, 2010
;;;
;;;Abstract
;;;
;;;	This  library   exists  because  the   DEFINE-CONTRACT  and  the
;;;	LET-CONTRACT macros make use of the same subroutines.
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa contracts helpers)
  (export
    ->
    build-variable-identifier-syntax
    build-function-identifier-syntax
    assert-keyword-subject

    &contract-violation
    make-contract-violation-condition
    (rename (contract-violation-condition? contract-violation?))
    condition-contract-violation/subject)
  (import (rnrs)
    (nausicaa language conditions)
    (prefix (nausicaa configuration) config.)
    (only (nausicaa language syntax-utilities) define-auxiliary-syntax))

(define-auxiliary-syntax ->)


(define-condition &contract-violation
  (parent &assertion)
  (fields subject))


(define (build-variable-identifier-syntax keyword-stx variable-stx predicate-stx)
  ;;Return a syntax object representing the IDENTIFIER-SYNTAX invocation
  ;;needed for the identifier macro of a variable.
  ;;
  ;;VARIABLE-STX  must  be  an   identifier  referencing  the  value  to
  ;;validate;  PREDICATE-STX  must  be  an  identifier  referencing  the
  ;;validation predicate.
  ;;
  ;;The returned  form can  be used as  right-hand side of  a LET-SYNTAX
  ;;binding:
  ;;
  ;;  #`(let-syntax
  ;;        ((key #,(build-variable-identifier-syntax #'var #'pred)))
  ;;      . body)
  ;;
  (define (%make-assert var)
    #`(unless (#,predicate-stx #,var)
    	(raise
    	 (condition
    	  (make-contract-violation-condition (quote #,variable-stx))
	  (make-irritants-condition (list #,var))
    	  (make-message-condition
	   (string-append #,(string-append "contract for variable \""
					   (symbol->string (syntax->datum variable-stx))
					   "\" and keyword \""
					   (symbol->string (syntax->datum keyword-stx))
					   "\" with predicate \""
					   (symbol->string (syntax->datum predicate-stx))
					   "\" violated by value: ")
			  (call-with-string-output-port
			      (lambda (port)
				(write #,var port)))))))))
  (if (config.enable-contracts?)
      (with-syntax ((V (datum->syntax variable-stx 'v)))
	#`(identifier-syntax
	   (_
	    (let ((V #,variable-stx)) ;we want to reference VARIABLE-STX only once
	      #,(%make-assert #'V)
	      V))
	   ((set! _ ?val)
	    (let ((V ?val)) ;we want to evaluate ?VAL only once
	      #,(%make-assert #'V)
	      (set! #,variable-stx V)))))
    #`(identifier-syntax
       (_ #,variable-stx)
       ((set! _ ?val) (set! #,variable-stx ?val)))))


(define (build-function-identifier-syntax keyword-stx subject-stx contract-stx synner)
  ;;Return a syntax object representing the IDENTIFIER-SYNTAX invocation
  ;;needed for the identifier macro of a function or macro.
  ;;
  ;;SUBJECT-STX must be an  identifier referencing the function or macro
  ;;to wrap; CONTRACT-STX must be the syntax object holding the contract
  ;;for a function or macro; SYNNER must be the function used to raise a
  ;;syntax violation for the calling macro transformer.
  ;;
  ;;The returned  form can  be used as  right-hand side of  a LET-SYNTAX
  ;;binding:
  ;;
  ;;  #`(let-syntax
  ;;        ((key #,(build-function-identifier-syntax #'var #'contract)))
  ;;      . body)
  ;;
  (define (main)
    (if (not (config.enable-contracts?))
	#'(identifier-syntax subject-stx)
      (let*-values (((argument-predicates retval-predicates)
		     (parse-function-contract contract-stx synner))
		    ((argument-formals)	(generate-temporaries argument-predicates))
		    ((retval-formals)	(generate-temporaries retval-predicates))
		    ((argument-reporter)	(datum->syntax keyword-stx 'reporter))
		    ((retval-reporter)	(datum->syntax keyword-stx 'retval-reporter)))
	(with-syntax
	    (((ARG ...) argument-formals)
	     ((RET ...) retval-formals)
	     ((ARGUMENT ...)
	      (if (null? argument-formals)
		  '()
		(%make-list-of-argument-assertions argument-formals argument-predicates
						   argument-reporter)))
	     ((RETVAL ...)
	      (if (null? retval-formals)
		  '()
		(%make-list-of-retval-assertions retval-formals retval-predicates retval-reporter)))
	     (ARGUMENT-REPORTER-DEFINITION
	      (if (null? argument-formals)
		  #f
		(%make-argument-reporter-definition argument-reporter)))
	     (RETVAL-REPORTER-DEFINITION
	      (if (null? retval-formals)
		  #f
		(%make-retval-reporter-definition retval-reporter))))
	  (if (null? retval-formals)
	      (if (null? argument-formals)
		  #`(identifier-syntax #,subject-stx)
		#`(identifier-syntax
		   (lambda (ARG ...)
		     ARGUMENT-REPORTER-DEFINITION
		     (#,subject-stx ARGUMENT ...))))
	    (if (null? argument-formals)
		#`(identifier-syntax
		   (lambda ()
		     RETVAL-REPORTER-DEFINITION
		     (let-values (((RET ...) (#,subject-stx)))
		       (values RETVAL ...))))
	      #`(identifier-syntax
		 (lambda (ARG ...)
		   ARGUMENT-REPORTER-DEFINITION
		   RETVAL-REPORTER-DEFINITION
		   (let-values (((RET ...) (#,subject-stx ARGUMENT ...)))
		     (values RETVAL ...))))))
	  ))))

  (define (%make-list-of-argument-assertions argument-formals argument-predicates argument-reporter)
    (let loop ((asserts		'())
	       (index		1)
	       (predicates	argument-predicates)
	       (formals		argument-formals))
      (if (null? predicates)
	  (reverse asserts)
	(loop (cons #`(begin
			(unless (#,(car predicates) #,(car formals))
			  (#,argument-reporter #,(car formals) #,(number->string index)))
			#,(car formals))
		    asserts)
	      (+ 1 index)
	      (cdr predicates)
	      (cdr formals)))))

  (define (%make-list-of-retval-assertions retval-formals retval-predicates retval-reporter)
    (let loop ((asserts		'())
	       (index		1)
	       (predicates	retval-predicates)
	       (formals		retval-formals))
      (if (null? predicates)
	  (reverse asserts)
	(loop (cons #`(begin
			(unless (#,(car predicates) #,(car formals))
			  (#,retval-reporter #,(car formals) #,(number->string index)))
			#,(car formals))
		    asserts)
	      (+ 1 index)
	      (cdr predicates)
	      (cdr formals)))))

  (define (%make-argument-reporter-definition argument-reporter)
    #`(define (#,argument-reporter argument argument-index)
	(raise
	 (condition
	  (make-contract-violation-condition (quote #,subject-stx))
	  (make-irritants-condition (list argument))
	  (make-message-condition
	   (string-append
	    #,(string-append "contract for function or macro \""
			     (symbol->string (syntax->datum subject-stx))
			     "\" and keyword \""
			     (symbol->string (syntax->datum keyword-stx))
			     "\" with contract \""
			     (%obj->string (syntax->datum contract-stx))
			     "\" violated by argument number ")
	    argument-index ": "
	    (call-with-string-output-port
		(lambda (port)
		  (write argument port)))))))))

  (define (%make-retval-reporter-definition retval-reporter)
    #`(define (#,retval-reporter retval retval-index)
	(raise
	 (condition
	  (make-contract-violation-condition (quote #,subject-stx))
	  (make-irritants-condition (list retval))
	  (make-message-condition
	   (string-append
	    #,(string-append "contract for function or macro \""
			     (symbol->string (syntax->datum subject-stx))
			     "\" and keyword \""
			     (symbol->string (syntax->datum keyword-stx))
			     "\" with contract \""
			     (%obj->string (syntax->datum contract-stx))
			     "\" violated by return value number ")
	    retval-index ": "
	    (call-with-string-output-port
		(lambda (port)
		  (write retval port)))))))))

  (define (%obj->string obj)
    (call-with-string-output-port
	(lambda (port)
	  (write obj port))))

  (main))


(define (parse-function-contract contract-stx synner)
  ;;Parse the contract's  syntax object and return two  values: the list
  ;;of  identifiers referencing  the  argument predicates,  the list  of
  ;;identifiers  referencing  the return  values  predicates.  Both  the
  ;;returned lists can be null.
  ;;
  ;;We want to support the following forms for a CONTRACT-STX:
  ;;
  ;;    (?predicate ... -> ?ret-predicate ...)
  ;;    (?predicate ...)
  ;;    (-> ?ret-predicate ...)
  ;;
  ;;but SYNTAX-CASE  cannot handle multiple ellipses at  the same level;
  ;;also, by experimenting, it has  been verified that using an ellipsis
  ;;and a rest pattern variable as in:
  ;;
  ;;    (?predicate ... -> ?ret-predicate0 . ?ret-predicates)
  ;;
  ;;also is not handles by SYNTAX-CASE.  So we use a loop and unwrap the
  ;;syntax object element by element.
  ;;
  (let loop ((stx	contract-stx)
	     (in-preds?	#t)
	     (preds	'())
	     (ret-preds	'()))
    (syntax-case stx (->)
      (()
       (values (reverse preds) (reverse ret-preds)))
      ((-> . ?cdr)
       (loop #'?cdr #f preds ret-preds))
      ((?car . ?cdr)
       (identifier? #'?car)
       (if in-preds?
	   (loop #'?cdr in-preds? (cons #'?car preds) ret-preds)
	 (loop #'?cdr in-preds? preds (cons #'?car ret-preds))))
      (else
       (synner "expected identifier as contract predicate" stx)))))


(define (assert-keyword-subject keyword subject synner)
  (unless (identifier? keyword)
    (synner "expected identifier as contract keyword" keyword))
  (unless (identifier? subject)
    (synner "expected identifier as contract subject" subject)))


;;;; done

)

;;; end of file
