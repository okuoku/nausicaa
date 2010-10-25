;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: utilities for assertions and function arguments validation
;;;Date: Mon Oct 25, 2010
;;;
;;;Abstract
;;;
;;;	This library exports an API to arguments validation functions.
;;;
;;;	  One of  the purposes is to  be able to  exclude completely the
;;;	validation  code  at  expand  time  and  to  make  this  feature
;;;	available as  option at compile  time for the  underlying Scheme
;;;	implementation.
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


(library (assertions)
  (export

    assert

    ;; validation form
    validation-clauses
    who				number-of-arguments
    argument			formal
    ordinal			predicate
    description

    )
  (import (except (rnrs) assert)
    (prefix (only (rnrs) assert) rnrs.)
    (sentinel)
    (makers)
    (language-extensions)
    (for (prefix (configuration) config.) expand))


(define-syntax assert
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?expr)
       (if (config.enable-assertions?)
	   #'(rnrs.assert ?expr)
	 #'?expr)))))


(define-auxiliary-syntaxes
  who
  number-of-arguments
  argument
  formal
  ordinal
  predicate
  description)

(define-maker arguments
  %validation-clauses
  ((who				#f)
   (number-of-arguments		0)
   (argument			sentinel)))

(define-maker %argument
  %internal-validation-clause
  ((formal			sentinel)
   (ordinal			0)
   (predicate			sentinel)
   (description			"undocumented argument")))

(define-syntax validation-clauses
  (lambda (stx)
    (syntax-case stx (who number-of-arguments argument)
      ((_ (who ?who) (number-of-arguments ?number-of-arguments) (argument . ?clauses) ...)
       (if (config.enable-function-arguments-validation?)
	   #'(begin
	       (%internal-validation-clause (who ?who)
					    (number-of-arguments ?number-of-arguments)
					    . ?clauses)
	       ...)
	 #'(values))))))

(define-syntax %internal-validation-clause
  ;;Expand  a single  function argument  validation clause.   The output
  ;;form is something like:
  ;;
  ;;	(unless (<predicate> <formal>)
  ;;	  (assertion-violation <who> <description> <formal>))
  ;;
  (lambda (stx)

    (define (main stx)
      (syntax-case stx (who number-of-arguments argument ordinal predicate description)
	((_ (who			?who)
	    (number-of-arguments	?number-of-arguments)
	    (formal		?formal)
	    (ordinal		?ordinal)
	    (predicate		?predicate)
	    (description		?description))
	 #`(unless (#,(%internal-validation-predicate #'?predicate) ?formal)
	     (assertion-violation ?who
	       (receive (port getter)
		   (open-string-output-port)
		 (let-syntax ((%display (syntax-rules ()
					  ((_ ?thing)
					   (display ?thing port)))))
		   (%display "expected ")
		   (%display ?description)
		   (%display " ")
		   #,(let ((ordinal (syntax->datum #'?ordinal)))
		       (cond ((= 1 (syntax->datum #'?number-of-arguments))
			      #'(%display "as unique argument"))
			     (else
			      #'(begin
				  (%display "as argument number ")
				  (%display ?ordinal)))))
		   (getter)))
	       ?formal)))))

    (define (%internal-validation-predicate stx)
      ;;If the argument is an  identifier: expand to the binding in this
      ;;library; if the argument is a list holding an identifier: expand
      ;;to the binding in the environment of the input form.
      ;;
      ;;This trick  allows us  not to export  from this library  all the
      ;;automatically generated predicate bindings.
      ;;
      (syntax-case stx ()
	(?predicate
	 (identifier? #'?predicate)
	 #'(begin ?predicate))
	((?predicate)
	 (identifier? #'?predicate)
	 (datum->syntax #'validation-clauses (syntax->datum #'?predicate))
;;;	 #`(begin #,(datum->syntax #'validation-clauses (syntax->datum #'?predicate)))
;;;	 #'(begin ?predicate)
	 )
	(_
	 (syntax-violation 'validation-clauses
	   "invalid PREDICATE clause in function argument declaration"
	   stx #f))))

    (main stx)))


(define-syntax define-list-of-values-predicates
  (lambda (stx)
    (define (main stx)
      (syntax-case stx ()
	((_ ?value-name ?value-pred)
	 (with-syntax ((LIST-OF-VALUES?			(%list-of-values-name		#'?value-name))
		       (LIST-OF-VALUES-OR-NULL?		(%list-of-values-or-null-name	#'?value-name))
		       (LIST-OF-VALUES-OR-FALSE?	(%list-of-values-or-false-name	#'?value-name)))
	   #'(begin
	       (define-inline (LIST-OF-VALUES? ell)
		 (and (list? ell) (for-all ?value-pred ell)))
	       (define-inline (LIST-OF-VALUES-OR-NULL? ell)
		 (or (null? ell)
		     (and (list? ell) (for-all ?value-pred ell))))
	       (define-inline (LIST-OF-VALUES-OR-FALSE? ell)
		 (or (not ell)
		     (and (list? ell) (for-all ?value-pred ell))))
	       )))))
    (define (%list-of-name value-name-stx suffix-string)
      (datum->syntax value-name-stx
		     (string->symbol (string-append "list-of-"
						    (symbol->string (syntax->datum value-name-stx))
						    "?" suffix-string))))
    (define (%list-of-values-name value-name-stx)
      (%list-of-name value-name-stx ""))
    (define (%list-of-values-or-null-name value-name-stx)
      (%list-of-name value-name-stx "/or-null"))
    (define (%list-of-values-or-false-name value-name-stx)
      (%list-of-name value-name-stx "/or-false"))
    (main stx)))

(define-list-of-values-predicates booleans		boolean?)
(define-list-of-values-predicates numbers		number?)
(define-list-of-values-predicates characters		character?)
(define-list-of-values-predicates symbols		symbol?)
(define-list-of-values-predicates strings		string?)
(define-list-of-values-predicates vectors		vector?)
(define-list-of-values-predicates bytevectors		bytevector?)
(define-list-of-values-predicates hashtables		hashtable?)
(define-list-of-values-predicates procedures		procedure?)


;;;; done

)

;;; end of file
