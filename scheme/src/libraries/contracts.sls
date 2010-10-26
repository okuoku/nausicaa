;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: contracts for functions
;;;Date: Mon Oct 25, 2010
;;;
;;;Abstract
;;;
;;;
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
(library (contracts)
  (export define-contract -> define/contract with-outer-contracts)
  (import (rnrs)
    (prefix (configuration) config.)
    (syntax-utilities))

(define-auxiliary-syntax ->)


(define-syntax define-contract
  (lambda (stx)
    (define (main stx)
      (syntax-case stx (-> values)

	;;Multiple return values.
	;;
	;;Notice  that the  only reason  for the  ?RET-PREDICATE pattern
	;;variables to  be enclosed in parentheses is  to allow ellipses
	;;for   both   ?PREDICATE   and  ?RET-predicate;   without   the
	;;parentheses the two ellipses would be at the same level.
	;;
	((_ ?name ?keyword (?predicate ... -> (?ret-predicate0 ?ret-predicate ...)))
	 (begin
	   (%assert-name-keyword #'?name #'?keyword)
	   (for-each %assert-predicate
	     (unwrap-syntax-object #'(?predicate ... ?ret-predicate0 ?ret-predicate ...)))
	   (if (config.enable-contracts?)
	       (with-syntax
		   (((ARG ...)      (generate-temporaries #'(?predicate ...)))
		    ((RET0 RET ...) (generate-temporaries #'(?ret-predicate0 ?ret-predicate ...))))
		 #'(define-syntax ?name
		     (identifier-syntax
		      (lambda (ARG ...)
			(let-values
			    (((RET0 RET ...)
			      (?keyword (begin (assert (?predicate ARG)) ARG) ...)))
			  (assert (?ret-predicate0 RET0))
			  (assert (?ret-predicate  RET))
			  ...
			  (values RET0 RET ...))))))
	     dummy-definition-stx)))

	;;Single return value.
	((_ ?name ?keyword (?predicate ... -> ?ret-predicate))
	 (begin
	   (%assert-name-keyword #'?name #'?keyword)
	   (for-each %assert-predicate
	     (unwrap-syntax-object #'(?predicate ... ?ret-predicate)))
	   (if (config.enable-contracts?)
	       (with-syntax (((ARG ...) (generate-temporaries #'(?predicate ...))))
		 #'(define-syntax ?name
		     (identifier-syntax
		      (lambda (ARG ...)
			(let ((return (?keyword (begin (assert (?predicate ARG)) ARG) ...)))
			  (assert (?ret-predicate return))
			  return)))))
	     dummy-definition-stx)))

	;;No return value.
	((_ ?name ?keyword (?predicate ...))
	 (begin
	   (%assert-name-keyword #'?name #'?keyword)
	   (for-each %assert-predicate
	     (unwrap-syntax-object #'(?predicate ...)))
	   (if (config.enable-contracts?)
	       (with-syntax (((ARG ...) (generate-temporaries #'(?predicate ...))))
		 #'(define-syntax ?name
		     (identifier-syntax
		      (lambda (ARG ...)
			(?keyword (begin (assert (?predicate ARG)) ARG)
				  ...)))))
	     dummy-definition-stx)))
	))

    (define (%assert-name-keyword name keyword)
      (unless (identifier? name)
	(%synner "expected identifier as contract name" name))
      (unless (identifier? keyword)
	(%synner "expected identifier as contract keyword" keyword)))

    (define (%assert-predicate stx)
      (unless (identifier? stx)
	(%synner "expected identifier as contract predicate" stx)))

    (define (%synner message subform)
      (syntax-violation 'define-contract message (syntax->datum stx) (syntax->datum subform)))

    (define dummy-definition-stx
      #'(define-syntax dummy (syntax-rules ())))

    (main stx)))


(define-syntax define/contract
  (lambda (stx)
    (syntax-case stx (->)
      ((_ (?name . ?args) ?contract ?body0 ?body ...)
       (if (config.enable-contracts?)
	   (with-syntax (((KEYWORD) (generate-temporaries #'(?name))))
	     #`(begin
		 (define-contract ?name KEYWORD ?contract)
		 (define (KEYWORD . ?args)
		   #,@(identifier-subst #'(?name) #'(KEYWORD) #'(?body0 ?body ...)))))
	 #'(define (?name . ?args) ?body0 ?body ...)))
      )))


(define-syntax with-outer-contracts
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((?name ?contract) ...) ?body0 ?body ...)
       (if (config.enable-contracts?)
	   (with-syntax (((KEYWORD ...) (generate-temporaries #'(?name ...))))
	     #`(begin
		 (define-contract ?name KEYWORD ?contract)
		 ...
		 #,@(identifier-subst #'(?name ...) #'(KEYWORD ...) #'(?body0 ?body ...))))
	 #'(begin ?body0 ?body ...)))
      )))


;;;; done

)

;;; end of file
