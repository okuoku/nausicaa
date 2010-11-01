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
(library (contracts helpers)
  (export
    ->
    build-variable-identifier-syntax
    build-function-identifier-syntax
    assert-name-keyword)
  (import (rnrs)
    (prefix (configuration) config.)
    (only (syntax-utilities) define-auxiliary-syntax))

(define-auxiliary-syntax ->)


(define (build-variable-identifier-syntax variable-stx predicate-stx)
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
  (if (config.enable-contracts?)
      #`(identifier-syntax
	 (_
	  (let ((v #,variable-stx)) ;we want to reference ?KEYWORD only once
	    (assert (#,predicate-stx v))
	    v))
	 ((set! _ ??val)
	  (let ((v ??val)) ;we want to evaluate ??VAL only once
	    (assert (#,predicate-stx v))
	    (set! #,variable-stx v))))
    #`(identifier-syntax
       (_ #,variable-stx)
       ((set! _ ?val) (set! #,variable-stx ?val)))))


(define (build-function-identifier-syntax function-stx contract-stx synner)
  ;;Return a syntax object representing the IDENTIFIER-SYNTAX invocation
  ;;needed for the identifier macro of a function or macro.
  ;;
  ;;FUNCTION-STX must be an identifier referencing the function or macro
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
  (if (not (config.enable-contracts?))
      #'(identifier-syntax function-stx)
    (let-values (((argument-predicates return-value-predicates)
		  (parse-function-contract contract-stx synner)))
      (with-syntax (((PRED ...)	argument-predicates)
		    ((ARG ...)	(generate-temporaries argument-predicates)))
	(with-syntax ((APPLICATION #`(#,function-stx (begin (assert (PRED ARG)) ARG) ...)))
	  (if (null? return-value-predicates)
	      #'(identifier-syntax (lambda (ARG ...) APPLICATION))
	    (with-syntax (((RET-PRED ...) return-value-predicates)
			  ((RET ...)      (generate-temporaries return-value-predicates)))
	      #'(identifier-syntax
		 (lambda (ARG ...)
		   (let-values (((RET ...) APPLICATION))
		     (assert (RET-PRED  RET))
		     ...
		     (values RET ...))))
	      )))))))


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


(define (assert-name-keyword name keyword synner)
  (unless (identifier? name)
    (synner "expected identifier as contract name" name))
  (unless (identifier? keyword)
    (synner "expected identifier as contract keyword" keyword)))


;;;; done

)

;;; end of file
