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
    parse-contract ->
    build-variable-identifier-syntax
    assert-name-keyword dummy-definition-stx)
  (import (rnrs)
    (only (syntax-utilities) define-auxiliary-syntax))

(define-auxiliary-syntax ->)


(define (parse-contract contract-stx synner)
  ;;Parse the contract's  syntax object and return two  values: the list
  ;;of  identifiers referencing  the  argument predicates,  the list  of
  ;;identifiers  referencing  the return  values  predicates.  Both  the
  ;;returned lists can be null.
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


(define (build-variable-identifier-syntax keyword predicate)
  ;;Return a syntax object representing the IDENTIFIER-SYNTAX invocation
  ;;needed for the identifier macro of a variable.
  ;;
  #`(identifier-syntax
     (_
      (let ((v #,keyword)) ;we want to reference ?KEYWORD only once
	(assert (#,predicate v))
	v))
     ((set! _ ??val)
      (let ((v ??val)) ;we want to evaluate ??VAL only once
	(assert (#,predicate v))
	(set! #,keyword v)))))


(define dummy-definition-stx
  #'(define-syntax dummy (syntax-rules ())))

(define (assert-name-keyword name keyword synner)
  (unless (identifier? name)
    (synner "expected identifier as contract name" name))
  (unless (identifier? keyword)
    (synner "expected identifier as contract keyword" keyword)))


;;;; done

)

;;; end of file
