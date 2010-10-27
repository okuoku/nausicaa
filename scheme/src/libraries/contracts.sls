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
  (export define-contract let-contract ->
	  define/contract with-outer-contracts)
  (import (rnrs)
    (prefix (configuration) config.)
    (syntax-utilities)
    (for (contracts helpers) run expand))


(define-syntax define-contract
  (lambda (stx)
    (define (%synner message subform)
      (syntax-violation 'define-contract message (syntax->datum stx) (syntax->datum subform)))

    (syntax-case stx ()

      ;; variable contract
      ((_ ?name ?keyword ?predicate)
       (identifier? #'?predicate)
       (begin
	 (assert-name-keyword #'?name #'?keyword %synner)
	 ;;The test for enabled contracts is performed by the procedure.
	 #`(define-syntax ?name
	     #,(build-variable-identifier-syntax #'?keyword #'?predicate))))

      ;; function contract
      ((_ ?name ?keyword ?contract)
       ;;We want to support the following forms for a ?CONTRACT:
       ;;
       ;;    (?predicate ... -> ?ret-predicate ...)
       ;;    (?predicate ...)
       ;;    (-> ?ret-predicate ...)
       ;;
       ;;but SYNTAX-CASE  cannot handle multiple ellipses  at the same
       ;;level;  also, by  experimenting,  it has  been verified  that
       ;;using an ellipsis and a rest pattern variable as in:
       ;;
       ;;    (?predicate ... -> ?ret-predicate0 . ?ret-predicates)
       ;;
       ;;also  is   not  handles  by  SYNTAX-CASE.   So   we  use  the
       ;;PARSE-CONTRACT function  to split a  contract's syntax object
       ;;into a list of ?PREDICATE and a list of ?RET-PREDICATE.
       ;;
       (begin
	 (assert-name-keyword #'?name #'?keyword %synner)
	 (if (config.enable-contracts?)
	     (let-values (((preds ret-preds) (parse-contract #'?contract %synner)))
	       (with-syntax (((?predicate ...) preds)
			     ((ARG ...)        (generate-temporaries preds)))
		 (with-syntax ((APPLICATION #'(?keyword (begin (assert (?predicate ARG)) ARG) ...)))
		   (if (null? ret-preds)
		       #'(define-syntax ?name
			   (identifier-syntax (lambda (ARG ...) APPLICATION)))
		     (with-syntax (((?ret-predicate ...) ret-preds)
				   ((RET ...)            (generate-temporaries ret-preds)))
		       #'(define-syntax ?name
			   (identifier-syntax
			    (lambda (ARG ...)
			      (let-values (((RET ...) APPLICATION))
				(assert (?ret-predicate  RET))
				...
				(values RET ...))))))))))
	   #'(define-syntax ?name (identifier-syntax ?keyword)))))

      (_
       (%synner "invalid input form in contract definition" #f))

      )))


(define-syntax let-contract
  (lambda (stx)
    (define (%synner message subform)
      (syntax-violation 'let-contract message (syntax->datum stx) (syntax->datum subform)))

    (syntax-case stx ()

      ((_ () ?body0 ?body ...)
       #'(begin ?body0 ?body ...))

      ((_ ((?name ?keyword ?predicate) . ?contracts) ?body0 ?body ...)
       (identifier? #'?predicate)
       (begin
	 (assert-name-keyword #'?name #'?keyword %synner)
	 #`(let-syntax ((?name #,(build-variable-identifier-syntax #'?keyword #'?predicate)))
	     (let-contract ?contracts ?body0 ?body ...))))

      ((_ ((?name ?keyword ?contract) . ?contracts) ?body0 ?body ...)
       (begin
	 (assert-name-keyword #'?name #'?keyword %synner)
	 (if (config.enable-contracts?)
	     (let-values (((preds ret-preds) (parse-contract #'?contract %synner)))
	       (with-syntax (((?predicate ...) preds)
			     ((ARG ...)        (generate-temporaries preds)))
		 (with-syntax ((APPLICATION #'(?keyword (begin (assert (?predicate ARG)) ARG) ...)))
		   (if (null? ret-preds)
		       #'(let-syntax ((?name (identifier-syntax (lambda (ARG ...) APPLICATION))))
			   (let-contract ?contracts ?body0 ?body ...))
		     (with-syntax (((?ret-predicate ...) ret-preds)
				   ((RET ...)            (generate-temporaries ret-preds)))
		       #'(let-syntax ((?name (identifier-syntax
					      (lambda (ARG ...)
						(let-values (((RET ...) APPLICATION))
						  (assert (?ret-predicate  RET))
						  ...
						  (values RET ...))))))
			   (let-contract ?contracts ?body0 ?body ...)))))))
	   #'(let-syntax ((?name (identifier-syntax ?keyword)))
	       (let-contract ?contracts ?body0 ?body ...)))))

      (_
       (%synner "invalid input form in contract definition" #f))
      )))


(define-syntax define/contract
  (lambda (stx)
    (syntax-case stx (->)

      ((_ (?name . ?args) (?contract0 ?contract ...) ?body0 ?body ...)
       (if (config.enable-contracts?)
	   #`(begin
	       (define-contract ?name keyword (?contract0 ?contract ...))
	       (define (keyword . ?args) ?body0 ?body ...))
	 #'(define (?name . ?args) ?body0 ?body ...)))

      ((_ ?name ?contract ?expression)
       (identifier? #'?name)
       (if (config.enable-contracts?)
	   #'(begin
	       (define-contract ?name keyword ?contract)
	       (define keyword ?expression))
	 #'(define ?name ?expression)))

      ((_ ?name ?contract)
       (identifier? #'?name)
       (if (config.enable-contracts?)
	   #'(begin
	       (define-contract ?name keyword ?contract)
	       (define keyword))
	 #'(define ?name)))

      (_
       (syntax-violation 'define/contract "invalid syntax in contract definition" stx #f))

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
