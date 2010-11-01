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
	  define/contract with-outer-contracts
	  enforce-contracts)
  (import (rnrs)
    (prefix (configuration) config.)
    (syntax-utilities)
    (only (language-extensions) define-syntax*)
    (for (contracts helpers) run expand))


(define-syntax* (enforce-contracts stx)
  (syntax-case stx ()
    ((_ ?expression ...)
     (if (config.enable-contracts?)
	 #'(begin (assert ?expression) ...)
       #'(values)))))


(define-syntax* (define-contract stx)
  (syntax-case stx ()
    ((_ ?name ?keyword ?contract)
     (begin
       (assert-name-keyword #'?name #'?keyword synner)
       #`(define-syntax ?name
	   #,(if (identifier? #'?contract)
		 (build-variable-identifier-syntax #'?keyword #'?contract)
	       (build-function-identifier-syntax #'?keyword #'?contract synner)))
       ))
    (_
     (synner "invalid input form in contract definition" #f))))


(define-syntax* (let-contract stx)
  (syntax-case stx ()

    ((_ () ?body0 ?body ...)
     #'(begin ?body0 ?body ...))

    ((_ ((?name ?keyword ?contract) ...) ?body0 ?body ...)
     (let ((names	(unwrap-syntax-object #'(?name ...)))
	   (keywords	(unwrap-syntax-object #'(?keyword ...)))
	   (contracts	(unwrap-syntax-object #'(?contract ...))))
       (for-all (lambda (name keyword)
		  (assert-name-keyword name keyword synner))
	 names keywords)
       (with-syntax (((RHS ...)
		      (map (lambda (keyword-stx contract-stx)
			     (if (identifier? contract-stx)
				 (build-variable-identifier-syntax keyword-stx contract-stx)
			       (build-function-identifier-syntax keyword-stx contract-stx synner)))
			keywords contracts)))
	 #'(let-syntax ((?name RHS) ...) ?body0 ?body ...)
	 )))

    (_
     (synner "invalid input form in contract definition" #f))))


(define-syntax* (define/contract stx)
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
     (synner "invalid syntax in contract definition"))))


(define-syntax* (with-outer-contracts stx)
  (syntax-case stx ()
    ((_ ((?name ?contract) ...) ?body0 ?body ...)
     (if (config.enable-contracts?)
	 (with-syntax (((KEYWORD ...) (generate-temporaries #'(?name ...))))
	   #`(begin ;notice that this is NOT like LET-CONTRACT
	       (define-contract ?name KEYWORD ?contract)
	       ...
	       #,@(identifier-subst #'(?name ...) #'(KEYWORD ...) #'(?body0 ?body ...))))
       #'(begin ?body0 ?body ...)))
    (_
     (synner "invalid syntax in region under contracts"))))


;;;; done

)

;;; end of file
