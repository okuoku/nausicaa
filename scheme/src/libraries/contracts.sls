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
    (only (syntax-utilities) define-auxiliary-syntax identifier-subst))

(define-auxiliary-syntax ->)


(define-syntax define-contract
  (lambda (stx)
    (syntax-case stx (->)

      ((_ ?name ?keyword (?predicate ... -> ?ret-predicate))
       (if (config.enable-function-arguments-validation?)
	   (with-syntax (((ARG ...) (generate-temporaries #'(?predicate ...))))
	     #'(define-syntax ?name
		 (identifier-syntax
		  (lambda (ARG ...)
		    (let ((result (?keyword (begin (assert (?predicate ARG)) ARG)
					    ...)))
		      (assert (?ret-predicate result))
		      result)))))
	 #'?keyword))

      ((_ ?name ?keyword (?predicate ...))
       (if (config.enable-function-arguments-validation?)
	   (with-syntax (((ARG ...) (generate-temporaries #'(?predicate ...))))
	     #'(define-syntax ?name
		 (identifier-syntax
		  (lambda (ARG ...)
		    (?keyword (begin (assert (?predicate ARG)) ARG)
			      ...)))))
	 #'?keyword))

      )))


(define-syntax define/contract
  (lambda (stx)
    (syntax-case stx (->)
      ((_ (?name . ?args)
	  (?predicate ... -> ?ret-predicate)
	  ?body0 ?body ...)
       (with-syntax (((KEYWORD) (generate-temporaries #'(?name))))
	 ;; (define out
	 ;;   #`(begin
	 ;;       (define-contract ?name KEYWORD ?predicate ... -> ?ret-predicate)
	 ;;       (define (KEYWORD . ?args)
	 ;; 	 #,@(identifier-subst `((,#'?name . ,#'KEYWORD)) #'(?body0 ?body ...)))))
	 ;; (write (syntax->datum out))(newline)(newline)
	 ;; out
	 #`(begin
	     (define-contract ?name KEYWORD (?predicate ... -> ?ret-predicate))
	     (define (KEYWORD . ?args)
	       #,@(identifier-subst #'(?name) #'(KEYWORD) #'(?body0 ?body ...))))
	 ))
      )))


(define-syntax with-outer-contracts
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((?name (?predicate ... -> ?ret-predicate)) ...) ?body0 ?body ...)
       (with-syntax (((KEYWORD ...) (generate-temporaries #'(?name ...))))
	 #`(begin
	     (define-contract ?name KEYWORD (?predicate ... -> ?ret-predicate))
	     ...
	     #,@(identifier-subst #'(?name ...) #'(KEYWORD ...) #'(?body0 ?body ...)))))
      )))


;;;; done

)

;;; end of file
