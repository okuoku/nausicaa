;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: helper definitions for classes library
;;;Date: Tue Apr 27, 2010
;;;
;;;Abstract
;;;
;;;	Aaron Hsu  contributed the SYNTAX->LIST function  through a post
;;;	on comp.lang.scheme.
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


(library (classes helpers)
  (export
    %variable-name->Setter-name		%variable-name->Getter-name
    syntax->list
    syntax-prefix			syntax-suffix
    syntax-accessor-name		syntax-mutator-name
    (rename (syntax-accessor-name syntax-method-name))
    syntax-dot-notation-name
    duplicated-identifiers?		all-identifiers?)
  (import (rnrs))


(define (%variable-name->Setter-name variable-name/stx)
  (datum->syntax variable-name/stx
		 (string->symbol
		  (string-append (symbol->string (syntax->datum variable-name/stx))
				 ".__nausicaa_private_Setter_identifier_syntax"))))

(define (%variable-name->Getter-name variable-name/stx)
  (datum->syntax variable-name/stx
		 (string->symbol
		  (string-append (symbol->string (syntax->datum variable-name/stx))
				 ".__nausicaa_private_Getter_identifier_syntax"))))

(define (syntax-accessor-name class-name/stx field-name/stx)
  (datum->syntax class-name/stx
		 (string->symbol
		  (string-append (symbol->string (syntax->datum class-name/stx))
				 "-"
				 (symbol->string (syntax->datum field-name/stx))))))

(define (syntax-mutator-name class-name/stx field-name/stx)
  (datum->syntax class-name/stx
		 (string->symbol
		  (string-append (symbol->string (syntax->datum class-name/stx))
				 "-"
				 (symbol->string (syntax->datum field-name/stx))
				 "-set!"))))

(define (syntax-dot-notation-name variable-name/stx field-name/stx)
  (datum->syntax variable-name/stx
		 (string->symbol
		  (string-append (symbol->string (syntax->datum variable-name/stx))
				 "."
				 (symbol->string (syntax->datum field-name/stx))))))


(define (syntax->list stx)
  ;;Given a syntax object STX holding  a list, decompose it and return a
  ;;list of syntax  objects.  Take care of returning  a proper list when
  ;;the input is a syntax object holding a proper list.
  ;;
  ;;This functions  provides a workaround  for bugs in Ikarus  and Mosh,
  ;;which expand syntax objects holding a list into IMproper lists.
  ;;
  (syntax-case stx ()
    (()			'())
    ((?car . ?cdr)	(cons (syntax->list #'?car) (syntax->list #'?cdr)))
    (?atom		#'?atom)))

(define (syntax-prefix prefix-string symbol/stx)
  (datum->syntax symbol/stx
		 (string->symbol (string-append prefix-string
						(symbol->string (syntax->datum symbol/stx))))))

(define (syntax-suffix symbol/stx suffix-string)
  (datum->syntax symbol/stx
		 (string->symbol (string-append (symbol->string (syntax->datum symbol/stx))
						suffix-string))))


(define (all-identifiers? ell/stx)
  (for-all identifier? (syntax->list ell/stx)))

(define (duplicated-identifiers? ell/stx)
  ;;Search the list of  identifier syntax objects ELL/STX for duplicated
  ;;identifiers; return  false of a  syntax object holding  a duplicated
  ;;identifier.
  ;;
  (if (null? ell/stx)
      #f
    (let loop ((x  (car ell/stx))
	       (ls (cdr ell/stx)))
      (if (null? ls)
	  (duplicated-identifiers? (cdr ell/stx))
	(if (bound-identifier=? x (car ls))
	    x
	  (loop x (cdr ls)))))))


;;;; done

)

;;; end of file
