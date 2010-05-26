;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: helper functions for expand time processing
;;;Date: Wed May 26, 2010
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


(library (syntax-utilities)
  (export
    unwrap-syntax-object		unwrap-options
    quoted-syntax-object?
    all-identifiers?			duplicated-identifiers?)
  (import (rnrs))


(define-enumeration enum-unwrap-options
  (keep-general-quoted)
  unwrap-options)

(define unwrap-syntax-object
  (case-lambda
   ((stx)
    (unwrap-syntax-object stx (unwrap-options)))
   ((stx options)
    ;;Given   a  syntax  object   STX  decompose   it  and   return  the
    ;;corresponding S-expression  holding datums and  identifiers.  Take
    ;;care of returning a proper list  when the input is a syntax object
    ;;holding a proper list.
    ;;
    ;;This functions also  provides a workaround for bugs  in Ikarus and
    ;;Mosh,  which expand syntax  objects holding  a list  into IMproper
    ;;lists.
    ;;
    ;;Aaron Hsu contributed the  SYNTAX->LIST function through a post on
    ;;comp.lang.scheme: it was used as starting point for this function.
    ;;
    (syntax-case stx ()
      (()
       '())
      ((?car . ?cdr)
       (and (enum-set-member? 'keep-general-quoted options)
	    (identifier? #'?car)
	    (or (free-identifier=? #'?car #'quote)
		(free-identifier=? #'?car #'quasiquote)
		(free-identifier=? #'?car #'syntax)
		(free-identifier=? #'?car #'quasisyntax)))
       (syntax (?car . ?cdr)))
      ((?car . ?cdr)
       (cons (unwrap-syntax-object (syntax ?car))
	     (unwrap-syntax-object (syntax ?cdr))))
      (#(?item ...)
       (list->vector (unwrap-syntax-object (syntax (?item ...)))))
      (?atom
       (identifier? (syntax ?atom))
       (syntax ?atom))
      (?atom
       (syntax->datum (syntax ?atom)))))))


(define (quoted-syntax-object? stx)
  (syntax-case stx ()
    ((?car . ?cdr)
     (and (identifier? #'?car)
	  (or (free-identifier=? #'?car #'quote)
	      (free-identifier=? #'?car #'quasiquote)
	      (free-identifier=? #'?car #'syntax)
	      (free-identifier=? #'?car #'quasisyntax)))
     #t)
    (_ #f)))

(define (all-identifiers? stx)
  (for-all identifier? (unwrap-syntax-object stx)))

(define (duplicated-identifiers? ell/stx)
  ;;Recursive  function.  Search  the  list of  identifiers ELL/STX  for
  ;;duplicated  identifiers; at  the first  duplicate found,  return it;
  ;;return false if no duplications are found.
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
