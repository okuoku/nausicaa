;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: helper functions for makers
;;;Date: Sat May 22, 2010
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


(library (makers helpers)
  (export syntax->list valid-keywords-and-defaults?)
  (import (rnrs))


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

(define (valid-keywords-and-defaults? keywords-and-defaults)
  (for-all (lambda (key-and-default)
	     (and (identifier? (car key-and-default))
		  (null? (cddr key-and-default))))
	   keywords-and-defaults))


;;;; done

)

;;; end of file
