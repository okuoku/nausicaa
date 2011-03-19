;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: variables handling for interpreters
;;;Date: Fri Jun 18, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa interps variables)
  (export define-variable)
  (import (nausicaa)
    (nausicaa interps variable-events))


(define-syntax define-variable
  (lambda (stx)
    (syntax-case stx ()

      ((?keyword ?eval-kont ?variable)
       #'(?keyword ?eval-kont ?variable sentinel))

      ((?keyword ?eval-kont (?variable . ?formals) . ?body)
       #'(?keyword ?eval-kont ?variable (lambda ?formals . ?body)))

      ((_ ?eval-kont ?variable ?expression)
       #'(begin
	   (define-syntax ?variable
	     (identifier-syntax
	      (_
	       (call/cc (lambda (identifier-accessor-kont)
			  (?eval-kont (make <variable-reference>
					(kont:	identifier-accessor-kont)
					(name:	'?variable))))))
	      ((set! _ ?e)
	       (call/cc (lambda (identifier-mutator-kont)
			  (?eval-kont (make <variable-mutation>
					(kont:	identifier-mutator-kont)
					(name:	'?variable)
					(value:	?e))))))
	      ))
	   (define dummy ;we want the output form to keep the definition context
	     (begin
	       (set! ?variable ?expression)
	       #f))))
      )))


;;;; done

)

;;; end of file
