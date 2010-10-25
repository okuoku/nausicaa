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
  (export define-contract ->)
  (import (rnrs)
    (prefix (assertions) ass.)
    (only (syntax-utilities) define-auxiliary-syntax))


(define-auxiliary-syntax ->)

(define-syntax define-contract
  (lambda (stx)
    (syntax-case stx (->)

      ((_ ?name ?keyword ?predicate ... -> ?ret-predicate)
       (with-syntax (((ARG ...) (generate-temporaries #'(?predicate ...))))
	 #'(define-syntax ?name
	     (identifier-syntax
	      (lambda (ARG ...)
		(let ((result (?keyword (begin (ass.assert (?predicate ARG)) ARG)
					...)))
		  (ass.assert (?ret-predicate result))
		  result))))))

      ((_ ?name ?keyword ?predicate ...)
       (with-syntax (((ARG ...) (generate-temporaries #'(?predicate ...))))
	 #'(define-syntax ?name
	     (identifier-syntax
	      (lambda (ARG ...)
		(?keyword (begin (ass.assert (?predicate ARG)) ARG)
			  ...))))))

      )))


;;;; done

)

;;; end of file
