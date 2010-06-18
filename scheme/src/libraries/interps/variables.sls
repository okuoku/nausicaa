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


(library (interps variables)
  (export define-variable)
  (import (rnrs)
    (sentinel))


(define-syntax define-variable
  (lambda (stx)
    (syntax-case stx ()

      ((_ ?variable)
       (syntax (define-variable ?variable sentinel)))

      ((_ ?variable ?expression)
       #'(define-syntax ?variable
	   (identifier-syntax
	    (_
	     (call/cc (lambda (kk)
			(k (list sentinel kk (quote ?variable) sentinel)))))
	    ((set! _ ?e)
	     (call/cc (lambda (kk)
			(k (list sentinel kk (quote ?variable) ?e))))))))

       ((_ (?variable . ?formals) . ?body)
	)

       ))))


;;;; done

)

;;; end of file
