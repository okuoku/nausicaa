;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: asynchronous resources handling
;;;Date: Sat Aug 15, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marcomaggi@gna.org>
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


(library (compensations)
  (export
    with-compensations with-compensations/on-error
    compensate run-compensations push-compensation)
  (import (rnrs)
    (begin0)
    (parameters)
    (deferred-exceptions))


(define compensations
  (make-parameter #f))

(define (run-compensations)
  (when (compensations)
    (for-each
	(lambda (closure)
	  (defer-exceptions
	    (closure)))
      (compensations))
    (compensations '())))

(define-syntax with-compensations/on-error
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (parametrise ((compensations '()))
       (with-exception-handler
	   (lambda (E)
	     (run-compensations)
	     (raise E))
	 (lambda ()
	   ?form0 ?form ...))))))

(define-syntax with-compensations
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (with-compensations/on-error
       (begin0
	   (begin ?form0 ?form ...)
	 (run-compensations))))))

(define-syntax push-compensation
  (syntax-rules ()
    ((_ ?release0 ?release ...)
     (begin
       (compensations (cons (lambda () ?release0 ?release ...)
			    (compensations)))))))

(define-syntax compensate
  ;;Splitting  COMPENSATE in  this macro  and %COMPENSATE  allows  us to
  ;;catch missing WITH subform errors.
  (syntax-rules (begin with)
    ((_ ?alloc ?form0 ?form ...)
     (%compensate (begin ?alloc) ?form0 ?form ...))))

(define-syntax %compensate
  (syntax-rules (begin with)
    ((_ (begin ?alloc0 ?alloc ...) (with ?release0 ?release ...))
     (begin0
	 (begin ?alloc0 ?alloc ...)
       (push-compensation ?release0 ?release ...)))

    ((_ (begin ?alloc0 ?alloc ...) ?allocn ?form ...)
     (%compensate (begin ?alloc0 ?alloc ... ?allocn) ?form ...))))


;;;; done

)

;;; end of file
