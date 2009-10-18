;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: deferred exceptions handling
;;;Date: Sat Aug 15, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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


(library (deferred-exceptions)
  (export
    with-deferred-exceptions-handler
    defer-exceptions run-deferred-exceptions-handler)
  (import (rnrs)
    (parameters))


(define deferred-exceptions
  (make-parameter #f))

(define deferred-exceptions-handler
  (make-parameter #f))

(define (run-deferred-exceptions-handler)
  (unless (null? (deferred-exceptions))
    (for-each
	(lambda (exc)
	  (guard (exc (else #f))
	    ((deferred-exceptions-handler) exc)))
      (deferred-exceptions))
    (deferred-exceptions '())))

(define-syntax defer-exceptions
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (guard (exc (else
		  (let ((e (deferred-exceptions)))
		    (and e (deferred-exceptions (cons exc e))))))
       ?form0 ?form ...))))

(define-syntax with-deferred-exceptions-handler
  (syntax-rules ()
    ((_ ?handler ?thunk)
     (parametrise ((deferred-exceptions '())
		   (deferred-exceptions-handler ?handler))
       (dynamic-wind
	   (lambda () #f)
	   ?thunk
	   (lambda () (run-deferred-exceptions-handler)))))))


;;;; done

)

;;; end of file
