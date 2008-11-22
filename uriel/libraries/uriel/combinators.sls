;;;
;;;Part of: Uriel libraries
;;;Contents: combinators
;;;Date: Wed Nov 19, 2008
;;;Time-stamp: <2008-11-22 07:38:30 marco>
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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



;;;; Setup.

(library (uriel combinators)
  (export
      K Y S)
  (import (rnrs)
    (uriel lang))


;;;; Code.

;;;This  syntax  comes  from  the  R6RS original  document,  Appendix  A
;;;``Formal semantics''.
(define-syntax K
  (syntax-rules ()
    ((_ ?expr0 ?expr ...)
     (call-with-values
	 (lambda () ?expr0)
       (lambda x
	 ?expr ...
	 (apply values x))))))

(define-syntax Y
  (syntax-rules ()
    ((_ ?func)
     (let ((F ?func))
       ((lambda (f)
	  (F (lambda args
	       (apply (f f) args))))
	(lambda (f)
	  (F (lambda args
	       (apply (f f) args)))))))))

(define-syntax S
  (syntax-rules ()
    ((_ ?x ?y ?z)
     (let ((Z ?z))
       (?x Z (?y Z))))))



)

;;; end of file
