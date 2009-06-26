;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: parameters from Larceny
;;;Date: Thu Jun 18, 2009
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


#!r6rs
(library (nausicaa parameters)
  (export
    (rename (make-this-parameter make-parameter)
	    (parameterize-this parameterize)))
  (import (rnrs)
    (primitives make-parameter parameterize))

  (define make-this-parameter
    (case-lambda
     ((value validator)
      (let ((the-parm (make-parameter 'unnamed (validator value))))
	(case-lambda
	 ((value)
	  (the-parm (validator value)))
	 (()
	  (the-parm)))))
     ((value)
      (make-this-parameter value (lambda (x) x)))))

  (define-syntax parameterize-this
    (syntax-rules ()
      ((_ ?bindings ?form0 ?form ...)
       (parameterize ?bindings
	 (letrec* () ?form0 ?form ...))))))

;;; end of file
