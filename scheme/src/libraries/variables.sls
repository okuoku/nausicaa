;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: variables library
;;;Date: Tue Jul  7, 2009
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


(library (variables)
  (export make-variable variable?
	  variable-ref variable-set!
	  define-variable)
  (import (rnrs)
    (sentinel))

  (define-record-type (:variable :variable-make variable?)
    (sealed #t)
    (opaque #t)
    (nongenerative)
    (fields (mutable value)))

  (define variable-ref  :variable-value)
  (define variable-set! :variable-value-set!)

  (define make-variable
    (case-lambda
     (()
      (:variable-make sentinel))
     ((value)
      (:variable-make value))))

  (define-syntax define-variable
    (lambda (stx)
      (syntax-case stx ()
	((_ (?name . ?args) ?form0 ?form ...)
	 (syntax (define-variable ?name (lambda ?args ?form0 ?form ...))))
	((_ ?name)
	 (syntax (define-variable ?name sentinel)))
	((_ ?name ?value)
	 (with-syntax (((the-var)
			(generate-temporaries (syntax (?name)))))
	   (syntax (begin
		     (define the-var (:variable-make ?value))
		     (define-syntax ?name
		       (identifier-syntax
			(_
			 (variable-ref  the-var))
			((set! _ ?e)
			 (variable-set! the-var ?e))))))))))))

;;; end of file
