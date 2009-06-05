;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: Larceny compatibility library for (nausicaa) language
;;;Date: Wed Jan 21, 2009
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


;;;; setup

#!r6rs
(library (nausicaa compat)
  (export

    equal-hash pretty-print implementation-features
    finite? infinite? nan?

    ;; parameters
    (rename (make-this-parameter make-parameter)
	    (parameterize-this parameterize))

    ;; environment variables
    (rename (getenv get-environment-variable))
    get-environment-variables)
  (import (rename (rnrs)
		  (finite?	rnrs:finite?)
		  (infinite?	rnrs:infinite?)
		  (nan?		rnrs:nan?))
    (primitives make-parameter parameterize getenv pretty-print)
    (nausicaa unimplemented))


;;;; implementation

  (define implementation-features
    '(larceny))

;;; --------------------------------------------------------------------

(define (finite? num)
  (if (complex? num)
      (and (rnrs:finite? (real-part num))
	   (rnrs:finite? (imag-part num)))
    (rnrs:finite? num)))

(define-syntax cplx-or-pred
  (syntax-rules ()
    ((_ ?pred ?rnrs-pred)
     (define (?pred num)
       (if (complex? num)
	   (or (?rnrs-pred (real-part num))
	       (?rnrs-pred (imag-part num)))
	 (?rnrs-pred num))))))

(cplx-or-pred infinite?	rnrs:infinite?)
(cplx-or-pred nan?	rnrs:nan?)

;;; --------------------------------------------------------------------

  (define (get-environment-variables)
    (raise-unimplemented-error 'get-environment-variables))

;;; --------------------------------------------------------------------

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
	 (letrec* () ?form0 ?form ...)))))

  )

;;; end of file
