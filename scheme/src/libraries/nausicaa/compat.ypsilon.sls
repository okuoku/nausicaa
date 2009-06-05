;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: Ypsilon compatibility library for (nausicaa) language
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
    make-parameter parameterize

    ;; environment variables
    (rename (lookup-process-environment get-environment-variable)
	    (process-environment->alist get-environment-variables)))
  (import (rename (rnrs)
		  (finite?	rnrs:finite?)
		  (infinite?	rnrs:infinite?)
		  (nan?		rnrs:nan?))
    (only (core)
	  make-parameter parameterize pretty-print
	  lookup-process-environment process-environment->alist))


;;;; implementation

(define implementation-features
  '(ypsilon))

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

)

;;; end of file
