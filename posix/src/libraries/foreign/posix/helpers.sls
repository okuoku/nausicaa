;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: miscellaneous helpers
;;;Date: Sun Oct 18, 2009
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


(library (foreign posix helpers)
  (export
    define-primitive-parameter)
  (import (rnrs)
    (only (parameters)
	  make-parameter))

  (define-syntax define-primitive-parameter
    (syntax-rules ()
      ((_ ?parameter-name ?primitive-name)
       (define ?parameter-name
	 (make-parameter ?primitive-name
	   (lambda (func)
	     (unless (procedure? func)
	       (assertion-violation (quote ?parameter-name)
		 (string-append "expected procedure as value for "
				?parameter-name
				" parameter")
		 func))
	     func))))))
  )

;;; end of file
