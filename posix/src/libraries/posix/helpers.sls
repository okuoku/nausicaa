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


(library (posix helpers)
  (export
    define-primitive-parameter
    define-parametrised)
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

  (define-syntax define-parametrised
    (lambda (stx)
      (syntax-case stx ()
	((_ ?name ?arg ...)
	 (let* ((name		(symbol->string (syntax->datum #'?name)))
		(parm-name	(string->symbol (string-append name "-function")))
		(prim-name	(string->symbol (string-append "primitive:" name))))
	   (with-syntax ((PARM-NAME (datum->syntax #'?name parm-name))
			 (PRIM-NAME (datum->syntax #'?name prim-name)))
	     #'(begin
		 (define-primitive-parameter PARM-NAME PRIM-NAME)
		 (define (?name ?arg ...)
		   ((PARM-NAME) ?arg ...)))))))))
  )

;;; end of file
