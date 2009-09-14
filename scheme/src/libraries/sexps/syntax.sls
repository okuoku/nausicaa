;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: macro transformer
;;;Date: Sat Sep 12, 2009
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


(library (sexps syntax)
  (export define-sexp-macro)
  (import (rnrs)
    (for (sexps) expand run))

  (define-syntax define-sexp-macro
    (syntax-rules ()
      ((_ ?name ((?var ?default) ...) ?pattern ?output)
       (define-syntax ?name
	 (let-sexp-variables ((?var ?default) ...)
	   (let ((transformer (make-sexp-transformer ?pattern ?output)))
	     (lambda (stx)
	       (syntax-case stx ()
		 ((?kontext ?arg (... ...))
		  (let ((output (transformer (syntax->datum (syntax (?arg (... ...)))))))
		    (datum->syntax (syntax ?kontext)
				   output)))))))))))

  )

;;; end of file
