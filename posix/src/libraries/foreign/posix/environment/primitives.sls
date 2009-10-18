;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: primitive functions for environment variables
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


(library (foreign posix environment primitives)
  (export primitive-setenv primitive-getenv)
  (import (rnrs)
    (only (foreign ffi pointers)
	  pointer-null?)
    (foreign posix environment platform)
    (foreign posix marshaling))

  (define primitive-setenv
    (case-lambda
     ((varname newvalue)
      (primitive-setenv varname newvalue #t))
     ((varname newvalue replace)
      (with-marshaling
	(letrec
	    ((name  (string->cstring varname))
	     (value (string->cstring newvalue)))
	  (platform-setenv name value (if replace 1 0)))))))

  (define (primitive-getenv varname)
    (with-marshaling
      (let ((p (platform-getenv (string->cstring varname))))
	(if (pointer-null? p)
	    #f
	  (cstring->string p))))))

;;; end of file
