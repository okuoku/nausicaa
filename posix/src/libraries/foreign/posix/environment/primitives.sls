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
  (export setenv getenv environ)
  (import (rnrs)
    (prefix (foreign posix environment platform)
	    platform:)
    (only (foreign ffi pointers)
	  pointer-null?)
    (foreign cstrings)
    (foreign ffi)
    (only (foreign posix marshaling)
	  with-marshaling
	  marshal-cstring->string
	  marshal-string->cstring
	  marshal-argv->strings))

  (define setenv
    (case-lambda
     ((varname newvalue)
      (setenv varname newvalue #t))
     ((varname newvalue replace)
      (with-marshaling
	(platform:setenv (marshal-string->cstring varname)
			 (marshal-string->cstring newvalue)
			 (if replace 1 0))))))

  (define (getenv varname)
    (with-marshaling
      (let ((p (platform:getenv (marshal-string->cstring varname))))
	(if (pointer-null? p)
	    #f
	  (marshal-cstring->string p)))))

  (define (environ)
    (with-marshaling
      (marshal-argv->strings (platform:environ)))))

;;; end of file
