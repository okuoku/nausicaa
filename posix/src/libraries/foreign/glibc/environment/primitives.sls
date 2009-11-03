;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: marshaling wrappers for environment variables
;;;Date: Tue Nov  3, 2009
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


(library (foreign glibc environment primitives)
  (export unsetenv clearenv)
  (import (rnrs)
    (receive)
    (only (foreign errno)
	  raise-errno-error)
    (prefix (foreign glibc environment platform)
	    platform:)
    (only (foreign posix marshaling)
	  with-marshaling
	  marshal-string->cstring))

  (define (clearenv)
    (with-marshaling
      (receive (result errno)
	  (platform:clearenv)
	(if (= 0 result)
	    result
	  (raise-errno-error 'clearenv result)))))

  (define (unsetenv name)
    (with-marshaling
      (receive (result errno)
	  (platform:unsetenv (marshal-string->cstring name))
	(if (= 0 result)
	    result
	  (raise-errno-error 'unsetenv result)))))

  )

;;; end of file
