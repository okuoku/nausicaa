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
  (export
    unsetenv putenv putenv*
    (rename (platform:clearenv		clearenv)))
  (import (rnrs)
    (receive)
    (only (compensations)
	  with-compensations)
    (only (foreign cstrings)
	  string->cstring/c
	  string->cstring)
    (only (strings)
	  string-index)
    (only (foreign ffi peekers-and-pokers)
	  pointer-set-c-signed-char!)
    (only (foreign errno)
	  raise-errno-error)
    (prefix (foreign glibc environment platform)
	    platform:)
    (only (foreign memory)
	  malloc))

  (define (unsetenv name)
    (with-compensations
      (platform:unsetenv (string->cstring/c name))))

  (define (%normalise s)
    (if (symbol? s)
	(symbol->string s)
      s))

  (define (putenv assignment)
    (let ((s (%normalise assignment)))
      (if (string-index s #\=)
	  (platform:putenv (string->cstring s malloc))
	(assertion-violation 'putenv
	  "missing equal sign in process' environment variable assignment"
	  assignment))))

  (define (putenv* name value)
    (putenv (string-append (%normalise name) "=" (%normalise value))))

  )

;;; end of file
