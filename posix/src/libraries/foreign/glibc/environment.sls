;;;
;;;Part of: Nausicaa/Glibc
;;;Contents: environment functions
;;;Date: Sun Nov 30, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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


(library (foreign glibc environment)
  (export
    unsetenv clearenv)
  (import (rnrs)
    (prefix (foreign glibc environment primitives)
	    primitive:)
    (only (foreign posix helpers)
	  define-primitive-parameter)
    (only (foreign ffi)
	  shared-object		self-shared-object
	  define-c-function))

  (define-primitive-parameter unsetenv-function primitive:unsetenv)
  (define-primitive-parameter clearenv-function primitive:clearenv)

  (define (unsetenv name)
    ((unsetenv-function) name))

  (define (clearenv)
    ((clearenv-function)))

  )

;;; end of file
