;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: direct wrappers for environment variables functions
;;;Date: Thu Jan  1, 2009
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


(library (foreign posix environment platform)
  (export getenv setenv environ)
  (import (rnrs)
    (only (foreign ffi)
	  shared-object		lookup-shared-object*
	  self-shared-object
	  define-c-function)
    (only (foreign ffi peekers-and-pokers)
	  pointer-ref-c-pointer))

  (define dummy
    (shared-object self-shared-object))

  (define-c-function setenv
    (int setenv (char* char* int)))

  (define-c-function getenv
    (char* getenv (char*)))

  (define (environ)
    (pointer-ref-c-pointer (lookup-shared-object* self-shared-object "__environ") 0)))

;;; end of file
