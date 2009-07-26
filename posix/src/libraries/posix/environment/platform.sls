;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to platform functions for access to environment variables
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


#!r6rs
(library (posix environment platform)
  (export
    platform-getenv
    platform-setenv)
  (import (rnrs)
    (foreign ffi)
    (posix sizeof))

  (define dummy
    (shared-object self-shared-object))


;;;; code

(define-c-function platform-setenv
  (int setenv (char* char* int)))

(define-c-function platform-getenv
  (char* getenv (char*)))



;;;; done

)

;;; end of file
