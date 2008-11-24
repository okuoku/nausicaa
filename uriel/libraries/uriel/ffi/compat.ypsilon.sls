;;;
;;;Part of: Uriel libraries
;;;Contents: FFI compatibility layer for Ypsilon
;;;Date: Mon Nov 24, 2008
;;;Time-stamp: <2008-11-24 08:17:05 marco>
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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



;;;; setup

(library (uriel ffi compat)
  (export
    malloc free)
  (import (rnrs)
    (core)
    (ffi))


;;;; code

(define libc (open-shared-object ""))

(define malloc
  (c-function libc "C Library" char* malloc (int)))

(define free
  (c-function libc "C Library" void free (char*)))



;;;; done

)

;;; end of file
