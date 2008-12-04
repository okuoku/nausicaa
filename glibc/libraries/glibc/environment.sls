;;;
;;;Part of: Nausicaa/Glibc
;;;Contents: environment functions
;;;Date: Sun Nov 30, 2008
;;;Time-stamp: <2008-12-04 11:35:21 marco>
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

(library (glibc environment)
  (export
    clearenv)
  (import (rnrs)
    (uriel ffi))

  ;;Look for other functions in the "(uriel posix)" library!!!
  (define-c-function clearenv
    (int clearenv (void))))

;;; end of file
