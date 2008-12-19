;;;
;;;Part of: Uriel libraries for R6RS Scheme
;;;Contents: implementation of VOID
;;;Date: Sun Nov 30, 2008
;;;Time-stamp: <2008-12-19 07:27:18 marco>
;;;
;;;Abstract
;;;
;;;	Ikarus Scheme provides the VOID function which returns the value
;;;	returned by  forms that do  not return a meaningful  value, like
;;;	SET!.  Not all implementations offers it, for example Ypsilon up
;;;	to revision 285 does not.
;;;
;;;       This library  attempts to provide a replacement.   There is no
;;;     guarantee  that   it  will  work   with  all  the   R6RS  Scheme
;;;     implementations.
;;;
;;;Copyright (C) 2008 Marco Maggi <marcomaggi@gna.org>
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

(library (uriel lang void)
  (export void)
  (import (rnrs))

  (define (void)
    (let ((a #f))
      (set! a 1))))

;;; end of file
