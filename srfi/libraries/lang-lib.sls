;;;
;;;Part of: Nausicaa/SRFI
;;;Contents: SRFI language extensions library
;;;Date: Wed Dec 31, 2008
;;;
;;;Abstract
;;;
;;;	Stub library for language extension SRFIs.
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

(library (lang-lib)
  (export
    and-let*
    cut cute
    make-parameter parameterize
    (rename (cond general-cond)
	    (cond srfi:cond))
    receive
    (rename (rec recursion)))
  (import
      (srfi and-let-star)
    (srfi cut)
    (srfi general-cond)
    (srfi parameters)
    (srfi rec)
    (srfi receive)))

;;; end of file
