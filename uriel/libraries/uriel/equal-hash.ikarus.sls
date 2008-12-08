;;;
;;;Part of: Uriel libraries for R6RS Scheme
;;;Contents: EQUAL-HASH implementation for Ikarus Scheme
;;;Date: Sun Nov 30, 2008
;;;Time-stamp: <2008-12-08 10:56:56 marco>
;;;
;;;Abstract
;;;
;;;	This library is loaded by  Ikarus Scheme, which at revision 1700
;;;	has no EQUAL-HASH implementation.
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

(library (uriel equal-hash)
  (export equal-hash)
  (import (except (rnrs) equal-hash))
  (define (equal-hash obj)
    (string-hash
     (call-with-string-output-port
 	 (lambda (port) (write obj port))))))

;;; end of file
