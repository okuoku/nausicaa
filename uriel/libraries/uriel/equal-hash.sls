;;;
;;;Part of: Uriel libraries for R6RS Scheme
;;;Contents: EQUAL-HASH implementation
;;;Date: Sun Nov 30, 2008
;;;Time-stamp: <2008-11-30 07:17:09 marco>
;;;
;;;Abstract
;;;
;;;	Implementation   of   the   EQUAL-HASH   function   for   Scheme
;;;	implementations that do not provide it (Ikarus).
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
  (import (rnrs))

  (define (equal-hash obj)
    (string-hash
     (call-with-string-output-port
	 (lambda () (write obj))))))

;;; end of file
