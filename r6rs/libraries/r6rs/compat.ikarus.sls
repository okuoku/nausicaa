;;;
;;;Part of: Uriel libraries
;;;Contents: stub library to make Ikarus R6RS compliant
;;;Date: Tue Dec  9, 2008
;;;Time-stamp: <2008-12-09 17:00:52 marco>
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

(library (r6rs compat)
  (export
    equal-hash)
  (import (except (rnrs) equal-hash))
  (define (equal-hash obj)
    (string-hash
     (call-with-string-output-port
 	 (lambda (port) (write obj port))))))

;;; end of file
