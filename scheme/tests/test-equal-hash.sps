;;;
;;;Part of: Uriel libraries
;;;Contents: test for equal-hash implementation
;;;Date: Mon Dec  8, 2008
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

(import (r6rs))

(define-syntax check
  (syntax-rules (=>)
    ((_ ?form => ?expected-result)
     (let ((result ?form))
       (if (equal? result ?expected-result)
	   (begin
	     (display "*** test success")
	     (newline))
	 (begin
	   (display "*** test FAILURE")
	   (newline)))))))

(check
    (integer? (equal-hash '#(1 2 3)))
  => #t)


;;; end of file
