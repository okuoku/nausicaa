;;;
;;;Part of: Uriel libraries
;;;Contents: test for equal-hash implementation
;;;Date: Mon Dec  8, 2008
;;;Time-stamp: <2008-12-08 10:53:54 marco>
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

(import (except (rnrs) equal-hash)
  (uriel equal-hash)
  (uriel printing)
  (uriel test))

(check-set-mode! 'report-failed)

(check
    (integer? (equal-hash '#(1 2 3)))
  => #t)

(check-report)

;;; end of file
