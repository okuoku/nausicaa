;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the environment variables functions
;;;Date: Sun Nov 30, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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


(import (nausicaa)
  (checks)
  (foreign posix environment))

(check-set-mode! 'report-failed)
(display "*** testing POSIX generic\n")


(parametrise ((check-test-name 'env))

  (check
      (let ()
	(setenv 'CIAO 'fusilli #t)
	(getenv 'CIAO))
    => "fusilli")

  (check
      (let ()
	(setenv 'CIAO 'fusilli #t)
	(setenv 'CIAO 'spaghetti #f)
	(getenv 'CIAO))
    => "fusilli")

  (check
      (let ()
	(setenv 'SALUT 'fusilli #t)
	(setenv 'SALUT 'fusilli #f)
	(getenv 'SALUT))
    => "fusilli")

  #t)


;;;; done

(check-report)

;;; end of file
