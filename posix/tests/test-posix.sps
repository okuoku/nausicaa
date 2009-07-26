;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the basic POSIX interface
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
  (foreign)
  (checks)
  (posix environment))

(check-set-mode! 'report-failed)


(parameterize ((check-test-name 'env))

  (check
      (let ()
	(setenv 'CIAO 'pasta 1)
	(getenv 'CIAO))
    => "pasta")

  (check
      (let ()
	(setenv 'SALUT 'pasta 1)
	(setenv 'SALUT 'fusillo 0)
	(getenv 'CIAO))
    => "pasta")

  )



;;;; done

(check-report)

;;; end of file
