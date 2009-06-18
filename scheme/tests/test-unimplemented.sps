;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for the &unimplemented condition
;;;Date: Mon Jan  5, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing unimplemented condition\n")



(parameterize ((debugging	#f))

  (check
      (guard (exc (else
		   (list (who-condition? exc)
			 (condition-who exc)
			 (unimplemented-condition? exc)
			 )))
	(raise-unimplemented-error 'woppa))
    => '(#t woppa #t))

  (check
      (guard (exc (else
		   (list (who-condition? exc)
			 (message-condition? exc)
			 (condition-who exc)
			 (unimplemented-condition? exc)
			 )))
	(raise-unimplemented-error 'woppa))
    => '(#t #t woppa #t))

  )


;;;; done

(check-report)

;;; end of file
