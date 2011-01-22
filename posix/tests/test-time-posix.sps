;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the POSIX time and date functions
;;;Date: Mon Dec 22, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (nausicaa checks)
  (nausicaa posix typedefs)
  (prefix (nausicaa posix time) px.))

(check-set-mode! 'report-failed)
(display "*** testing POSIX time\n")


(parametrise ((check-test-name 'clock))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in clock" E))
    (lambda ()


      (check
	  (flonum? (px.clock))
	=> #t)

      (check
	  (receive (result tms)
	      (px.times)
	    (list (flonum? result)
		  (flonum? (<tms>-utime tms))
		  (flonum? (<tms>-stime tms))
		  (flonum? (<tms>-cutime tms))
		  (flonum? (<tms>-cstime tms))))
	=> '(#t #t #t #t #t))

      #t)))


(parametrise ((check-test-name 'simple-calendar))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in simple calendar" E))
    (lambda ()

      (check
	  (flonum? (px.time))
	=> #t)

      #t)))


;;;; done

(check-report)

;;; end of file
