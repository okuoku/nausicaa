;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the POSIX time and date functions
;;;Date: Mon Dec 22, 2008
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



;;;; setup

(import (r6rs)
  (uriel lang)
  (uriel test)
  (posix time)
  (posix time stub))

(check-set-mode! 'report-failed)




(parameterize ((testname 'clock))

  (check
      (flonum? (clock))
    => #t)

  (check
      (receive (result tms)
	  (times)
	(list (flonum? result)
	      (flonum? (struct-tms-tms_utime-ref tms))
	      (flonum? (struct-tms-tms_stime-ref tms))
	      (flonum? (struct-tms-tms_cutime-ref tms))
	      (flonum? (struct-tms-tms_cstime-ref tms))))
    => '(#t #t #t #t #t))

  )



;;;; done

(check-report)

;;; end of file
