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


#!r6rs
(import (nausicaa)
  (nausicaa checks)
  (prefix (nausicaa ffi memory) mem.)
  (nausicaa posix sizeof)
  (prefix (nausicaa posix time) px.))

(check-set-mode! 'report-failed)
(display "*** testing POSIX time\n")


(parametrise ((check-test-name 'clock))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in clock" E))
    (lambda ()


      (check
	  (exact? (px.clock))
	=> #t)

      ;;This tests the type of values  in the fields of the <tms> mirror
      ;;class; the values are produced by functions in the stub library.
      (check
	  (receive (result (tms <tms>))
	      (px.times)
	    (list (exact? result)
		  (exact? tms.tms_utime)
		  (exact? tms.tms_stime)
		  (exact? tms.tms_cutime)
		  (exact? tms.tms_cstime)))
	=> '(#t #t #t #t #t))

      #t)))


(parametrise ((check-test-name 'simple-calendar))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in simple calendar" E))
    (lambda ()

      (check
	  (exact? (px.time))
	=> #t)

      #t)))


;;;; done

(check-report)

;;; end of file
