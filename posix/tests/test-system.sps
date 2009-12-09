;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the system configuration inspection functions
;;;Date: Wed Dec  9, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (strings)
  (compensations)
  (deferred-exceptions)
  (foreign errno)
  (foreign posix sizeof)
  (foreign posix typedefs)
  (prefix (foreign posix system) posix:)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing system configuration inspection functions\n")


(parameterize ((check-test-name	'sysconf)
	       (debugging	#f))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in sysconf" E))
    (lambda ()

      (check
	  (integer? (posix:sysconf _SC_ARG_MAX))
	=> #t)

      (check
	  (integer? (posix:pathconf "/bin/ls" _PC_NAME_MAX))
	=> #t)

      (check
	  (posix:confstr _CS_PATH)
	=> "/bin:/usr/bin")

      #t)))


;;;; done

(check-report)

;;; end of file
