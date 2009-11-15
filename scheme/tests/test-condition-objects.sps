;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (conditions)
;;;Date: Sun Nov 15, 2009
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
  (conditions)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing conditions\n")


(parameterize ((check-test-name	'unimplemented))

  (check
      (guard (E ((unimplemented-condition? E)
		 (list (who-condition? E)
		       (condition-who E)))
		(else #f))
	(raise-unimplemented-error 'woppa))
    => '(#t woppa))

  #t)


(parameterize ((check-test-name	'wrong-num-args))

  (check
      (guard (E ((wrong-num-args-condition? E)
		 (list (condition-who E)
		       (condition-message E)
		       (condition-wrong-num-args-procname E)
		       (condition-expected-arguments-number E)
		       (condition-given-arguments-number E)))
		(else #f))
	(raise-wrong-num-args 'woppa "hey!" 'the-proc 5 10))
    => '(woppa "hey!" the-proc 5 10))

  #t)


;;;; done

(check-report)

;;; end of file
