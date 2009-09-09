;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (records)
;;;Date: Wed Sep  9, 2009
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
  (records)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing records\n")


(parametrise ((check-test-name 'class-precedence-list))

  (let ()
    (define-record-type <alpha>)
    (define-record-type <beta>
      (parent <alpha>))
    (define-record-type <gamma>
      (parent <beta>))

    (check
	(record-precedence-list (record-type-descriptor <alpha>))
      => (list (record-type-descriptor <alpha>)))

    (check
	(record-precedence-list (record-type-descriptor <beta>))
      => (list (record-type-descriptor <beta>)
	       (record-type-descriptor <alpha>)))

    (check
	(record-precedence-list (record-type-descriptor <gamma>))
      => (list (record-type-descriptor <gamma>)
	       (record-type-descriptor <beta>)
	       (record-type-descriptor <alpha>)))
    #t)

  (let ()
    (define-record-type <alpha>)
    (define-record-type <beta>
      (parent <alpha>))
    (define-record-type <gamma>
      (parent <beta>))

    (check
	(record-precedence-list* <alpha>)
      => (list (record-type-descriptor <alpha>)))

    (check
	(record-precedence-list* <beta>)
      => (list (record-type-descriptor <beta>)
	       (record-type-descriptor <alpha>)))

    (check
	(record-precedence-list* <gamma>)
      => (list (record-type-descriptor <gamma>)
	       (record-type-descriptor <beta>)
	       (record-type-descriptor <alpha>)))

    #t)

  #t)


;;;; done

(check-report)

;;; end of file
