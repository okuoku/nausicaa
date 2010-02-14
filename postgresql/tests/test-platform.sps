;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/PostgreSQL
;;;Contents: platform library loading test
;;;Date: Sun Feb 14, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (compensations)
  (foreign memory)
  (foreign cstrings)
  (foreign databases postgresql sizeof)
  (foreign databases postgresql platform)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing platform library\n")


(parametrise ((check-test-name	'opening))

  (check
      (with-compensations
	(letrec ((conn (compensate
			   (PQconnectdb (string->cstring/c "dbname=nausicaa-test"))
			 (with
			  (PQfinish conn)))))
	  (if (not (= CONNECTION_OK (PQstatus conn)))
	      #f
	    (begin
	      #t))))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
