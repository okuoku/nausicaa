;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/SQLite
;;;Contents: compensated constructors
;;;Date: Sat Nov 14, 2009
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


(library (foreign databases sqlite compensated)
  (export
    sqlite-open/c		sqlite-open-v2/c
    )
  (import (rnrs)
    (compensations)
    (foreign databases sqlite))


(define (sqlite-open/c pathname)
  (letrec ((session (compensate
			(sqlite-open pathname)
		      (with
		       (sqlite-close session)))))
    session))

(define sqlite-open-v2/c
  (case-lambda
   ((pathname flags)
    (sqlite-open-v2/c pathname flags #f))
   ((pathname flags name-of-vfs-module)
    (letrec ((session (compensate
			  (sqlite-open-v2 pathname flags name-of-vfs-module)
			(with
			 (sqlite-close session)))))
      session))))


;;;; done

)

;;; end of file
