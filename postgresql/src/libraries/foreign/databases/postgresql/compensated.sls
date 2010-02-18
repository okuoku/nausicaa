;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/PostgreSQL
;;;Contents: compensated constructors
;;;Date: Fri Feb 12, 2010
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


(library (foreign databases postgresql compensated)
  (export
    connect-db/c
    exec/c)
  (import (rnrs)
    (compensations)
    (prefix (foreign databases postgresql) pg:))


(define (connect-db/c info)
  (letrec ((conn (compensate
		     (pg:connect-db info)
		   (with
		    (pg:connect-finish conn)))))
    conn))

(define (exec/c conn query)
  (letrec ((result (compensate
		       (pg:exec conn query)
		     (with
		      (pg:clear-result result)))))
    result))



;;;; done

)

;;; end of file
