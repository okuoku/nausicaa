;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: NOS record types for special lists
;;;Date: Fri Sep 11, 2009
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


(library (nos lists)
  (export
    <circular-list>	<circular-list>?
    <dotted-list>	<dotted-list>?
    list-record-type-of)
  (import (rnrs)
    (nos)
    (lists))

  (define-builtin <circular-list>	<pair>		circular-list?)
  (define-builtin <dotted-list>		<pair>		dotted-list?)

  (define (list-record-type-of obj)
    (and (pair? obj)
	 (cond ((circular-list?	obj)	(record-type-descriptor <circular-list>))
	       ((dotted-list?	obj)	(record-type-descriptor <dotted-list>))
	       (else #f)))))

;;; end of file
