;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: record utilities
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


#!r6rs
(library (records)
  (export
    record-precedence-list
    record-precedence-list*)
  (import (rnrs))

  (define-syntax record-precedence-list*
    (syntax-rules ()
      ((_ ?record-name)
       (record-precedence-list (record-type-descriptor ?record-name)))))

  (define (record-precedence-list rtd)
    (let loop ((cls `(,rtd))
	       (rtd (record-type-parent rtd)))
      (if rtd
	  (loop (cons rtd cls) (record-type-parent rtd))
	(reverse cls)))))

;;; end of file
