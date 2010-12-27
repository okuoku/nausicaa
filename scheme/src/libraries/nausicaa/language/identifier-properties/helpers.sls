;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: helpers for expand-time identifier properties
;;;Date: Mon Nov  8, 2010
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


#!r6rs
(library (identifier-properties helpers)
  (export lookup-identifier-property identifier-property-set!)
  (import (rnrs)
    (identifier-properties identifier-alists))

  ;;An alist  of alists  sucks in performance,  but with  identifiers as
  ;;keys we have no other choice.
  (define $table-of-property-tables '())

  (define lookup-identifier-property
    (case-lambda
     ((subject key)
      (lookup-identifier-property subject key #f))
     ((subject key default)
      (assert (identifier? subject))
      (assert (identifier? key))
      (let ((property-table (identifier-alist-ref $table-of-property-tables key #f)))
	(if property-table
	    (identifier-alist-ref property-table subject default)
	  default)))))

  (define (identifier-property-set! subject key value)
    (assert (identifier? subject))
    (assert (identifier? key))
    (let ((property-table (identifier-alist-ref $table-of-property-tables key #f)))
      (set! $table-of-property-tables
	    (if property-table
		(identifier-alist-replace key (identifier-alist-replace subject value property-table)
					  $table-of-property-tables)
	      (identifier-alist-cons key `((,subject . ,value))
				     $table-of-property-tables)))))
  )

;;; end of file
