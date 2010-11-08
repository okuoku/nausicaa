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
  (export lookup-identifier-property id-hash $property-tables)
  (import (rnrs))


(define (id-hash id)
  (assert (identifier? id))
  (symbol-hash (syntax->datum id)))

(define $property-tables
  (make-hashtable id-hash free-identifier=?))

(define (lookup-identifier-property subject key)
  (assert (identifier? subject))
  (assert (identifier? key))
  (let ((table (hashtable-ref $property-tables key #f)))
    (if table
	(hashtable-ref table subject #f)
      (assertion-violation 'lookup-identifier-property "unknown property key" key))))


;;;; done

)

;;; end of file
