;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: expand-time identifier properties
;;;Date: Mon Nov  8, 2010
;;;
;;;Abstract
;;;
;;;	The idea is taken from the compile-time values and properties of
;;;	Chez  Scheme  (see  Chez's  manual, Section  11.4  "Compile-time
;;;	Values and Properties").  No code comes from Chez.
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
(library (identifier-properties)
  (export define-identifier-property
	  lookup-identifier-property)
  (import (rnrs)
    (for (identifier-properties helpers) expand))


(define-syntax define-identifier-property
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?subject ?key ?value)
       (begin
	 (assert (identifier? #'?subject))
	 (assert (identifier? #'?key))
	 (let ((table (hashtable-ref $property-tables #'?key #f)))
	   (unless table
	     (set! table (make-hashtable id-hash free-identifier=?))
	     (hashtable-set! $property-tables #'?key table))
	   (hashtable-set! table #'?subject #'?value))
	 #'(define dummy))))))


;;;; done

)

;;; end of file
