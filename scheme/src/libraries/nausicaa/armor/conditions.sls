;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: condition objects for ASCII armor libraries
;;;Date: Fri Feb  5, 2010
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
(library (nausicaa armor conditions)
  (export

    &armor-error
    make-armor-error-condition
    armor-error-condition?

    &armor-invalid-input-byte
    make-armor-invalid-input-byte-condition
    armor-invalid-input-byte-condition?

    &armor-invalid-input-length
    make-armor-invalid-input-length-condition
    armor-invalid-input-length-condition?

    &armor-invalid-padding
    make-armor-invalid-padding-condition
    armor-invalid-padding-condition?
    )
  (import (rnrs))


(define-condition-type &armor-error &error
  make-armor-error-condition
  armor-error-condition?)

(define-condition-type &armor-invalid-input-byte &armor-error
  make-armor-invalid-input-byte-condition
  armor-invalid-input-byte-condition?)

(define-condition-type &armor-invalid-input-length &armor-error
  make-armor-invalid-input-length-condition
  armor-invalid-input-length-condition?)

(define-condition-type &armor-invalid-padding &armor-error
  make-armor-invalid-padding-condition
  armor-invalid-padding-condition?)



;;;; done

)

;;; end of file
