;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: keyword values for Nausicaa Object System
;;;Date: Wed Aug 26, 2009
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


(library (nos keywords)
  (export
    :init-value		:init-thunk
    :accessor		:accessor-before	:accessor-after
    :mutator		:mutator-before		:mutator-after)
  (import (rnrs)
    (keywords))

  (define-keyword :init-value)
  (define-keyword :init-thunk)

  (define-keyword :mutator)
  (define-keyword :mutator-before)
  (define-keyword :mutator-after)

  (define-keyword :accessor)
  (define-keyword :accessor-before)
  (define-keyword :accessor-after)

  )

;;; end of file
