;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: definitions of identifiers to be used as property keywords
;;;Date: Fri Mar 25, 2011
;;;
;;;Abstract
;;;
;;;	This library exports auxiliary  syntax identifiers to be used as
;;;	property   keywords   by    the   library   (nausicaa   language
;;;	identifier-properties).
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa language property-keywords)
  (export generic-function)
  (import (rnrs base))
  (define-syntax generic-function (syntax-rules ()))
  )

;;; end of file
