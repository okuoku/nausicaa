;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Template
;;;Contents: bindings to foreign functions
;;;Date:
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (foreign category template platform)
  (export template-shared-object)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign category template shared-object)
    (foreign category template sizeof))


(define-c-functions template-shared-object
  )


;;;; done

)

;;; end of file
