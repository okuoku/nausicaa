;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/BLAS
;;;Contents: load foreign shared library
;;;Date: Mon Feb  1, 2010
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


(library (foreign math blas shared-object)
  (export cblas-shared-object)
  (import (rnrs)
    (foreign ffi)
    (foreign math blas sizeof))
  (define-shared-object blas-shared-object
    BLAS_SHARED_OBJECT)
  (define-shared-object cblas-shared-object
    CBLAS_SHARED_OBJECT))

;;; end of file
