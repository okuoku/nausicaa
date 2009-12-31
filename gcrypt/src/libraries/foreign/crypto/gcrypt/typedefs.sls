;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Gcrypt
;;;Contents: type definitions
;;;Date: Mon Dec 28, 2009
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


(library (foreign crypto gcrypt typedefs)
  (export

    gcry-symmetric-handle		gcry-symmetric-handle?
    pointer->gcry-symmetric-handle	gcry-symmetric-handle->pointer

    gcry-md-handle			gcry-md-handle?
    pointer->gcry-md-handle		gcry-md-handle->pointer

    )
  (import (rnrs)
    (foreign crypto gcrypt sizeof))


  (define-record-type (gcry-symmetric-handle pointer->gcry-symmetric-handle gcry-symmetric-handle?)
    (nongenerative nausicaa:gcry:symmetric-handle)
    (fields (immutable pointer gcry-symmetric-handle->pointer)))

  (define-record-type (gcry-md-handle pointer->gcry-md-handle gcry-md-handle?)
    (nongenerative nausicaa:gcry:md-handle)
    (fields (immutable pointer gcry-md-handle->pointer)))

  )

;;; end of file
