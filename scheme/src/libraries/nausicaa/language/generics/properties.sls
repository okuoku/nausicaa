;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: expand time property values for generic functions
;;;Date: Fri Mar 25, 2011
;;;
;;;Abstract
;;;
;;;
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
(library (nausicaa language generics properties)
  (export
    make-generic		generic?
    generic-number-of-arguments
    generic-methods-arguments	generic-methods-arguments-set!)
  (import (rnrs records syntactic))


(define-record-type generic
  ;;Records of this type are  meant to be associated to generic function
  ;;identifiers using the :GENERIC-FUNCTION auxiliary keyword defined in
  ;;(nausicaa language property-identifiers).
  ;;
  (nongenerative nausicaa:language:generics:properties:generic)
  (opaque #t)
  (sealed #t)
  (fields (immutable number-of-arguments)
	  (mutable   methods-arguments)))


;;;; done

)

;;; end of file
