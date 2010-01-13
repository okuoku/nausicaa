;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Nettle
;;;Contents: load foreign shared library
;;;Date: Thu Jan  7, 2010
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


(library (foreign crypto hogweed shared-object)
  (export hogweed-shared-object)
  (import (rnrs)
    (foreign ffi)
    (foreign crypto nettle sizeof))
  ;;Explicitly loading nettle here prevents a "dl" problem with Hogweed:
  ;;"libhogweed"  is not linked  to "libnettle"  in Nettle  version 2.0;
  ;;notice that loading "(foreign crypto nettle shared-object)" does not
  ;;solve the problem.  The problem may  be fixed in the next release of
  ;;Nettle.
  ;;
  (define-shared-object nettle-shared-object
    NETTLE_SHARED_OBJECT)
  (define-shared-object hogweed-shared-object
    HOGWEED_SHARED_OBJECT))

;;; end of file
