;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/MHD
;;;Contents: enumeration types
;;;Date: Thu Dec  3, 2009
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


(library (foreign net mhd enumerations)
  (export
    enum-mhd-flags		mhd-flags
    %mhd-flags-set->flags	%mhd-flags-set?
    )
  (import (rnrs)
    (foreign net mhd sizeof))


(define-enumeration enum-mhd-flags
  (NO_FLAG
   USE_DEBUG
   USE_SSL
   USE_THREAD_PER_CONNECTION
   USE_SELECT_INTERNALLY
   USE_IPv6
   USE_PEDANTIC_CHECKS)
  mhd-flags)

(define %mhd-flags-universe
  (enum-set-universe (mhd-flags)))

(define (%mhd-flags-set? set)
  (enum-set-subset? set %mhd-flags-universe))

(define (%mhd-flags-set->flags set)
  (fold-left (lambda (knil symbol)
	       (bitwise-ior knil (case symbol
				   ((NO_FLAG)			MHD_NO_FLAG)
				   ((USE_DEBUG)			MHD_USE_DEBUG)
				   ((USE_SSL)			MHD_USE_SSL)
				   ((USE_THREAD_PER_CONNECTION)	MHD_USE_THREAD_PER_CONNECTION)
				   ((USE_SELECT_INTERNALLY)	MHD_USE_SELECT_INTERNALLY)
				   ((USE_IPv6)			MHD_USE_IPv6)
				   ((USE_PEDANTIC_CHECKS)	MHD_USE_PEDANTIC_CHECKS))))
	     0
	     (enum-set->list set)))


;;;; done

)

;;; end of file
