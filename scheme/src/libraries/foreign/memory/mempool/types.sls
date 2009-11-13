;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: record type definitions for (foreign memory mempool)
;;;Date: Tue Sep 29, 2009
;;;
;;;Abstract
;;;
;;;	Export  the definitions  of  the record  types  required by  the
;;;	(foreign memory  mempool) library.   They are in  an independent
;;;	library so that  they can be made available in  both the run and
;;;	expand phases.
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


(library (foreign memory mempool types)
  (export
    <mempool>			<mempool-rtd>
    make-mempool		<mempool>?
    <mempool>-pointer-free	<mempool>-pointer-free-set!
    (rename (<memblock>-pointer	<mempool>-pointer)
	    (<memblock>-size	<mempool>-size)))
  (import (rnrs)
    (foreign memory memblocks))

  (define-record-type <mempool>
    (parent <memblock>)
    (fields (mutable pointer-free))) ;pointer to the first free byte

  (define (make-mempool pointer size)
    (make-<mempool> pointer size pointer))

  (define <mempool-rtd>
    (record-type-descriptor <mempool>)))

;;; end of file
