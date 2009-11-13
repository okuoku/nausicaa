;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: record type definitions for (foreign memory membuffers)
;;;Date: Tue Sep 29, 2009
;;;
;;;Abstract
;;;
;;;	Export  the definitions  of  the record  types  required by  the
;;;	(foreign memory membuffers) library.  They are in an independent
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


(library (foreign memory membuffers types)
  (export

    <buffer>			<buffer-rtd>
    make-<buffer>		<buffer>?
    <buffer>-pointer-used	<buffer>-pointer-used-set!
    <buffer>-pointer-free	<buffer>-pointer-free-set!
    (rename (<memblock>-pointer	<buffer>-pointer)
	    (<memblock>-size	<buffer>-size))

    <membuffer>			<membuffer-rtd>
    make-<membuffer>		<membuffer>?
    <membuffer>-cache)
  (import (rnrs)
    (foreign memory memblocks)
    (queues))
  (define-record-type <buffer>
    (parent <memblock>)
    (fields (mutable pointer-used)	;pointer to first used byte
	    (mutable pointer-free)))	;pointer to first free byte
  (define-record-type <membuffer>
    (parent <queue>)
    (fields (immutable cache)))
  (define <buffer-rtd>
    (record-type-descriptor <buffer>))
  (define <membuffer-rtd>
    (record-type-descriptor <membuffer>)))

;;; end of file
