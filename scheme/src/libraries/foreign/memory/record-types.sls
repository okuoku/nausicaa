;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: record type definitions for (memory)
;;;Date: Tue Sep 29, 2009
;;;
;;;Abstract
;;;
;;;	Export  the definitions  of  the record  types  required by  the
;;;	(memory) library.   They are in  an independent library  so that
;;;	they can be made available in both the run and expand phases.
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


(library (foreign memory record-types)
  (export <membuffer> <memblock>)
  (import (rnrs))

  (define-record-type <memblock>
    (fields (immutable pointer)	;pointer to the allocated block
	    (immutable size)))	;number of allocated bytes

  (define-record-type <membuffer>
    (parent <memblock>)
    (fields (mutable first-used) ;pointer to the first used byte
	    (mutable used-size))) ;number of used bytes



;;;; code



;;;; done

)

;;; end of file
