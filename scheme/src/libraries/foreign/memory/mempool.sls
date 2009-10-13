;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: allocation from memory pool
;;;Date: Tue Oct 13, 2009
;;;
;;;Abstract
;;;
;;;
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


(library (foreign memory mempool)
  (export
    <mempool> make-mempool <mempool>? <mempool>-pointer-free
    <mempool>-pointer <mempool>-size
    memory-pool
    primitive-malloc/mempool	malloc/mempool)
  (import (rnrs)
    (parameters)
    (records)
    (begin0)
    (foreign memory conditions)
    (foreign memory pointers)
    (for (foreign memory mempool types) expand run)
    (for (foreign memory mempool extensions) expand))

  (define memory-pool
    (make-parameter #f
      (lambda (obj)
	(assert (or (not obj) (is-a? obj <mempool>)))
	obj)))

  (define (primitive-malloc/mempool number-of-bytes)
    (let ((pool (memory-pool)))
      (assert pool)
      (with-fields (((pointer-free free-size) <mempool*> pool))
	(and (< number-of-bytes free-size)
	     (begin0
		 pointer-free
	       (set! pointer-free (pointer-add pointer-free number-of-bytes)))))))

  (define (malloc/mempool number-of-bytes)
    (or (primitive-malloc/mempool number-of-bytes)
	(raise-out-of-memory 'malloc/mempool number-of-bytes))))

;;; end of file
