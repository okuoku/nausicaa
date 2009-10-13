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
    memory-pool
    primitive-malloc/mempool	malloc/mempool)
  (import (rnrs)
    (parameters)
    (foreign memory conditions)
    (foreign memory pointers)
    (foreign memory alloc)
    (for (foreign memory mempool types) expand))

  (define memory-pool
    (make-parameter #f
      (lambda (obj)
	(unless (or (not obj) (is-a? obj <mempool>))
	  (assertion-violation 'memory-buffer-pool
	    "expected #f or memory pool as parameter value" obj))
	obj)))

  (define (primitive-malloc/mempool number-of-bytes)
    (let ((pool (memory-pool)))
      (assert pool)
      (with-record-fields ((next <mempool> pool))
	(with-virtual-fields ((free-size <mempool> pool))
	  (and (< number-of-bytes free-size)
	       (begin0
		   next
		 (set! next (pointer-add next number-of-bytes))))))))

  (define (malloc/mempool number-of-bytes)
    (or (primitive-malloc/mempool number-of-bytes)
	(raise-out-of-memory 'malloc/mempool number-of-bytes))))

;;; end of file
