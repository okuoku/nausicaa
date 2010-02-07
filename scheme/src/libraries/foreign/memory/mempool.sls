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
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
    <mempool>		<mempool-rtd>
    <mempool*>
    make-mempool	<mempool>?
    <mempool>-pointer-free
    <mempool>-pointer <mempool>-size
    memory-pool
    primitive-malloc/mempool	malloc/mempool)
  (import (rnrs)
    (parameters)
    (begin0)
    (foreign memory conditions)
    (foreign ffi pointers)
    (for (foreign memory mempool types) expand run)
    (for (foreign memory mempool extensions) expand run))

  (define memory-pool
    (make-parameter #f
      (lambda (obj)
	(assert (or (not obj) (<mempool>? obj)))
	obj)))

  (define (primitive-malloc/mempool number-of-bytes)
    (let ((pool (memory-pool)))
      (assert pool)
      (let-syntax ((pointer-free	(identifier-syntax
					 (_ (<mempool>-pointer-free pool))
					 ((set! _ ?value)
					  (<mempool>-pointer-free-set! pool ?value))))
		   (free-size		(identifier-syntax
					 (%mempool-free-size pool))))
	(and (<= number-of-bytes free-size)
	     (begin0
		 pointer-free
	       (pointer-incr! pointer-free number-of-bytes))))))

  (define (malloc/mempool number-of-bytes)
    (or (primitive-malloc/mempool number-of-bytes)
	(raise-out-of-memory 'malloc/mempool number-of-bytes))))

;;; end of file
