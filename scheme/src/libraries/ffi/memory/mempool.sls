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


(library (ffi memory mempool)
  (export
    <mempool>
    make-<mempool>		<mempool>?
    <mempool>-pointer-free	<mempool>-pointer-free-set!
    <mempool>-pointer
    <mempool>-size
    memory-pool
    primitive-malloc/mempool	malloc/mempool)
  (import (nausicaa)
    (ffi memory memblocks)
    (ffi memory conditions)
    (ffi pointers))


(define-class <mempool>
  (parent <memblock>)
  (nongenerative nausicaa:ffi:memory:mempool:<mempool>)
  (protocol (lambda (make-<memblock>)
	      (lambda (pointer size)
		((make-<memblock> pointer size #f) pointer))))
  (fields (mutable pointer-free))
		;pointer to the first free byte
  (virtual-fields (immutable free-size	%mempool-free-size)
		  (immutable pointer	<mempool>-pointer)
		  (immutable size	<mempool>-size)))

(define <mempool>-pointer	<memblock>-pointer)
(define <mempool>-size		<memblock>-size)

(define (%mempool-free-size (pool <mempool>))
  (- pool.size (pointer-diff pool.pointer-free pool.pointer)))

(define memory-pool
  (make-parameter #f
    (lambda (obj)
      (assert (or (not obj) (<mempool>? obj)))
      obj)))

(define (primitive-malloc/mempool number-of-bytes)
  (let-fields (((pool <mempool>) (memory-pool)))
    (assert pool)
    (and (<= number-of-bytes pool.free-size)
	 (begin0
	     pool.pointer-free
	   (pointer-incr! pool.pointer-free number-of-bytes)))))

(define (malloc/mempool number-of-bytes)
  (or (primitive-malloc/mempool number-of-bytes)
      (raise-out-of-memory 'malloc/mempool number-of-bytes)))


;;;; done

)

;;; end of file
