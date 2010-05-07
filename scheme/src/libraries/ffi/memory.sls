;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: low level memory functions
;;;Date: Tue Dec 16, 2008
;;;
;;;Abstract
;;;
;;;	Notice that this library avoids using (ffi).
;;;
;;;Copyright (c) 2008-2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (ffi memory)
  (export

    ;; memory blocks
    <memblock>
    make-<memblock>		<memblock>?
    <memblock>-pointer		<memblock>-pointer-set!
    <memblock>-size		<memblock>-size-set!
    <memblock>-alloc-size	<memblock>-alloc-size-set!
    memblock-null
    memblock-shallow-clone	memblock-deep-clone
    memblock->string-hex	string-hex->memblock
    memblock-head		memblock-tail
    memblock-head?		memblock-tail?
    memblock&tail-head		memblock&head-tail

    ;; bindings from (ffi memory conditions)
    &out-of-memory			&memory-request
    make-out-of-memory-condition	make-memory-request-condition
    out-of-memory-condition?		memory-request-condition?
    condition-out-of-memory/number-of-bytes
    condition-memory-request/number-of-bytes
    condition-memory-request/clean?
    raise-out-of-memory			raise-memory-request

    ;; bindings from (ffi memory alloc)
    system-free		platform-free		primitive-free
    system-malloc	system-calloc		system-realloc
    platform-malloc	platform-calloc		platform-realloc
    platform-malloc*	platform-calloc*	platform-realloc*
    primitive-malloc	primitive-calloc	primitive-realloc
    malloc		realloc			calloc

    primitive-malloc-function	primitive-calloc-function
    primitive-realloc-function	primitive-free-function

    ;; bindings from (ffi memory operations)
    memset		memmove		memcpy		memcmp

    ;; bindings from (ffi pointers)
    pointer?
    pointer-null		pointer-null?
    integer->pointer		pointer->integer
    pointer-diff		pointer-add
    pointer-incr!
    pointer=?			pointer<>?
    pointer<?			pointer>?
    pointer<=?			pointer>=?

    ;; bindings from (ffi memory caches)
    make-block-cache		make-caching-object-factory
    small-blocks-cache		page-blocks-cache
    small-blocks-size		page-blocks-size
    memblocks-cache

    ;; bindings from (ffi memory bytevectors)
    bytevector->pointer		pointer->bytevector
    bytevector->memblock	memblock->bytevector

    ;; bindings from (ffi memory compensated)
    malloc/compensated		malloc/c
    calloc/compensated		calloc/c
    malloc-small/compensated	malloc-small/c
    malloc-page/compensated	malloc-page/c
    malloc-block/compensated	malloc-block/c
    malloc-memblock/compensated	malloc-memblock/c

;;; --------------------------------------------------------------------
;;; bindings from (ffi peekers-and-pokers)

    pointer-c-ref	pointer-c-set!
    array-c-ref		array-c-set!
    array-c-pointer-to)
  (import (nausicaa)
    (ffi memory alloc)
    (ffi memory bytevectors)
    (ffi memory caches)
    (ffi memory compensated)
    (ffi memory conditions)
    (ffi memory memblocks)
    (ffi memory operations)
    (ffi peekers-and-pokers)
    (ffi pointers)))

;;; end of file
