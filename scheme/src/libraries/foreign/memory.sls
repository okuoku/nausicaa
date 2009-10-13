;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: low level memory functions
;;;Date: Tue Dec 16, 2008
;;;
;;;Abstract
;;;
;;;	Notice that this library avoids using (foreign ffi).
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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


(library (foreign memory)
  (export

    <memblock> <memblock-rtd>
    <memblock>? <memblock>-pointer <memblock>-size

    ;; bindings from (foreign memory conditions)
    &out-of-memory			&memory-request
    make-out-of-memory-condition	make-memory-request-condition
    out-of-memory-condition?		memory-request-condition?
    condition-out-of-memory/number-of-bytes
    condition-memory-request/number-of-bytes
    condition-memory-request/clean?
    raise-out-of-memory			raise-memory-request

    ;; bindings from (foreign memory alloc)
    system-free		platform-free		primitive-free
    system-malloc	system-calloc		system-realloc
    platform-malloc	platform-calloc		platform-realloc
    platform-malloc*	platform-calloc*	platform-realloc*
    primitive-malloc	primitive-calloc	primitive-realloc
    malloc		realloc			calloc

    primitive-malloc-function	primitive-calloc-function
    primitive-realloc-function	primitive-free-function

    ;; bindings from (foreign memory operations)
    memset		memmove		memcpy		memcmp

    ;; bindings from (foreign memory pointers)
    pointer?
    pointer-null		pointer-null?
    integer->pointer		pointer->integer
    pointer-diff		pointer-add
    pointer=?			pointer<>?
    pointer<?			pointer>?
    pointer<=?			pointer>=?

    ;; bindings from (foreign memory caches)
    make-block-cache		make-caching-object-factory
    small-blocks-cache		page-blocks-cache
    small-blocks-size		page-blocks-size
    memblocks-cache

    ;; bindings from (foreign memory bytevectors)
    bytevector->pointer		pointer->bytevector
    bytevector->memblock	memblock->bytevector

;;; --------------------------------------------------------------------
;;; bindings from (foreign memory peekers-and-pokers)

    ;;peekers
    pointer-ref-c-int8			pointer-ref-c-uint8
    pointer-ref-c-int16			pointer-ref-c-uint16
    pointer-ref-c-int32			pointer-ref-c-uint32
    pointer-ref-c-int64			pointer-ref-c-uint64
    pointer-ref-c-float			pointer-ref-c-double
    pointer-ref-c-signed-char		pointer-ref-c-unsigned-char
    pointer-ref-c-signed-short		pointer-ref-c-unsigned-short
    pointer-ref-c-signed-int		pointer-ref-c-unsigned-int
    pointer-ref-c-signed-long		pointer-ref-c-unsigned-long
    pointer-ref-c-signed-long-long	pointer-ref-c-unsigned-long-long
    pointer-ref-c-pointer		pointer-ref-c-void*

    ;;pokers
    pointer-set-c-int8!			pointer-set-c-uint8!
    pointer-set-c-int16!		pointer-set-c-uint16!
    pointer-set-c-int32!		pointer-set-c-uint32!
    pointer-set-c-int64!		pointer-set-c-uint64!
    pointer-set-c-float!		pointer-set-c-double!
    pointer-set-c-signed-char!		pointer-set-c-unsigned-char!
    pointer-set-c-signed-short!		pointer-set-c-unsigned-short!
    pointer-set-c-signed-int!		pointer-set-c-unsigned-int!
    pointer-set-c-signed-long!		pointer-set-c-unsigned-long!
    pointer-set-c-signed-long-long!	pointer-set-c-unsigned-long-long!
    pointer-set-c-pointer!		pointer-set-c-void*!

    ;;array peekers
    array-ref-c-int8			array-ref-c-uint8
    array-ref-c-int16			array-ref-c-uint16
    array-ref-c-int32			array-ref-c-uint32
    array-ref-c-int64			array-ref-c-uint64
    array-ref-c-float			array-ref-c-double
    array-ref-c-signed-char		array-ref-c-unsigned-char
    array-ref-c-signed-short		array-ref-c-unsigned-short
    array-ref-c-signed-int		array-ref-c-unsigned-int
    array-ref-c-signed-long		array-ref-c-unsigned-long
    array-ref-c-signed-long-long	array-ref-c-unsigned-long-long
    array-ref-c-void*			array-ref-c-pointer

    ;;array pokers
    array-set-c-int8!			array-set-c-uint8!
    array-set-c-int16!			array-set-c-uint16!
    array-set-c-int32!			array-set-c-uint32!
    array-set-c-int64!			array-set-c-uint64!
    array-set-c-float!			array-set-c-double!
    array-set-c-signed-char!		array-set-c-unsigned-char!
    array-set-c-signed-short!		array-set-c-unsigned-short!
    array-set-c-signed-int!		array-set-c-unsigned-int!
    array-set-c-signed-long!		array-set-c-unsigned-long!
    array-set-c-signed-long-long!	array-set-c-unsigned-long-long!
    array-set-c-void*!			array-set-c-pointer!

    peek-signed-char			peek-unsigned-char
    peek-signed-short			peek-unsigned-short
    peek-signed-int			peek-unsigned-int
    peek-signed-long			peek-unsigned-long
    peek-signed-long-long		peek-unsigned-long-long
    peek-float				peek-double
    peek-pointer			peek-void*

    peek-int8				peek-uint8
    peek-int16				peek-uint16
    peek-int32				peek-uint32
    peek-int64				peek-uint64

    poke-signed-char!			poke-unsigned-char!
    poke-signed-short!			poke-unsigned-short!
    poke-signed-int!			poke-unsigned-int!
    poke-signed-long!			poke-unsigned-long!
    poke-signed-long-long!		poke-unsigned-long-long!
    poke-float!				poke-double!
    poke-pointer!			poke-void*!

    poke-int8!				poke-uint8!
    poke-int16!				poke-uint16!
    poke-int32!				poke-uint32!
    poke-int64!				poke-uint64!

    peek-array-signed-char		peek-array-unsigned-char
    peek-array-signed-short		peek-array-unsigned-short
    peek-array-signed-int		peek-array-unsigned-int
    peek-array-signed-long		peek-array-unsigned-long
    peek-array-signed-long-long		peek-array-unsigned-long-long
    peek-array-float			peek-array-double
    peek-array-pointer			peek-array-void*

    peek-array-int8			peek-array-uint8
    peek-array-int16			peek-array-uint16
    peek-array-int32			peek-array-uint32
    peek-array-int64			peek-array-uint64

    poke-array-signed-char!		poke-array-unsigned-char!
    poke-array-signed-short!		poke-array-unsigned-short!
    poke-array-signed-int!		poke-array-unsigned-int!
    poke-array-signed-long!		poke-array-unsigned-long!
    poke-array-signed-long-long!	poke-array-unsigned-long-long!
    poke-array-float!			poke-array-double!
    poke-array-pointer!			poke-array-void*!

    poke-array-int8!			poke-array-uint8!
    poke-array-int16!			poke-array-uint16!
    poke-array-int32!			poke-array-uint32!
    poke-array-int64!			poke-array-uint64!)
  (import (nausicaa)
    (foreign memory memblocks)
    (foreign memory conditions)
    (foreign memory pointers)
    (foreign memory alloc)
    (foreign memory operations)
    (foreign memory caches)
    (foreign memory bytevectors)
    (foreign memory peekers-and-pokers)))

;;; end of file
