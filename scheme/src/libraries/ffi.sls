;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: foreign function interface extensions
;;;Date: Tue Nov 18, 2008
;;;
;;;Abstract
;;;
;;;	This is the core of the foreign functions interface.
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


(library (ffi)
  (export

;;; bindings from (ffi conditions)
    &library-name
    make-library-name-condition
    library-name-condition?
    condition-library-name

    &shared-object
    make-shared-object-condition
    shared-object-condition?
    condition-shared-object

    &foreign-symbol
    make-foreign-symbol-condition
    foreign-symbol-condition?
    condition-foreign-symbol

    &shared-object-opening-error
    make-shared-object-opening-error-condition
    shared-object-opening-error-condition?

    &shared-object-lookup-error
    make-shared-object-lookup-error-condition
    shared-object-lookup-error-condition?

    raise-shared-object-opening-error
    raise-shared-object-lookup-error

;;; bindings from (ffi primitives)
    shared-object?			libc-shared-object
    open-shared-object			lookup-shared-object
    make-c-function			make-c-function/with-errno
    make-c-callout			make-c-callout/with-errno
    make-c-callback			free-c-callback
    define-c-struct-accessor		define-c-struct-mutator
    define-c-struct-accessor-and-mutator
    define-c-struct-field-pointer-accessor

;;; bindings from (ffi utilities)
    define-shared-object
    define-c-functions			define-c-functions/with-errno
    define-c-callouts			define-c-callouts/with-errno
    make-c-function*			make-c-function/with-errno*
    make-c-callout*			make-c-callout/with-errno*
    make-c-callback*

;;; bindings from (ffi pointers)
    pointer?
    pointer-null			pointer-null?
    integer->pointer			pointer->integer
    pointer-diff			pointer-add
    pointer-incr!
    pointer=?				pointer<>?
    pointer<?				pointer>?
    pointer<=?				pointer>=?

;;; bindings from (ffi peekers-and-pokers)

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
    pointer-ref-c-size_t		pointer-ref-c-ssize_t

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
    pointer-set-c-size_t!		pointer-set-c-ssize_t!

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
    array-ref-c-size_t			array-ref-c-ssize_t

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
    array-set-c-size_t!			array-set-c-ssize_t!

    ;; array pointer getters
    array-ptr-c-int8			array-ptr-c-uint8
    array-ptr-c-int16			array-ptr-c-uint16
    array-ptr-c-int32			array-ptr-c-uint32
    array-ptr-c-int64			array-ptr-c-uint64
    array-ptr-c-float			array-ptr-c-double
    array-ptr-c-signed-char		array-ptr-c-unsigned-char
    array-ptr-c-signed-short		array-ptr-c-unsigned-short
    array-ptr-c-signed-int		array-ptr-c-unsigned-int
    array-ptr-c-signed-long		array-ptr-c-unsigned-long
    array-ptr-c-signed-long-long	array-ptr-c-unsigned-long-long
    array-ptr-c-void*			array-ptr-c-pointer
    array-ptr-c-size_t			array-ptr-c-ssize_t

    ;; aliases
    peek-signed-char			peek-unsigned-char
    peek-signed-short			peek-unsigned-short
    peek-signed-int			peek-unsigned-int
    peek-signed-long			peek-unsigned-long
    peek-signed-long-long		peek-unsigned-long-long
    peek-float				peek-double
    peek-pointer			peek-void*
    peek-ssize_t			peek-size_t

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
    poke-ssize_t!			poke-size_t!

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
    peek-array-ssize_t			peek-array-size_t

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
    poke-array-ssize_t!			poke-array-size_t!

    poke-array-int8!			poke-array-uint8!
    poke-array-int16!			poke-array-uint16!
    poke-array-int32!			poke-array-uint32!
    poke-array-int64!			poke-array-uint64!)
  (import (rnrs)
    (ffi conditions)
    (ffi pointers)
    (ffi peekers-and-pokers)
    (ffi primitives)
    (ffi utilities)))

;;; end of file
