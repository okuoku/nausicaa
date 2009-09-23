;;;
;;;Part of: Nausicaa libraries for R6RS
;;;Contents: compound library for foreign functions interfaces
;;;Date: Fri Dec 19, 2008
;;;
;;;Abstract
;;;
;;;
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

(library (foreign)
  (export


;;;; (foreign memory)

    ;;allocation
    system-free		platform-free		primitive-free
    system-malloc	system-calloc		system-realloc
    platform-malloc	platform-calloc		platform-realloc
    platform-malloc*	platform-calloc*	platform-realloc*
    primitive-malloc	primitive-calloc	primitive-realloc
    malloc		realloc			calloc

    primitive-malloc-function	primitive-calloc-function
    primitive-realloc-function	primitive-free-function

    memset		memmove			memcpy
    memcmp

    ;;pointers
    pointer?
    pointer-null			pointer-null?
    integer->pointer			pointer->integer
    pointer-diff			pointer-add

    pointer=?				pointer<>?
    pointer<?				pointer>?
    pointer<=?				pointer>=?

    ;;memory blocks
    make-memblock			memblock?
    memblock-pointer			memblock-size

    ;;buffers
    make-buffer
    buffer-pointer			buffer-size
    buffer?				buffer-used?
    buffer-full?			buffer-empty?
    buffer-used-size			buffer-used-size-set!
    buffer-free-size			buffer-consume-bytes!
    buffer-used-memblock		buffer-free-memblock
    buffer-pointer-to-free-bytes	buffer-incr-used-size!
    buffer-push-memblock!		buffer-pop-memblock!
    buffer-push-bytevector!		buffer-pop-bytevector!
    buffer-push-buffer!			buffer-pop-buffer!

    ;;cached memory blocks
    make-block-cache			make-caching-object-factory
    small-blocks-cache			page-blocks-cache
    memblocks-cache			buffers-cache

    ;;compensated allocations
    malloc/compensated			malloc/c
    calloc/compensated			calloc/c
    malloc-small/compensated		malloc-small/c
    malloc-page/compensated		malloc-page/c
    malloc-block/compensated		malloc-block/c
    malloc-memblock/compensated		malloc-memblock/c
    malloc-buffer/compensated		malloc-buffer/c

    ;;bytevector conversion functions
    bytevector->pointer			pointer->bytevector
    bytevector->memblock		memblock->bytevector

    ;;buffer allocation
    memory-buffer-pool
    primitive-malloc/buffer		malloc/buffer

    ;;reference counting
    malloc/refcount			malloc/rc
    pointer-acquire			pointer-release
    pointer-dismiss

    ;;peekers
    pointer-ref-c-int8			pointer-ref-c-uint8
    pointer-ref-c-int16			pointer-ref-c-uint16
    pointer-ref-c-int32			pointer-ref-c-uint32
    pointer-ref-c-int64			pointer-ref-c-uint64

    pointer-ref-c-float			pointer-ref-c-double
    pointer-ref-c-void*

    pointer-ref-c-signed-char		pointer-ref-c-unsigned-char
    pointer-ref-c-signed-short		pointer-ref-c-unsigned-short
    pointer-ref-c-signed-int		pointer-ref-c-unsigned-int
    pointer-ref-c-signed-long		pointer-ref-c-unsigned-long
    pointer-ref-c-signed-long-long	pointer-ref-c-unsigned-long-long

    pointer-ref-c-pointer

    ;;pokers
    pointer-set-c-int8!			pointer-set-c-uint8!
    pointer-set-c-int16!		pointer-set-c-uint16!
    pointer-set-c-int32!		pointer-set-c-uint32!
    pointer-set-c-int64!		pointer-set-c-uint64!

    pointer-set-c-float!		pointer-set-c-double!
    pointer-set-c-void*!

    pointer-set-c-signed-char!		pointer-set-c-unsigned-char!
    pointer-set-c-signed-short!		pointer-set-c-unsigned-short!
    pointer-set-c-signed-int!		pointer-set-c-unsigned-int!
    pointer-set-c-signed-long!		pointer-set-c-unsigned-long!
    pointer-set-c-signed-long-long!	pointer-set-c-unsigned-long-long!

    pointer-set-c-pointer!

    ;;array peekers
    array-ref-c-int8			array-ref-c-uint8
    array-ref-c-int16			array-ref-c-uint16
    array-ref-c-int32			array-ref-c-uint32
    array-ref-c-int64			array-ref-c-uint64

    array-ref-c-float			array-ref-c-double
    array-ref-c-void*

    array-ref-c-signed-char		array-ref-c-unsigned-char
    array-ref-c-signed-short		array-ref-c-unsigned-short
    array-ref-c-signed-int		array-ref-c-unsigned-int
    array-ref-c-signed-long		array-ref-c-unsigned-long
    array-ref-c-signed-long-long	array-ref-c-unsigned-long-long

    array-ref-c-pointer

    ;;array pokers
    array-set-c-int8!			array-set-c-uint8!
    array-set-c-int16!			array-set-c-uint16!
    array-set-c-int32!			array-set-c-uint32!
    array-set-c-int64!			array-set-c-uint64!

    array-set-c-float!			array-set-c-double!
    array-set-c-void*!

    array-set-c-signed-char!		array-set-c-unsigned-char!
    array-set-c-signed-short!		array-set-c-unsigned-short!
    array-set-c-signed-int!		array-set-c-unsigned-int!
    array-set-c-signed-long!		array-set-c-unsigned-long!
    array-set-c-signed-long-long!	array-set-c-unsigned-long-long!

    array-set-c-pointer!

    ;;conditions
    &out-of-memory			&memory-request
    make-out-of-memory-condition	make-memory-request-condition
    out-of-memory-condition?		memory-request-condition?
    condition-out-of-memory/number-of-bytes
    (rename (condition-out-of-memory/number-of-bytes
	     condition-memory-request/number-of-bytes))
    condition-memory-request/clean?
    raise-out-of-memory			raise-memory-request


;;;; (foreign ffi)

    ;;shared object access
    open-shared-object
    shared-object			self-shared-object

    ;;interface functions
    primitive-make-c-function		primitive-make-c-function/with-errno
    make-c-function			make-c-function/with-errno
    define-c-function			define-c-function/with-errno

    errno

    ;;foreign struct accessors
    define-c-struct-accessors		define-c-struct-field-pointer-getter
    define-c-struct-getter		define-c-struct-setter


;;;; (foreign ffi sizeof)

    sizeof-char			sizeof-short
    sizeof-int			sizeof-long
    sizeof-long-long		sizeof-float
    sizeof-double		sizeof-long-double
    sizeof-pointer

    alignof-char		alignof-short
    alignof-int			alignof-long
    alignof-long-long		alignof-float
    alignof-double		alignof-long-double
    alignof-pointer

    strideof-char		strideof-short
    strideof-int		strideof-long
    strideof-long-long		strideof-float
    strideof-double		strideof-long-double
    strideof-pointer

    sizeof-char-array		sizeof-short-array
    sizeof-int-array		sizeof-long-array
    sizeof-long-long-array	sizeof-float-array
    sizeof-double-array		sizeof-long-double-array
    sizeof-pointer-array

    valueof-char-max		valueof-char-min
    valueof-schar-max		valueof-schar-min	valueof-uchar-max
    valueof-shrt-max		valueof-shrt-min	valueof-ushrt-max
    valueof-int-max		valueof-int-min		valueof-uint-max
    valueof-long-max		valueof-long-min	valueof-ulong-max
    valueof-long-long-max	valueof-long-long-min	valueof-ulong-long-max
    valueof-wchar-max		valueof-ssize-max
    words-bigendian		on-64-bits-system	on-32-bits-system
    (rename (on-32-bits-system on-32-bits-systems)
	    (on-64-bits-system on-64-bits-systems))


;;;; (foreign cstring)

    ;;inspection
    strlen
    strcmp			strncmp

    ;;operations
    strcpy			strncpy
    strdup			strndup

    ;;conversion
    cstring->string		cstring->string/len
    string->cstring/c		string->cstring

    ;; null-terminated arrays of cstrings
    strings->argv		argv->strings
    argv-length


;;;; (foreign errno)

    strerror

    ;; conversion
    errno->symbol			errno->symbol/or-error
    symbol->errno			symbol->errno/or-error

    ;; condition
    &errno
    make-errno-condition		errno-condition?
    errno-numeric-value			errno-symbolic-value

    raise-errno-error

    ;; codes
    E2BIG		EACCES		EADDRINUSE
    EADDRNOTAVAIL	EADV		EAFNOSUPPORT
    EAGAIN		EALREADY	EBADE
    EBADF		EBADFD		EBADMSG
    EBADR		EBADRQC		EBADSLT
    EBFONT		EBUSY		ECANCELED
    ECHILD		ECHRNG		ECOMM
    ECONNABORTED	ECONNREFUSED	ECONNRESET
    EDEADLK		EDEADLOCK	EDESTADDRREQ
    EDOM		EDOTDOT		EDQUOT
    EEXIST		EFAULT		EFBIG
    EHOSTDOWN		EHOSTUNREACH	EIDRM
    EILSEQ		EINPROGRESS	EINTR
    EINVAL		EIO		EISCONN
    EISDIR		EISNAM		EKEYEXPIRED
    EKEYREJECTED	EKEYREVOKED	EL2HLT
    EL2NSYNC		EL3HLT		EL3RST
    ELIBACC		ELIBBAD		ELIBEXEC
    ELIBMAX		ELIBSCN		ELNRNG
    ELOOP		EMEDIUMTYPE	EMFILE
    EMLINK		EMSGSIZE	EMULTIHOP
    ENAMETOOLONG	ENAVAIL		ENETDOWN
    ENETRESET		ENETUNREACH	ENFILE
    ENOANO		ENOBUFS		ENOCSI
    ENODATA		ENODEV		ENOENT
    ENOEXEC		ENOKEY		ENOLCK
    ENOLINK		ENOMEDIUM	ENOMEM
    ENOMSG		ENONET		ENOPKG
    ENOPROTOOPT		ENOSPC		ENOSR
    ENOSTR		ENOSYS		ENOTBLK
    ENOTCONN		ENOTDIR		ENOTEMPTY
    ENOTNAM		ENOTRECOVERABLE	ENOTSOCK
    ENOTTY		ENOTUNIQ	ENXIO
    EOPNOTSUPP		EOVERFLOW	EOWNERDEAD
    EPERM		EPFNOSUPPORT	EPIPE
    EPROTO		EPROTONOSUPPORT	EPROTOTYPE
    ERANGE		EREMCHG		EREMOTE
    EREMOTEIO		ERESTART	EROFS
    ESHUTDOWN		ESOCKTNOSUPPORT	ESPIPE
    ESRCH		ESRMNT		ESTALE
    ESTRPIPE		ETIME		ETIMEDOUT
    ETOOMANYREFS	ETXTBSY		EUCLEAN
    EUNATCH		EUSERS		EWOULDBLOCK
    EXDEV		EXFULL


;;;; done

    )
  (import (rnrs)
    (foreign memory)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign cstring)
    (foreign errno)))

;;; end of file
