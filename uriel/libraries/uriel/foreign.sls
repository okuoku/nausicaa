;;;
;;;Part of: Uriel libraries for R6RS
;;;Contents: compound library for foreign functions interfaces
;;;Date: Fri Dec 19, 2008
;;;Time-stamp: <2009-01-04 09:29:22 marco>
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

(library (uriel foreign)
  (export


;;;; (uriel memory)

    ;;allocation
    platform-free	primitive-free		primitive-free-function
    platform-malloc	primitive-malloc	primitive-malloc-function
    platform-calloc	primitive-calloc	primitive-calloc-function
    platform-realloc    primitive-realloc	primitive-realloc-function

    malloc		realloc			calloc
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
    primitive-buffer-malloc		buffer-malloc

    ;;reference counting
    malloc/refcount			malloc/rc
    pointer-acquire			pointer-release
    pointer-dismiss

    ;;peekers
    pointer-ref-c-signed-char		pointer-ref-c-unsigned-char
    pointer-ref-c-signed-short		pointer-ref-c-unsigned-short
    pointer-ref-c-signed-int		pointer-ref-c-unsigned-int
    pointer-ref-c-signed-long		pointer-ref-c-unsigned-long
    pointer-ref-c-signed-long-long	pointer-ref-c-unsigned-long-long
    pointer-ref-c-float			pointer-ref-c-double
    pointer-ref-c-pointer

    peek-signed-char			peek-unsigned-char
    peek-signed-short			peek-unsigned-short
    peek-signed-int			peek-unsigned-int
    peek-signed-long			peek-unsigned-long
    peek-signed-long-long		peek-unsigned-long-long
    peek-float				peek-double
    peek-pointer

    ;;pokers
    pointer-set-c-char!			pointer-set-c-short!
    pointer-set-c-int!			pointer-set-c-long!
    pointer-set-c-long-long!		pointer-set-c-float!
    pointer-set-c-double!		pointer-set-c-pointer!

    poke-char!				poke-short!
    poke-int!				poke-long!
    poke-long-long!			poke-float!
    poke-double!			poke-pointer!

    ;;array peekers
    peek-array-signed-char		peek-array-unsigned-char
    peek-array-signed-short		peek-array-unsigned-short
    peek-array-signed-int		peek-array-unsigned-int
    peek-array-signed-long		peek-array-unsigned-long
    peek-array-signed-long-long		peek-array-unsigned-long-long
    peek-array-float			peek-array-double
    peek-array-pointer

    ;;array pokers
    poke-array-char!
    poke-array-short!			poke-array-int!
    poke-array-long!			poke-array-long-long!
    poke-array-float!			poke-array-double!
    poke-array-pointer!

    ;;conditions
    &out-of-memory
    make-out-of-memory-condition	out-of-memory-condition?
    out-of-memory-number-of-bytes	raise-out-of-memory


;;;; (uriel ffi)

    ;;shared object access
    open-shared-object
    shared-object			self-shared-object

    ;;interface functions
    primitive-make-c-function		primitive-make-c-function/with-errno
    make-c-function			make-c-function/with-errno
    define-c-function			define-c-function/with-errno

    ;;foreign struct accessors
    define-c-struct-accessors		define-c-struct-field-pointer-getter
    define-c-struct-getter		define-c-struct-setter


;;;; (uriel ffi sizeof)

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



;;;; (uriel cstring)

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


;;;; (uriel errno)

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
  (import (r6rs)
    (uriel memory)
    (uriel ffi)
    (uriel ffi sizeof)
    (uriel cstring)
    (uriel errno)))

;;; end of file
