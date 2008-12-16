;;;
;;;Part of: Uriel libraries for R6RS Scheme
;;;Contents: values of the errno constants
;;;Date: Mon Dec  1, 2008
;;;Time-stamp: <2008-12-16 16:39:27 marco>
;;;
;;;Abstract
;;;
;;;	Exports  one   binding  for  each  "errno"   constant,  and  two
;;;	conversion functions.   Not all the constants may  be defined on
;;;	your platform, if a symbol is not defined its value is #f.
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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


;;; --------------------------------------------------------------------
;;; Setup.
;;; --------------------------------------------------------------------

(library (uriel ffi errno)
  (export

    ;; conversion
    symbol->errno		errno->symbol
    symbol->errno/or-error	errno->symbol/or-error

    ;; condition
    (rename (make-errno-condition* make-errno-condition))
    &errno			errno-condition?
    errno-numeric-value		errno-symbolic-value

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
    EXDEV		EXFULL)
  (import (r6rs)
    (srfi lists)
    (uriel cstring))


;;; --------------------------------------------------------------------
;;; Constants.
;;; --------------------------------------------------------------------

(define E2BIG		7)
(define EACCES		13)
(define EADDRINUSE	98)
(define EADDRNOTAVAIL	99)
(define EADV		68)
(define EAFNOSUPPORT	97)
(define EAGAIN		11)
(define EALREADY	114)
(define EBADE		52)
(define EBADF		9)
(define EBADFD		77)
(define EBADMSG		74)
(define EBADR		53)
(define EBADRQC		56)
(define EBADSLT		57)
(define EBFONT		59)
(define EBUSY		16)
(define ECANCELED	125)
(define ECHILD		10)
(define ECHRNG		44)
(define ECOMM		70)
(define ECONNABORTED	103)
(define ECONNREFUSED	111)
(define ECONNRESET	104)
(define EDEADLK		35)
(define EDEADLOCK	35)
(define EDESTADDRREQ	89)
(define EDOM		33)
(define EDOTDOT		73)
(define EDQUOT		122)
(define EEXIST		17)
(define EFAULT		14)
(define EFBIG		27)
(define EHOSTDOWN	112)
(define EHOSTUNREACH	113)
(define EIDRM		43)
(define EILSEQ		84)
(define EINPROGRESS	115)
(define EINTR		4)
(define EINVAL		22)
(define EIO		5)
(define EISCONN		106)
(define EISDIR		21)
(define EISNAM		120)
(define EKEYEXPIRED	127)
(define EKEYREJECTED	129)
(define EKEYREVOKED	128)
(define EL2HLT		51)
(define EL2NSYNC	45)
(define EL3HLT		46)
(define EL3RST		47)
(define ELIBACC		79)
(define ELIBBAD		80)
(define ELIBEXEC	83)
(define ELIBMAX		82)
(define ELIBSCN		81)
(define ELNRNG		48)
(define ELOOP		40)
(define EMEDIUMTYPE	124)
(define EMFILE		24)
(define EMLINK		31)
(define EMSGSIZE	90)
(define EMULTIHOP	72)
(define ENAMETOOLONG	36)
(define ENAVAIL		119)
(define ENETDOWN	100)
(define ENETRESET	102)
(define ENETUNREACH	101)
(define ENFILE		23)
(define ENOANO		55)
(define ENOBUFS		105)
(define ENOCSI		50)
(define ENODATA		61)
(define ENODEV		19)
(define ENOENT		2)
(define ENOEXEC		8)
(define ENOKEY		126)
(define ENOLCK		37)
(define ENOLINK		67)
(define ENOMEDIUM	123)
(define ENOMEM		12)
(define ENOMSG		42)
(define ENONET		64)
(define ENOPKG		65)
(define ENOPROTOOPT	92)
(define ENOSPC		28)
(define ENOSR		63)
(define ENOSTR		60)
(define ENOSYS		38)
(define ENOTBLK		15)
(define ENOTCONN	107)
(define ENOTDIR		20)
(define ENOTEMPTY	39)
(define ENOTNAM		118)
(define ENOTRECOVERABLE	131)
(define ENOTSOCK	88)
(define ENOTTY		25)
(define ENOTUNIQ	76)
(define ENXIO		6)
(define EOPNOTSUPP	95)
(define EOVERFLOW	75)
(define EOWNERDEAD	130)
(define EPERM		1)
(define EPFNOSUPPORT	96)
(define EPIPE		32)
(define EPROTO		71)
(define EPROTONOSUPPORT	93)
(define EPROTOTYPE	91)
(define ERANGE		34)
(define EREMCHG		78)
(define EREMOTE		66)
(define EREMOTEIO	121)
(define ERESTART	85)
(define EROFS		30)
(define ESHUTDOWN	108)
(define ESOCKTNOSUPPORT	94)
(define ESPIPE		29)
(define ESRCH		3)
(define ESRMNT		69)
(define ESTALE		116)
(define ESTRPIPE	86)
(define ETIME		62)
(define ETIMEDOUT	110)
(define ETOOMANYREFS	109)
(define ETXTBSY		26)
(define EUCLEAN		117)
(define EUNATCH		49)
(define EUSERS		87)
(define EWOULDBLOCK	11)
(define EXDEV		18)
(define EXFULL		54)



;;; --------------------------------------------------------------------
;;; Conversion.
;;; --------------------------------------------------------------------

(define errno-alist
  '((E2BIG		. 7)
    (EACCES		. 13)
    (EADDRINUSE		. 98)
    (EADDRNOTAVAIL	. 99)
    (EADV		. 68)
    (EAFNOSUPPORT	. 97)
    (EAGAIN		. 11)
    (EALREADY		. 114)
    (EBADE		. 52)
    (EBADF		. 9)
    (EBADFD		. 77)
    (EBADMSG		. 74)
    (EBADR		. 53)
    (EBADRQC		. 56)
    (EBADSLT		. 57)
    (EBFONT		. 59)
    (EBUSY		. 16)
    (ECANCELED		. 125)
    (ECHILD		. 10)
    (ECHRNG		. 44)
    (ECOMM		. 70)
    (ECONNABORTED	. 103)
    (ECONNREFUSED	. 111)
    (ECONNRESET		. 104)
    (EDEADLK		. 35)
    (EDEADLOCK		. 35)
    (EDESTADDRREQ	. 89)
    (EDOM		. 33)
    (EDOTDOT		. 73)
    (EDQUOT		. 122)
    (EEXIST		. 17)
    (EFAULT		. 14)
    (EFBIG		. 27)
    (EHOSTDOWN		. 112)
    (EHOSTUNREACH	. 113)
    (EIDRM		. 43)
    (EILSEQ		. 84)
    (EINPROGRESS	. 115)
    (EINTR		. 4)
    (EINVAL		. 22)
    (EIO		. 5)
    (EISCONN		. 106)
    (EISDIR		. 21)
    (EISNAM		. 120)
    (EKEYEXPIRED	. 127)
    (EKEYREJECTED	. 129)
    (EKEYREVOKED	. 128)
    (EL2HLT		. 51)
    (EL2NSYNC		. 45)
    (EL3HLT		. 46)
    (EL3RST		. 47)
    (ELIBACC		. 79)
    (ELIBBAD		. 80)
    (ELIBEXEC		. 83)
    (ELIBMAX		. 82)
    (ELIBSCN		. 81)
    (ELNRNG		. 48)
    (ELOOP		. 40)
    (EMEDIUMTYPE	. 124)
    (EMFILE		. 24)
    (EMLINK		. 31)
    (EMSGSIZE		. 90)
    (EMULTIHOP		. 72)
    (ENAMETOOLONG	. 36)
    (ENAVAIL		. 119)
    (ENETDOWN		. 100)
    (ENETRESET		. 102)
    (ENETUNREACH	. 101)
    (ENFILE		. 23)
    (ENOANO		. 55)
    (ENOBUFS		. 105)
    (ENOCSI		. 50)
    (ENODATA		. 61)
    (ENODEV		. 19)
    (ENOENT		. 2)
    (ENOEXEC		. 8)
    (ENOKEY		. 126)
    (ENOLCK		. 37)
    (ENOLINK		. 67)
    (ENOMEDIUM		. 123)
    (ENOMEM		. 12)
    (ENOMSG		. 42)
    (ENONET		. 64)
    (ENOPKG		. 65)
    (ENOPROTOOPT	. 92)
    (ENOSPC		. 28)
    (ENOSR		. 63)
    (ENOSTR		. 60)
    (ENOSYS		. 38)
    (ENOTBLK		. 15)
    (ENOTCONN		. 107)
    (ENOTDIR		. 20)
    (ENOTEMPTY		. 39)
    (ENOTNAM		. 118)
    (ENOTRECOVERABLE	. 131)
    (ENOTSOCK		. 88)
    (ENOTTY		. 25)
    (ENOTUNIQ		. 76)
    (ENXIO		. 6)
    (EOPNOTSUPP		. 95)
    (EOVERFLOW		. 75)
    (EOWNERDEAD		. 130)
    (EPERM		. 1)
    (EPFNOSUPPORT	. 96)
    (EPIPE		. 32)
    (EPROTO		. 71)
    (EPROTONOSUPPORT	. 93)
    (EPROTOTYPE		. 91)
    (ERANGE		. 34)
    (EREMCHG		. 78)
    (EREMOTE		. 66)
    (EREMOTEIO		. 121)
    (ERESTART		. 85)
    (EROFS		. 30)
    (ESHUTDOWN		. 108)
    (ESOCKTNOSUPPORT	. 94)
    (ESPIPE		. 29)
    (ESRCH		. 3)
    (ESRMNT		. 69)
    (ESTALE		. 116)
    (ESTRPIPE		. 86)
    (ETIME		. 62)
    (ETIMEDOUT		. 110)
    (ETOOMANYREFS	. 109)
    (ETXTBSY		. 26)
    (EUCLEAN		. 117)
    (EUNATCH		. 49)
    (EUSERS		. 87)
    (EWOULDBLOCK	. 11)
    (EXDEV		. 18)
    (EXFULL		. 54)))


(define errno-vector
  (let ((ev (make-vector (+ 1 (fold (lambda (pair max)
				      (if (< max (cdr pair))
					  (cdr pair)
					max))
				    0 errno-alist))
			 #f)))
    (for-each
	(lambda (pair)
	  (vector-set! ev (cdr pair) (car pair)))
      errno-alist)
    ev))

(define (symbol->errno symbol)
  (assq symbol errno-alist))

(define (symbol->errno/or-error errno-symbol)
  (let ((errno-code (symbol->errno errno-symbol)))
    (or errno-code
	(assertion-violation 'symbol->errno/or-error
	  "expected symbolic errno value" errno-symbol))))

(define (errno->symbol errno-code)
  (and (< errno-code (vector-length errno-vector))
       (vector-ref errno-vector errno-code)))

(define (errno->symbol/or-error errno-code)
  (let ((errno-symbol (errno->symbol errno-code)))
    (or errno-symbol
	(assertion-violation 'errno->symbol/or-error
	  "expected numeric errno value" errno-code))))


;;; --------------------------------------------------------------------
;;; Conditions.
;;; --------------------------------------------------------------------

(define-condition-type &errno &error
  make-errno-condition errno-condition?
  (numeric-value	errno-numeric-value)
  (symbolic-value	errno-symbolic-value))

(define (make-errno-condition* errno-numeric-value)
  (make-errno-condition errno-numeric-value
			(errno->symbol/or-error errno-numeric-value)))

(define (raise-errno-error who errno irritants)
  (raise (condition (make-who-condition who)
		    (make-message-condition (strerror errno))
		    (make-errno-condition* errno)
		    (make-irritants-condition irritants))))


;;; --------------------------------------------------------------------
;;; Done.
;;; --------------------------------------------------------------------

)

;;; end of file
