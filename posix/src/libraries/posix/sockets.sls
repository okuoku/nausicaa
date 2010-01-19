;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: parametrised interface to sockets functions
;;;Date: Sun Dec 20, 2009
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


(library (posix sockets)
  (export

    ;; Internet address conversion
    inet-aton		inet-pton
    inet-ntoa		inet-ntop
    (rename (primitive:htons		htons)
	    (primitive:htonl		htonl)
	    (primitive:ntohs		ntohs)
	    (primitive:ntohl		ntohl))

    ;; host names
    gethostbyname		gethostbyaddr
    sethostent			gethostent		endhostent

    ;; networks database
    getnetbyname		getnetbyaddr
    setnetent			getnetent		endnetent

    ;; protocols database
    getprotobyname		getprotobynumber
    setprotoent			getprotoent		endprotoent

    ;; interface name
    if-nametoindex		if-indextoname
    if-nameindex

    ;; socket operations
    socket			socketpair
    shutdown			connect
    bind			listen
    accept
    getsockname			getpeername
    send			recv
    send/string			recv/string
    send/bytevector		recv/bytevector
    send/memblock		recv/memblock
    sendto			recvfrom
    sendto/string		recvfrom/string
    sendto/bytevector		recvfrom/bytevector
    sendto/memblock		recvfrom/memblock
    getsockopt			setsockopt

    ;; syntaxes
    socket*			socketpair*)
  (import (rnrs)
    (posix helpers)
    (posix typedefs)
    (prefix (posix sockets primitives) primitive:))


(define-parametrised inet-aton address-name)
(define-parametrised inet-pton namespace address-name)
(define-parametrised inet-ntoa address-bytevector)
(define-parametrised inet-ntop namespace address-bytevector)

(define-parametrised gethostbyname host-name)
(define-parametrised gethostbyaddr address-bytevector)
(define-parametrised sethostent (() (stay-open?)))
(define-parametrised gethostent)
(define-parametrised endhostent)

(define-parametrised getnetbyname net-name)
(define-parametrised getnetbyaddr ((address-number) (address-number address-format)))
(define-parametrised setnetent (() (stay-open?)))
(define-parametrised getnetent)
(define-parametrised endnetent)

(define-parametrised getprotobyname proto-name)
(define-parametrised getprotobynumber number)
(define-parametrised setprotoent (() (stay-open?)))
(define-parametrised getprotoent)
(define-parametrised endprotoent)

(define-parametrised if-nametoindex name)
(define-parametrised if-indextoname index)
(define-parametrised if-nameindex)

(define-parametrised socket     ((namespace style) (namespace style protocol)))
(define-parametrised socketpair ((namespace style) (namespace style protocol)))
(define-parametrised shutdown sock how)

(define-parametrised bind sock sockaddr)
(define-parametrised connect sock sockaddr)
(define-parametrised listen sock max-pending-connections)
(define-parametrised accept sock)

(define-parametrised send		((sock buf.ptr buf.len) (sock buf.ptr buf.len flags)))
(define-parametrised send/string	((sock str) (sock str flags)))
(define-parametrised send/bytevector	((sock bv) (sock bv flags)))
(define-parametrised send/memblock	((sock mb) (sock mb flags)))

(define-parametrised recv		((sock buf.ptr buf.len) (sock buf.ptr buf.len flags)))
(define-parametrised recv/string	((sock max-len) (sock max-len flags)))
(define-parametrised recv/bytevector	((sock max-len) (sock max-len flags)))
(define-parametrised recv/memblock	((sock max-len malloc) (sock max-len flags malloc)))

(define-parametrised sendto		((sock buf.ptr buf.len sockaddr)
					 (sock buf.ptr buf.len flags sockaddr)))
(define-parametrised sendto/string	((sock str sockaddr) (sock str flags sockaddr)))
(define-parametrised sendto/bytevector	((sock bv sockaddr) (sock bv flags sockaddr)))
(define-parametrised sendto/memblock	((sock mb sockaddr) (sock mb flags sockaddr)))

(define-parametrised recvfrom		((sock buf.ptr buf.len) (sock buf.ptr buf.len flags)))
(define-parametrised recvfrom/string	((sock max-len) (sock max-len flags)))
(define-parametrised recvfrom/bytevector ((sock max-len) (sock max-len flags)))
(define-parametrised recvfrom/memblock  ((sock max-len malloc) (sock max-len flags malloc)))

(define-parametrised getsockname sock)
(define-parametrised getpeername sock)

(define-parametrised getsockopt sock option)
(define-parametrised setsockopt sock option optval)


(define-syntax socket*
  (syntax-rules (namespace style protocol)
    ((_ (namespace ?namespace) (style ?style) (protocol ?protocol))
     (socket (socket-namespace	?namespace)
	     (socket-style	?style)
	     (socket-protocol	?protocol)))
    ((_ (namespace ?namespace) (style ?style))
     (socket (socket-namespace	?namespace)
	     (socket-style	?style)))
    ))

(define-syntax socketpair*
  (syntax-rules (namespace style protocol)
    ((_ (namespace ?namespace) (style ?style) (protocol ?protocol))
     (socketpair (socket-namespace	?namespace)
		 (socket-style		?style)
		 (socket-protocol	?protocol)))
    ((_ (namespace ?namespace) (style ?style))
     (socketpair (socket-namespace	?namespace)
		 (socket-style		?style)))
    ))



;;;; done

)

;;; end of file
