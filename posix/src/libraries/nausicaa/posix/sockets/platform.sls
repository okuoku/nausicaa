;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: callouts to socket functions
;;;Date: Sat Dec 19, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (nausicaa posix sockets platform)
  (export
    htons		htonl
    ntohs		ntohl
    inet_aton		inet_pton
    inet_ntoa		inet_ntop

    gethostbyname	gethostbyaddr
    sethostent		gethostent		endhostent

    getprotobyname	getprotobynumber
    setprotoent		getprotoent		endprotoent

    getservbyname	getservbyport
    setservent		getservent		endservent

    socket		socketpair
    shutdown
    connect		bind
    listen		accept
    getsockname		getpeername
    send		recv
    sendto		recvfrom
    getsockopt		setsockopt
    SUN_LEN

    if_nametoindex	if_indextoname
    if_nameindex	if_freenameindex

    getnetbyname	getnetbyaddr
    setnetent		getnetent		endnetent

    h_errno)
  (import (rnrs)
    (nausicaa ffi)
    (nausicaa ffi sizeof)
    (nausicaa posix shared-object)
    (nausicaa posix sizeof))


(define-c-functions libc-shared-object
  (htons		(uint16_t htons (uint16_t)))
  (htonl		(uint32_t htonl (uint32_t)))
  (ntohs		(uint16_t ntohs (uint16_t)))
  (ntohl		(uint32_t ntohl (uint32_t)))

  (inet_aton		(int inet_aton (char* void*)))
  (inet_pton		(int inet_pton (int char* void*)))
  (inet_ntoa		(char* inet_ntoa (uint32_t)))
  (inet_ntop		(char* inet_ntop (int void* char* size_t)))

  (if_nametoindex	(unsigned if_nametoindex (char*)))
  (if_indextoname	(char* if_indextoname (unsigned char*)))
  (if_nameindex		(if_nameindex* if_nameindex (void)))
  (if_freenameindex	(void if_freenameindex (if_nameindex*))))

(define-c-functions libc-shared-object
  (gethostbyname	(pointer gethostbyname (char*)))
  (gethostbyaddr	(pointer gethostbyaddr (char* size_t int)))
  (sethostent		(void sethostent (int)))
  (gethostent		(void* gethostent (void)))
  (endhostent		(void endhostent (void)))

  (getnetbyname		(netent* getnetbyname (char*)))
  (getnetbyaddr		(netent* getnetbyaddr (unsigned-long int)))
  (setnetent		(void setnetent (int)))
  (getnetent		(netent* getnetent (void)))
  (endnetent		(void endnetent (void)))

  (getprotobyname	(void* getprotobyname (char*)))
  (getprotobynumber	(void* getprotobynumber (int)))
  (setprotoent		(void setprotoent (int)))
  (getprotoent		(void* getprotoent (void)))
  (endprotoent		(void endprotoent (void)))

  (getservbyname	(void* getservbyname (char* char*)))
  (getservbyport	(void* getservbyport (int char*)))
  (setservent		(void setservent (int)))
  (getservent		(void* getservent (void)))
  (endservent		(void endservent (void))))

(define h_errno_pointer
  (lookup-shared-object libc-shared-object "h_errno"))
(define (h_errno)
  (pointer-ref-c-signed-int h_errno_pointer 0))

(define-c-functions/with-errno libc-shared-object
  (bind			(int bind (int sockaddr* socklen_t)))
  (getsockname		(int getsockname (int sockaddr* void*)))
  (socket		(int socket (int int int)))
  (socketpair		(int socketpair (int int int void*)))
  (shutdown		(int shutdown (int int)))
  (connect		(int connect (int sockaddr* socklen_t)))
  (listen		(int listen (int unsigned)))
  (accept		(int accept (int sockaddr* socklen_t*)))
  (getpeername		(int getpeername (int sockaddr* socklen_t*)))
  (send			(int send (int void* size_t int)))
  (recv			(int recv (int void* size_t int)))
  (sendto		(int sendto (int void* size_t int sockaddr* socklen_t)))
  (recvfrom		(int recvfrom (int void* size_t int sockaddr* socklen_t*)))
  (getsockopt		(int getsockopt (int int int void* socklen_t*)))
  (setsockopt		(int setsockopt (int int int void* socklen_t))))

(define-c-functions libnausicaa-posix
  (SUN_LEN		(int nausicaa_posix_SUN_LEN (pointer))))


;;;; done

)

;;; end of file
