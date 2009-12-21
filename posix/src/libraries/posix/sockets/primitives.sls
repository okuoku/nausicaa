;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: marshaling interface to sockets callouts
;;;Date: Sun Dec 20, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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
    (rename (platform:htons		htons)
	    (platform:htonl		htonl)
	    (platform:ntohs		ntohs)
	    (platform:ntohl		ntohl))

    socket
    socketpair
    shutdown
    connect
    bind
    listen
    accept
    getsockname
    getpeername
    send
    recv
    sendto
    recvfrom
    getsockopt
    setsockopt

    if_nametoindex
    if_indextoname
    if_nameindex
    if_freenameindex

    ;; networks database
    getnetbyname
    getnetbyaddr
    setnetent
    getnetent
    endnetent)
  (import (rnrs)
    (receive)
    (compensations)
    (foreign memory)
    (foreign cstrings)
    (posix typedefs)
    (posix sizeof)
    (prefix (posix sockets platform) platform:))


;;;; code



;;;; done

)

;;; end of file
