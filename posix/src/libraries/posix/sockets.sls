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

    ;; interface name
    if-nametoindex		if-indextoname
    if-nameindex

    ;; socket operations
    socket			socketpair
    shutdown
    bind			getsockname

    )
  (import (rnrs)
    (posix helpers)
    (prefix (posix sockets primitives) primitive:))

  (define-parametrised if-nametoindex name)
  (define-parametrised if-indextoname index)
  (define-parametrised if-nameindex)

  (define-primitive-parameter socket-function primitive:socket)
  (define socket
    (case-lambda
     ((namespace style)
      ((socket-function) namespace style))
     ((namespace style protocol)
      ((socket-function) namespace style protocol))))

  (define-primitive-parameter socketpair-function primitive:socketpair)
  (define socketpair
    (case-lambda
     ((namespace style)
      ((socketpair-function) namespace style))
     ((namespace style protocol)
      ((socketpair-function) namespace style protocol))))

  (define-parametrised shutdown sock how)

  (define-parametrised bind sock sockaddr)
  (define-parametrised getsockname sock)

  )

;;; end of file
