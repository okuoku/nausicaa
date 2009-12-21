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


(library (posix sockets primitives)
  (export
    (rename (platform:htons		htons)
	    (platform:htonl		htonl)
	    (platform:ntohs		ntohs)
	    (platform:ntohl		ntohl))

    socket		socketpair
    shutdown
    ;; connect		bind
    ;; listen		accept
    ;; getsockname		getpeername
    ;; send		recv
    ;; sendto		recvfrom
    ;; getsockopt		setsockopt

    if-nametoindex	if-indextoname
    if-nameindex

    ;; networks database
    ;; getnetbyname	getnetbyaddr
    ;; setnetent		getnetent	endnetent
    )
  (import (rnrs)
    (begin0)
    (receive)
    (compensations)
    (foreign memory)
    (foreign cstrings)
    (foreign errno)
    (posix typedefs)
    (posix sizeof)
    (prefix (posix sockets platform) platform:))


;;;; interface naming

(define (if-nametoindex name)
  (with-compensations
    (platform:if_nametoindex (string->cstring/c name))))

(define (if-indextoname index)
  (with-compensations
    (let* ((name*	(malloc-block/c IFNAMSIZ))
	   (result*	(platform:if_indextoname index name*)))
      (if (pointer-null? result*)
	  (error 'if-indextoname "invalid network interface index" index)
	(cstring->string name*)))))

(define (if-nameindex)
  (with-compensations
    (letrec ((names* (compensate
			 (begin0-let ((p (platform:if_nameindex)))
			   (when (pointer-null? p)
			     (error 'if-names "unable to acquire list of interface names")))
		       (with
			(platform:if_freenameindex names*)))))
      (let loop ((names* names*)
      		 (alist  '()))
      	(if (pointer-null? (struct-if_nameindex-if_name-ref names*))
	    alist
      	  (loop (pointer-add names* sizeof-if_nameindex)
      	  	(cons (make-<struct-if-nameindex>
		       (struct-if_nameindex-if_index-ref names*)
		       (cstring->string (struct-if_nameindex-if_name-ref names*)))
      	  	      alist)))))))


;;;; opening and closing

(define (socket namespace style protocol)
  (receive (result errno)
      (platform:socket (socket-namespace->value namespace)
		       (socket-style->value style)
		       protocol)
    (if (= -1 result)
	(raise-errno-error 'socket errno (list namespace style protocol))
      (make-<socket> result namespace style protocol))))

(define (socketpair namespace style protocol)
  (with-compensations
    (let ((fds* (malloc-small/c)))
      (receive (result errno)
	  (platform:socketpair (socket-namespace->value namespace)
			       (socket-style->value style)
			       protocol
			       fds*)
	(if (= -1 result)
	    (raise-errno-error 'socketpair errno (list namespace style protocol))
	  (values (make-<socket> (pointer-ref-c-signed-int fds* 0) namespace style protocol)
		  (make-<socket> (pointer-ref-c-signed-int fds* 0) namespace style protocol)))))))

(define (shutdown sock how)
  (receive (result errno)
      (platform:shutdown (file-descriptor->integer sock) (socket-shutdown-mode->value how))
    (when (= -1 result)
      (raise-errno-error 'shutdown errno (list sock how)))))


;;;; addresses

(define (bind sock sockaddr)
  (with-compensations
    (let* ((bv		(<struct-sockaddr>-struct sockaddr))
	   (sockaddr*	(bytevector->pointer bv malloc-block/c))
	   (socklen	(bytevector-length bv)))
      (receive (result errno)
	  (platform:bind (file-descriptor->integer sock) sockaddr* socklen)
	(if (= -1 result)
	    (raise-errno-error 'bind errno (list sock sockaddr))
	  result)))))

(define (getsockname sock)
  (with-compensations
    (let* ((socklen	(socket-namespace->socklen (<socket>-namespace sock)))
	   (socklen*	(begin0-let ((p (malloc-small/c)))
			  (pointer-set-c-signed-int! p socklen)))
	   (sockaddr*	(malloc-block/c sizeof-sockaddr)))
      (receive (result errno)
	  (platform:getsockname (file-descriptor->integer sock) sockaddr* socklen*)
	(if (= -1 result)
	    (raise-errno-error 'getsockname errno sock)
	  result)))))


;;;; done

)

;;; end of file
