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
    shutdown		connect
    bind		listen
    accept
    getsockname		getpeername
    send		recv
    sendto		recvfrom
    getsockopt		setsockopt

    if-nametoindex	if-indextoname
    if-nameindex

    ;; networks database
    ;; getnetbyname	getnetbyaddr
    ;; setnetent		getnetent	endnetent

    pointer-><struct-sockaddr-in>
    pointer-><struct-sockaddr-in6>
    pointer-><struct-sockaddr-un>

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

(define socket
  (case-lambda
   ((namespace style)
    (socket namespace style (socket-protocol zero)))
   ((namespace style protocol)
    (receive (result errno)
	(platform:socket (socket-namespace->value namespace)
			 (socket-style->value style)
			 (socket-protocol->value protocol))
      (if (= -1 result)
	  (raise-errno-error 'socket errno (list namespace style protocol))
	(make-<socket> result namespace style protocol))))))

(define socketpair
  (case-lambda
   ((namespace style)
    (socketpair namespace style (socket-protocol zero)))
   ((namespace style protocol)
    (with-compensations
      (let ((fds* (malloc-small/c)))
	(receive (result errno)
	    (platform:socketpair (socket-namespace->value namespace)
				 (socket-style->value style)
				 (socket-protocol->value protocol)
				 fds*)
	  (if (= -1 result)
	      (raise-errno-error 'socketpair errno (list namespace style protocol))
	    (values (make-<socket> (pointer-ref-c-signed-int fds* 0) namespace style protocol)
		    (make-<socket> (pointer-ref-c-signed-int fds* 0) namespace style protocol)))))))))

(define (shutdown sock how)
  (receive (result errno)
      (platform:shutdown (file-descriptor->integer sock) (socket-shutdown-mode->value how))
    (when (= -1 result)
      (raise-errno-error 'shutdown errno (list sock how)))))


;;;; sockaddr records builders

(define (sockaddr->pointer/length sockaddr)
  (cond ((<struct-sockaddr-un>? sockaddr)
	 (<struct-sockaddr-un>->pointer/length sockaddr malloc-block/c))
	((<struct-sockaddr-in>? sockaddr)
	 (<struct-sockaddr-in>->pointer/length sockaddr malloc-block/c))
	((<struct-sockaddr-in6>? sockaddr)
	 (<struct-sockaddr-in6>->pointer/length sockaddr malloc-block/c))
	(else
	 (assertion-violation 'bind "invalid socket address object" sockaddr))))

(define (<struct-sockaddr-un>->pointer/length sockaddr malloc)
  (with-compensations
    (let* ((cstr.ptr	(string->cstring/c (<struct-sockaddr-un>-path sockaddr)))
	   (cstr.len	(strlen cstr.ptr))
	   (addr*	(malloc (+ 4 1 cstr.len))))
      (struct-sockaddr_un-sun_family-set! addr* (<struct-sockaddr-un>-family sockaddr))
      (strncpy (struct-sockaddr_un-sun_path-ref addr*) cstr.ptr cstr.len)
      (values addr* (platform:SUN_LEN addr*)))))

(define (<struct-sockaddr-in>->pointer/length sockaddr malloc)
  (begin0-let ((sockaddr* (malloc sizeof-sockaddr_in)))
    (struct-sockaddr_in-sin_family sockaddr* (<struct-sockaddr-in>-family sockaddr))
    (struct-sockaddr_in-sin_port   sockaddr* (<struct-sockaddr-in>-port sockaddr))
    (with-compensations
      (let ((addr_in*	(struct-sockaddr_in-sin_addr-ref sockaddr*))
	    (ptr	(bytevector->pointer (<struct-sockaddr-in>-addr sockaddr) malloc-block/c)))
	(memcpy addr_in* ptr sizeof-addr_in)))))

(define (<struct-sockaddr-in6>->pointer/length sockaddr malloc)
  (begin0-let ((sockaddr* (malloc sizeof-sockaddr_in6)))
    (struct-sockaddr_in6-sin6_family sockaddr* (<struct-sockaddr-in6>-family sockaddr))
    (struct-sockaddr_in6-sin6_port   sockaddr* (<struct-sockaddr-in6>-port sockaddr))
    (with-compensations
      (let ((addr_in6*	(struct-sockaddr_in6-sin6_addr-ref sockaddr*))
	    (ptr	(bytevector->pointer (<struct-sockaddr-in6>-addr sockaddr) malloc-block/c)))
	(memcpy addr_in* ptr sizeof-addr_in)))))

;;; --------------------------------------------------------------------

(define (pointer->sockaddr sockaddr*)
  (let ((namespace (struct-sockaddr-sa_family-ref sockaddr*)))
    (cond ((or (= namespace AF_LOCAL)
	       (= namespace AF_UNIX)  ;redundant
	       (= namespace AF_FILE)) ;redundant
	   (pointer-><struct-sockaddr-un> sockaddr*))
	  ((= namespace AF_INET)
	   (pointer-><struct-sockaddr-in> sockaddr*))
	  ((= namespace AF_INET6)
	   (pointer-><struct-sockaddr-in6> sockaddr*))
	  (else
	   (assertion-violation 'accept
	     "invalid namespace in sockaddr structure" namespace)))))

(define (pointer-><struct-sockaddr-in> pointer)
  (make-<struct-sockaddr-in>
   (struct-sockaddr_in-sin_family-ref pointer)
   (pointer->bytevector (struct-sockaddr_in-sin_addr-ref pointer) sizeof-in_addr)
   (struct-sockaddr_in-sin_port-ref pointer)))

(define (pointer-><struct-sockaddr-in6> pointer)
  (make-<struct-sockaddr-in6>
   (struct-sockaddr_in6-sin6_family-ref pointer)
   (pointer->bytevector (struct-sockaddr_in6-sin6_addr-ref pointer) sizeof-in6_addr)
   (struct-sockaddr_in6-sin6_port-ref pointer)))

(define (pointer-><struct-sockaddr-un> pointer)
  (make-<struct-sockaddr-un> (cstring->string (struct-sockaddr_un-sun_path-ref pointer))))


;;;; addresses

(define (bind sock sockaddr)
  (with-compensations
    (receive (sockaddr* socklen)
	(sockaddr->pointer/length sockaddr)
      (receive (result errno)
	  (platform:bind (file-descriptor->integer sock) sockaddr* socklen)
	(when (= -1 result)
	  (raise-errno-error 'bind errno (list sock sockaddr)))))))

(define (connect sock sockaddr)
  (with-compensations
    (receive (sockaddr* socklen)
	(sockaddr->pointer/length sockaddr)
      (receive (result errno)
	  (platform:connect (file-descriptor->integer sock) sockaddr* socklen)
	(when (= -1 result)
	  (raise-errno-error 'connect errno (list sock sockaddr)))))))

(define (listen sock max-pending-connections)
  (receive (result errno)
      (platform:listen (file-descriptor->integer sock) max-pending-connections)
    (when (= -1 result)
      (raise-errno-error 'listen errno (list sock max-pending-connections)))))

(define (accept sock sockaddr)
  (with-compensations
    (let* ((socklen	4096) ;let's play it safe
	   (sockaddr*	(malloc-block/c socklen))
	   (socklen*	(malloc-small/c)))
      (pointer-set-c-socklen_t! socklen* socklen)
      (receive (result errno)
	  (platform:accept (file-descriptor->integer sock) sockaddr* socklen*)
	(if (= -1 result)
	    (raise-errno-error 'accept errno (list sock sockaddr))
	  (values (make-<socket> (integer->file-descriptor result)
				 (<socket>-namespace sock)
				 (<socket>-style     sock)
				 (<socket>-protocol  sock))
		  ;;In truth we  know the namespace of SOCK,  so we know
		  ;;also the type of sockaddr structure.
		  (pointer->sockaddr sockaddr*)))))))


(define (getsockname sock)
  (with-compensations
    (let* ((socklen	4096) ;let's play it safe
	   (sockaddr*	(malloc-block/c socklen))
	   (socklen*	(malloc-small/c)))
      (pointer-set-c-socklen_t! socklen* socklen)
      (receive (result errno)
	  (platform:getsockname (file-descriptor->integer sock) sockaddr* socklen*)
	(if (= -1 result)
	    (raise-errno-error 'getsockname errno sock)
	  ;;In truth we know the namespace  of SOCK, so we know also the
	  ;;type of sockaddr structure.
	  (pointer->sockaddr sockaddr*))))))

(define (getpeername sock)
  (with-compensations
    (let* ((socklen	4096) ;let's play it safe
	   (sockaddr*	(malloc-block/c socklen))
	   (socklen*	(malloc-small/c)))
      (pointer-set-c-socklen_t! socklen* socklen)
      (receive (result errno)
	  (platform:getpeername (file-descriptor->integer sock) sockaddr* socklen*)
	(if (= -1 result)
	    (raise-errno-error 'getpeername errno sock)
	  ;;In truth we know the namespace  of SOCK, so we know also the
	  ;;type of sockaddr structure.
	  (pointer->sockaddr sockaddr*))))))


(define (send sock buf.ptr buf.len flags)
  (receive (result errno)
      (platform:send (file-descriptor->integer sock) buf.ptr buf.len
		     (socket-data-options->value flags))
    (if (= -1 result)
	(raise-errno-error 'send errno (list sock buf.ptr buf.len flags))
      result)))

(define (recv)
  (receive (result errno)
      (platform:recv (file-descriptor->integer sock) buf.ptr buf.len
		     (socket-data-options->value flags))
    (if (= -1 result)
	(raise-errno-error 'recv errno (list sock buf.ptr buf.len flags))
      result)))

(define (sendto)
  )

;; glibc node "Sending Datagrams"

(define (recvfrom)
  )


(define (getsockopt)
  )

(define (setsockopt)
  )


;;;; done

)

;;; end of file
