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

    ;; interface names
    if-nametoindex	if-indextoname
    if-nameindex

    ;; socket operations
    socket		socketpair
    shutdown		connect
    bind		listen
    accept
    getsockname		getpeername
    send		recv
    send/string		recv/string
    send/bytevector	recv/bytevector
    send/memblock	recv/memblock
    sendto		recvfrom
    sendto/string	recvfrom/string
    sendto/bytevector	recvfrom/bytevector
    sendto/memblock	recvfrom/memblock
    getsockopt		setsockopt

    ;; ancillary messages
    ;; cmsg
    ;; sendmsg		recvmsg

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
    (only (foreign ffi)
	  pointer-ref-c-size_t
	  pointer-set-c-size_t!
	  pointer-ref-c-signed-int
	  pointer-set-c-signed-int!)
    (only (foreign ffi sizeof)
	  sizeof-int sizeof-size_t)
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
		    (make-<socket> (pointer-ref-c-signed-int fds* 1) namespace style protocol)))))))))

(define (shutdown sock how)
  (receive (result errno)
      (platform:shutdown (file-descriptor->integer sock) (socket-shutdown-mode->value how))
    (when (= -1 result)
      (raise-errno-error 'shutdown errno (list sock how)))))


;;;; conversion between sockaddr structures and records

(define (%sockaddr->pointer&length sockaddr malloc)
  (cond ((<struct-sockaddr-un>? sockaddr)
	 (<struct-sockaddr-un>->pointer&length sockaddr malloc))
	((<struct-sockaddr-in>? sockaddr)
	 (<struct-sockaddr-in>->pointer&length sockaddr malloc))
	((<struct-sockaddr-in6>? sockaddr)
	 (<struct-sockaddr-in6>->pointer&length sockaddr malloc))
	(else
	 (assertion-violation 'bind "invalid socket address object" sockaddr))))

(define (<struct-sockaddr-un>->pointer&length sockaddr malloc)
  (with-compensations
    (let* ((cstr.ptr	(string->cstring/c (<struct-sockaddr-un>-path sockaddr)))
	   (cstr.len	(strlen cstr.ptr))
	   (sockaddr*	(malloc (+ 16 cstr.len))))
      (struct-sockaddr_un-sun_family-set! sockaddr* (<struct-sockaddr-un>-family sockaddr))
      (strncpy (struct-sockaddr_un-sun_path-ref sockaddr*) cstr.ptr cstr.len)
      (values sockaddr* (platform:SUN_LEN sockaddr*)))))

(define (<struct-sockaddr-in>->pointer&length sockaddr malloc)
  (begin0-let ((sockaddr* (malloc sizeof-sockaddr_in)))
    (struct-sockaddr_in-sin_family-set! sockaddr* (<struct-sockaddr-in>-family sockaddr))
    (struct-sockaddr_in-sin_port-set!   sockaddr* (<struct-sockaddr-in>-port sockaddr))
    (with-compensations
      (let ((in_addr*	(struct-sockaddr_in-sin_addr-ref sockaddr*))
	    (ptr	(bytevector->pointer (<struct-sockaddr-in>-addr sockaddr) malloc-block/c)))
	(memcpy in_addr* ptr sizeof-in_addr)))))

(define (<struct-sockaddr-in6>->pointer&length sockaddr malloc)
  (begin0-let ((sockaddr* (malloc sizeof-sockaddr_in6)))
    (struct-sockaddr_in6-sin6_family-set! sockaddr* (<struct-sockaddr-in6>-family sockaddr))
    (struct-sockaddr_in6-sin6_port-set!   sockaddr* (<struct-sockaddr-in6>-port sockaddr))
    (with-compensations
      (let ((in6_addr*	(struct-sockaddr_in6-sin6_addr-ref sockaddr*))
	    (ptr	(bytevector->pointer (<struct-sockaddr-in6>-addr sockaddr) malloc-block/c)))
	(memcpy in6_addr* ptr sizeof-in6_addr)))))

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

(define (pointer-><struct-sockaddr-un> sockaddr*)
  (make-<struct-sockaddr-un> (cstring->string (struct-sockaddr_un-sun_path-ref sockaddr*))))

(define (pointer-><struct-sockaddr-in> sockaddr*)
  (make-<struct-sockaddr-in>
   (struct-sockaddr_in-sin_family-ref sockaddr*)
   (pointer->bytevector (struct-sockaddr_in-sin_addr-ref sockaddr*) sizeof-in_addr)
   (struct-sockaddr_in-sin_port-ref sockaddr*)))

(define (pointer-><struct-sockaddr-in6> sockaddr*)
  (make-<struct-sockaddr-in6>
   (struct-sockaddr_in6-sin6_family-ref sockaddr*)
   (pointer->bytevector (struct-sockaddr_in6-sin6_addr-ref sockaddr*) sizeof-in6_addr)
   (struct-sockaddr_in6-sin6_port-ref sockaddr*)))


;;;; addresses

(define (bind sock sockaddr)
  (with-compensations
    (receive (sockaddr* socklen)
	(%sockaddr->pointer&length sockaddr malloc)
      (push-compensation (primitive-free sockaddr*))
      (receive (result errno)
	  (platform:bind (file-descriptor->integer sock) sockaddr* socklen)
	(when (= -1 result)
	  (raise-errno-error 'bind errno (list sock sockaddr)))))))

(define (connect sock sockaddr)
  (with-compensations
    (receive (sockaddr* socklen)
	(%sockaddr->pointer&length sockaddr malloc)
      (push-compensation (primitive-free sockaddr*))
      (receive (result errno)
	  (platform:connect (file-descriptor->integer sock) sockaddr* socklen)
	(when (= -1 result)
	  (raise-errno-error 'connect errno (list sock sockaddr)))))))

(define (listen sock max-pending-connections)
  (receive (result errno)
      (platform:listen (file-descriptor->integer sock) max-pending-connections)
    (when (= -1 result)
      (raise-errno-error 'listen errno (list sock max-pending-connections)))))

(define (accept sock)
  (with-compensations
    (let* ((socklen	4096) ;let's play it safe
	   (sockaddr*	(malloc-block/c socklen))
	   (socklen*	(malloc-small/c)))
      (pointer-set-c-socklen_t! socklen* 0 socklen)
      (receive (result errno)
	  (platform:accept (file-descriptor->integer sock) sockaddr* socklen*)
	(cond ((= -1 result)
	       (raise-errno-error 'accept errno sock))
	      ((< socklen (pointer-ref-c-socklen_t socklen* 0))
	       (error 'accept
		 "sockaddr structure of source address too big" socklen))
	      (else
	       (values (make-<socket> result
				      (<socket>-namespace sock)
				      (<socket>-style     sock)
				      (<socket>-protocol  sock))
		       ;;In truth  we know the namespace of  SOCK, so we
		       ;;know also the type of sockaddr structure.
		       (pointer->sockaddr sockaddr*))))))))


(define (getsockname sock)
  (with-compensations
    (let* ((socklen	4096) ;let's play it safe
	   (sockaddr*	(malloc-block/c socklen))
	   (socklen*	(malloc-small/c)))
      (pointer-set-c-socklen_t! socklen* socklen)
      (receive (result errno)
	  (platform:getsockname (file-descriptor->integer sock) sockaddr* socklen*)
	(cond ((= -1 result)
	       (raise-errno-error 'getsockname errno sock))
	      ((< socklen (pointer-ref-c-socklen_t socklen*))
	       (error 'getsockname
		 "sockaddr structure of source address too big" socklen))
	      (else
	       ;;In truth we know the namespace of SOCK, so we know also
	       ;;the type of sockaddr structure.
	       (pointer->sockaddr sockaddr*)))))))

(define (getpeername sock)
  (with-compensations
    (let* ((socklen	4096) ;let's play it safe
	   (sockaddr*	(malloc-block/c socklen))
	   (socklen*	(malloc-small/c)))
      (pointer-set-c-socklen_t! socklen* socklen)
      (receive (result errno)
	  (platform:getpeername (file-descriptor->integer sock) sockaddr* socklen*)
	(cond ((= -1 result)
	       (raise-errno-error 'getpeername errno sock))
	      ((< socklen (pointer-ref-c-socklen_t socklen*))
	       (error 'getpeername
		 "sockaddr structure of source address too big" socklen))
	      (else
	       ;;In truth we know the namespace of SOCK, so we know also
	       ;;the type of sockaddr structure.
	       (pointer->sockaddr sockaddr*)))))))


(define send
  (case-lambda
   ((sock buf.ptr buf.len)
    (send sock buf.ptr buf.len (socket-data-options)))
   ((sock buf.ptr buf.len flags)
    (receive (result errno)
	(platform:send (file-descriptor->integer sock) buf.ptr buf.len
		       (socket-data-options->value flags))
      (if (= -1 result)
	  (raise-errno-error 'send errno (list sock buf.ptr buf.len flags))
	result)))))

(define send/string
  (case-lambda
   ((sock str)
    (send/string sock str (socket-data-options)))
   ((sock str flags)
    (assert (string? str))
    (with-compensations
      (let ((buf.ptr (string->cstring/c str)))
	;;We want to send the terminating zero, too.
	(send sock buf.ptr (+ 1 (strlen buf.ptr)) flags))))))

(define send/bytevector
  (case-lambda
   ((sock bv)
    (send/bytevector sock bv (socket-data-options)))
   ((sock bv flags)
    (assert (bytevector? bv))
    (with-compensations
      (let ((buf.ptr (bytevector->pointer bv malloc-block/c))
	    (buf.len (bytevector-length bv)))
	(send sock buf.ptr (strlen buf.ptr) flags))))))

(define send/memblock
  (case-lambda
   ((sock mb)
    (send/memblock sock mb (socket-data-options)))
   ((sock mb flags)
    (assert (<memblock>? mb))
    (send sock (<memblock>-pointer mb) (<memblock>-size mb) flags))))


(define recv
  (case-lambda
   ((sock buf.ptr buf.len)
    (recv sock buf.ptr buf.len (socket-data-options)))
   ((sock buf.ptr buf.len flags)
    (receive (result errno)
	(platform:recv (file-descriptor->integer sock) buf.ptr buf.len
		       (socket-data-options->value flags))
      (if (= -1 result)
	  (raise-errno-error 'recv errno (list sock buf.ptr buf.len flags))
	result)))))

(define recv/string
  (case-lambda
   ((sock max-len)
    (recv/string sock max-len (socket-data-options)))
   ((sock max-len flags)
    (with-compensations
      (let* ((buf.ptr (malloc-block/c max-len))
	     (buf.len (recv sock buf.ptr max-len flags)))
	(cstring->string buf.ptr buf.len))))))

(define recv/bytevector
  (case-lambda
   ((sock max-len)
    (recv/bytevector sock max-len (socket-data-options)))
   ((sock max-len flags)
    (with-compensations
      (let* ((buf.ptr (malloc-block/c max-len))
	     (buf.len (recv sock buf.ptr max-len flags)))
	(pointer->bytevector buf.ptr buf.len))))))

(define recv/memblock
  (case-lambda
   ((sock max-len malloc)
    (recv/memblock sock max-len (socket-data-options) malloc))
   ((sock max-len flags malloc)
    (let* ((buf.ptr (malloc max-len))
	   (buf.len (recv sock buf.ptr max-len flags)))
      (make-<memblock> buf.ptr buf.len max-len)))))


(define sendto
  (case-lambda
   ((sock buf.ptr buf.len sockaddr)
    (sendto sock buf.ptr buf.len (socket-data-options) sockaddr))
   ((sock buf.ptr buf.len flags sockaddr)
    (with-compensations
      (receive (sockaddr* socklen)
	  (%sockaddr->pointer&length sockaddr malloc)
	(push-compensation (primitive-free sockaddr*))
	(receive (result errno)
	    (platform:sendto (file-descriptor->integer sock)
			     buf.ptr buf.len
			     (socket-data-options->value flags)
			     sockaddr* socklen)
	  (if (= -1 result)
	      (raise-errno-error 'sendto errno (list sock buf.ptr buf.len flags))
	    result)))))))

(define sendto/string
  (case-lambda
   ((sock str sockaddr)
    (sendto/string sock str (socket-data-options) sockaddr))
   ((sock str flags sockaddr)
    (assert (string? str))
    (with-compensations
      (let ((buf.ptr (string->cstring/c str)))
	;;We want to send the terminating zero, too.
	(sendto sock buf.ptr (+ 1 (strlen buf.ptr)) flags sockaddr))))))

(define sendto/bytevector
  (case-lambda
   ((sock bv sockaddr)
    (sendto/bytevector sock bv (socket-data-options) sockaddr))
   ((sock bv flags sockaddr)
    (assert (bytevector? bv))
    (with-compensations
      (let ((buf.ptr (pointer->bytevector bv malloc-block/c))
	    (buf.len (bytevector-length bv)))
	(sendto sock buf.ptr (strlen buf.ptr) flags sockaddr))))))

(define sendto/memblock
  (case-lambda
   ((sock mb sockaddr)
    (sendto/memblock sock mb (socket-data-options) sockaddr))
   ((sock mb flags sockaddr)
    (assert (<memblock>? mb))
    (sendto sock (<memblock>-pointer mb) (<memblock>-size mb) flags sockaddr))))


(define recvfrom
  (case-lambda
   ((sock buf.ptr buf.len)
    (recvfrom sock buf.ptr buf.len (socket-data-options)))
   ((sock buf.ptr buf.len flags)
    (with-compensations
      (let* ((socklen	4096) ;let's play it safe
	     (sockaddr*	(malloc-block/c socklen))
	     (socklen*	(malloc-small/c)))
	(pointer-set-c-socklen_t! socklen* socklen)
	(receive (result errno)
	    (platform:recvfrom (file-descriptor->integer sock)
			       buf.ptr buf.len
			       (socket-data-options->value flags)
			       sockaddr* socklen*)
	  (cond ((= -1 result)
		 (raise-errno-error 'recvfrom errno (list sock buf.ptr buf.len flags)))
		((< socklen (pointer-ref-c-socklen_t socklen*))
		 (error 'recvfrom
		   "sockaddr structure of source address too big" socklen))
		(else
		 (values result (pointer->sockaddr sockaddr*))))))))))

(define recvfrom/string
  (case-lambda
   ((sock max-len)
    (recvfrom/string sock max-len (socket-data-options)))
   ((sock max-len flags)
    (with-compensations
      (let ((buf.ptr (malloc-block/c max-len)))
	(receive (buf.len sockaddr)
	    (recvfrom sock buf.ptr max-len flags)
	  (values (cstring->string buf.ptr buf.len) sockaddr)))))))

(define recvfrom/bytevector
  (case-lambda
   ((sock max-len)
    (recvfrom/bytevector sock max-len (socket-data-options)))
   ((sock max-len flags)
    (with-compensations
      (let ((buf.ptr (malloc-block/c max-len)))
	(receive (buf.len sockaddr)
	    (recvfrom sock buf.ptr max-len flags)
	  (values (pointer->bytevector buf.ptr buf.len) sockaddr)))))))

(define recvfrom/memblock
  (case-lambda
   ((sock max-len malloc)
    (recvfrom/memblock sock max-len (socket-data-options) malloc))
   ((sock max-len flags malloc)
    (let ((buf.ptr (malloc max-len)))
      (receive (buf.len sockaddr)
	  (recvfrom sock buf.ptr max-len flags)
	(values (make-<memblock> buf.ptr buf.len max-len) sockaddr))))))


(define (getsockopt sock option)
  (with-compensations
    (let* ((optsym	(socket-option->symbol option))
	   (optlen	(case optsym
			  ((debug reuseaddr keepalive dontroute broadcast oobinline style type error)
			   sizeof-int)
			  ((sndbuf rcvbuf)	sizeof-size_t)
			  ((linger)		sizeof-linger)
			  (else
			   (assertion-violation 'getsockopt "invalid socket option" option))))
	   (optval*	(malloc-block/c optlen))
	   (optlen*	(malloc-small/c)))
      (pointer-set-c-socklen_t! optlen* 0 optlen)
      (receive (result errno)
	  (platform:getsockopt (file-descriptor->integer sock) SOL_SOCKET
			       (socket-option->value option) optval* optlen*)
	(if (= -1 result)
	    (raise-errno-error 'getsockopt errno (list sock))
	  (case optsym
	    ((debug reuseaddr keepalive dontroute broadcast oobinline)
	     (not (= 0 (pointer-ref-c-signed-int optval* 0))))
	    ((sndbuf rcvbuf)
	     (pointer-ref-c-size_t optval* 0))
	    ((linger)
	     (if (= 0 (struct-linger-l_onoff-ref optval*))
		 #f
	       (struct-linger-l_linger-ref optval*)))
	    ((style type)
	     (value->socket-style (pointer-ref-c-signed-int optval* 0)))
	    ((error)
	     (pointer-ref-c-signed-int optval* 0))
	    (else
	     (assertion-violation 'getsockopt "invalid socket option" option))))))))

(define (setsockopt sock option optval)
  (with-compensations
    (let* ((optsym	(socket-option->symbol option))
	   (optlen	(case optsym
			  ((debug reuseaddr keepalive dontroute broadcast oobinline)
			   sizeof-int)
			  ((sndbuf rcvbuf)	sizeof-size_t)
			  ((linger)		sizeof-linger)
			  (else
			   (assertion-violation 'setsockopt "invalid socket option" option))))
	   (optval*	(malloc-block/c optlen)))
      (case optsym
	((debug reuseaddr keepalive dontroute broadcast oobinline)
	 (pointer-set-c-signed-int! optval* 0 (if optval 1 0)))
	((sndbuf rcvbuf)
	 (pointer-set-c-size_t! optval* 0 optval))
	((linger)
	 (struct-linger-l_onoff-set!  optval* (if (and optval (< 0 optval)) 1 0))
	 (struct-linger-l_linger-set! optval* (if optval optval 0)))
	(else
	 (assertion-violation 'setsockopt "invalid socket option" option)))
      (receive (result errno)
	  (platform:setsockopt (file-descriptor->integer sock) SOL_SOCKET
			       (socket-option->value option) optval* optlen)
	(if (= -1 result)
	    (raise-errno-error 'setsockopt errno (list sock))
	  result)))))


;;;; done

)

;;; end of file
