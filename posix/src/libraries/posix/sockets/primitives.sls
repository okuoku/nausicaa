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


(library (posix sockets primitives)
  (export
    ;; Internet address conversion
    inet-aton		inet-pton
    inet-ntoa		inet-ntop
    (rename (platform:htons		htons)
	    (platform:htonl		htonl)
	    (platform:ntohs		ntohs)
	    (platform:ntohl		ntohl))

    ;; Host names
    gethostbyname		gethostbyaddr

    ;; interface names
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

    ;; ancillary messages
    ;; cmsg
    ;; sendmsg		recvmsg

    ;; networks database
    ;; getnetbyname	getnetbyaddr
    ;; setnetent		getnetent	endnetent

    pointer-><sockaddr-in>
    pointer-><sockaddr-in6>
    pointer-><sockaddr-un>

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


;;;; Internet address conversion

(define (inet-aton address-name)
  (with-compensations
    (let* ((cstr*	(string->cstring/c address-name))
	   (addr.ptr	(malloc-small/c))
	   (result	(platform:inet_aton cstr* addr.ptr)))
      (if (= 0 result)
	  #f
	(let ((bv (make-bytevector sizeof-in_addr)))
	  (do ((i 0 (+ 1 i)))
	      ((= i sizeof-in_addr)
	       bv)
	    (bytevector-u8-set! bv i (pointer-ref-c-uint8 addr.ptr i))))))))

(define (inet-ntoa address-bytevector)
  (cstring->string (platform:inet_ntoa
		    (bytevector-u32-ref (u8-list->bytevector
					 (list (bytevector-u8-ref address-bytevector 3)
					       (bytevector-u8-ref address-bytevector 2)
					       (bytevector-u8-ref address-bytevector 1)
					       (bytevector-u8-ref address-bytevector 0)))
					0 (endianness big)))))

;;; --------------------------------------------------------------------

(define (inet-pton namespace address-name)
  (with-compensations
    (let* ((cstr*	(string->cstring/c address-name))
	   (family	(socket-namespace->value namespace))
	   (addr.ptr	(malloc-small/c))
	   (result	(platform:inet_pton family cstr* addr.ptr)))
      (if (= 0 result)
	  #f
	(let* ((addr.len	(if (= AF_INET family) sizeof-in_addr sizeof-in6_addr))
	       (bv		(make-bytevector addr.len)))
	  (do ((i 0 (+ 1 i)))
	      ((= i addr.len)
	       bv)
	    (bytevector-u8-set! bv i (pointer-ref-c-uint8 addr.ptr i))))))))

(define (inet-ntop namespace address-bytevector)
  (with-compensations
    (let ((family (socket-namespace->value namespace)))
      (let-values (((addr.len addr.ptr str.len)
		    (if (= AF_INET family)
			(values sizeof-in_addr
				(malloc-small/c)
				INET_ADDRSTRLEN)
		      (values sizeof-in6_addr
			      (malloc-small/c)
			      INET6_ADDRSTRLEN))))
	(let ((str.ptr (malloc-block/c str.len)))
	  (do ((i 0 (+ 1 i)))
	      ((= i addr.len)
	       (cstring->string (platform:inet_ntop family addr.ptr str.ptr str.len)))
	    (pointer-set-c-uint8! addr.ptr i (bytevector-u8-ref address-bytevector i))))))))


;;;; Host names
;;
;;The helper  functions POINTER-><HOSTENT> and  %RAISE-H-ERRNO-ERROR are
;;duplicated in (glibc sockets primitives).

(define (pointer-><hostent> hostent*)
  (make-<hostent> (cstring->string	(struct-hostent-h_name-ref hostent*))
		  (argv->strings	(struct-hostent-h_aliases-ref hostent*))
		  (value->socket-address-format (struct-hostent-h_addrtype-ref hostent*))
		  (let ((addr**		(struct-hostent-h_addr_list-ref hostent*))
			(addr.len	(struct-hostent-h_length-ref hostent*)))
		    (let loop ((i 0) (ell '()))
		      (let ((addr.ptr (array-ref-c-pointer addr** i)))
			(if (pointer-null? addr.ptr)
			    ell
			  (loop (+ 1 i) (cons (pointer->bytevector addr.ptr addr.len) ell))))))))

(define (%raise-h-errno-error who . irritants)
  (apply error who
	 (let ((h_errno (platform:h_errno)))
	   (cond ((= h_errno HOST_NOT_FOUND)
		  "hostname resolution error, host not found")
		 ((= h_errno TRY_AGAIN)
		  "hostname resolution error, try again")
		 ((= h_errno NO_RECOVERY)
		  "hostname resolution non recoverable error")
		 ((= h_errno NO_ADDRESS)
		  "hostname resolution error, no Internet address associated to host name")
		 (else
		  (assertion-violation '%raise-h-errno-error "unknown h_errno value" h_errno))))
	 irritants))

;;; --------------------------------------------------------------------

(define (gethostbyname host-name)
  (with-compensations
    (let ((hostent* (platform:gethostbyname (string->cstring/c host-name))))
      (if (pointer-null? hostent*)
	  (%raise-h-errno-error 'gethostbyname host-name)
	(pointer-><hostent> hostent*)))))

(define (gethostbyaddr address-bytevector)
  (with-compensations
    (let* ((addr.ptr	(malloc-small/c))
	   (addr.len	(bytevector-length address-bytevector)))
      (assert (or (= addr.len sizeof-in_addr) (= addr.len sizeof-in6_addr)))
      (do ((i 0 (+ 1 i)))
	  ((= i addr.len))
	(pointer-set-c-uint8! addr.ptr i (bytevector-u8-ref address-bytevector i)))
      (let ((hostent* (platform:gethostbyaddr addr.ptr addr.len (if (= addr.len sizeof-in_addr)
								    AF_INET
								  AF_INET6))))
	(if (pointer-null? hostent*)
	    (%raise-h-errno-error 'gethostbyaddr address-bytevector)
	  (pointer-><hostent> hostent*))))))


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
      	  	(cons (make-<if-nameindex>
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
      (platform:shutdown (file-descriptor->integer sock) (shutdown-mode->value how))
    (when (= -1 result)
      (raise-errno-error 'shutdown errno (list sock how)))))


;;;; conversion between sockaddr structures and records

(define (%socklen-from-sock sock)
  (let ((ns (<socket>-namespace sock)))
    (cond ((= AF_UNIX ns)
	   4096) ;let's play it safe
	  ((= AF_INET ns)
	   sizeof-sockaddr_in)
	  ((= AF_INET6 ns)
	   sizeof-sockaddr_in6)
	  (else
	   (assertion-violation '%socklen-from-sock "invalid socket namespace value" sock)))))

(define (%sockaddr->pointer&length sockaddr malloc)
  (cond ((<sockaddr-un>? sockaddr)
	 (<sockaddr-un>->pointer&length sockaddr malloc))
	((<sockaddr-in>? sockaddr)
	 (<sockaddr-in>->pointer&length sockaddr malloc))
	((<sockaddr-in6>? sockaddr)
	 (<sockaddr-in6>->pointer&length sockaddr malloc))
	(else
	 (assertion-violation '%sockaddr->pointer&length
	   "invalid socket address object" sockaddr))))

(define (<sockaddr-un>->pointer&length sockaddr malloc)
  (with-compensations
    (let* ((cstr.ptr	(string->cstring/c (<sockaddr-un>-path sockaddr)))
	   (cstr.len	(strlen cstr.ptr))
	   ;;The  "sockaddr_un" structure has  two fields:  the pathname
	   ;;and the socket family  integer.  The length of the pathname
	   ;;field cannot  be trusted to be  enough, so, to  be safe, we
	   ;;allocate more bytes.
	   (sockaddr*	(malloc (+ sizeof-sockaddr_un cstr.len))))
      (struct-sockaddr_un-sun_family-set! sockaddr* (<sockaddr-un>-family sockaddr))
      (strncpy (struct-sockaddr_un-sun_path-ref sockaddr*) cstr.ptr cstr.len)
      (pointer-set-c-signed-char! (struct-sockaddr_un-sun_path-ref sockaddr*) cstr.len 0)
      (values sockaddr* (platform:SUN_LEN sockaddr*)))))

(define (<sockaddr-in>->pointer&length sockaddr malloc)
  (begin0-let ((sockaddr* (malloc sizeof-sockaddr_in)))
    (struct-sockaddr_in-sin_family-set! sockaddr* (<sockaddr-in>-family sockaddr))
    (struct-sockaddr_in-sin_port-set!   sockaddr* (<sockaddr-in>-port sockaddr))
    (with-compensations
      (let ((in_addr*	(struct-sockaddr_in-sin_addr-ref sockaddr*))
	    (bv		(<sockaddr-in>-addr sockaddr)))
	(do ((i 0 (+ 1 i)))
	    ((= i sizeof-in_addr)
	     (values sockaddr* sizeof-sockaddr_in))
	  (pointer-set-c-uint8! in_addr* i (bytevector-u8-ref bv i)))))))

(define (<sockaddr-in6>->pointer&length sockaddr malloc)
  (begin0-let ((sockaddr* (malloc sizeof-sockaddr_in6)))
    (struct-sockaddr_in6-sin6_family-set! sockaddr* (<sockaddr-in6>-family sockaddr))
    (struct-sockaddr_in6-sin6_port-set!   sockaddr* (<sockaddr-in6>-port sockaddr))
    (with-compensations
      (let ((in6_addr*	(struct-sockaddr_in6-sin6_addr-ref sockaddr*))
	    (bv		(<sockaddr-in>-addr sockaddr)))
	(do ((i 0 (+ 1 i)))
	    ((= i sizeof-in6_addr)
	     (values sockaddr* sizeof-sockaddr_in6))
	  (pointer-set-c-uint8! in6_addr* i (bytevector-u8-ref bv i)))))))

;;; --------------------------------------------------------------------

(define (pointer->sockaddr sockaddr*)
  (let ((namespace (struct-sockaddr-sa_family-ref sockaddr*)))
    (cond ((or (= namespace AF_LOCAL)
	       (= namespace AF_UNIX)  ;redundant
	       (= namespace AF_FILE)) ;redundant
	   (pointer-><sockaddr-un> sockaddr*))
	  ((= namespace AF_INET)
	   (pointer-><sockaddr-in> sockaddr*))
	  ((= namespace AF_INET6)
	   (pointer-><sockaddr-in6> sockaddr*))
	  (else
	   (assertion-violation 'pointer->sockaddr
	     "invalid namespace in sockaddr structure" namespace)))))

(define (pointer-><sockaddr-un> sockaddr*)
  (make-<sockaddr-un> (cstring->string (struct-sockaddr_un-sun_path-ref sockaddr*))))

(define (pointer-><sockaddr-in> sockaddr*)
  (make-<sockaddr-in>
   (pointer->bytevector (struct-sockaddr_in-sin_addr-ref sockaddr*) sizeof-in_addr)
   (struct-sockaddr_in-sin_port-ref sockaddr*)))

(define (pointer-><sockaddr-in6> sockaddr*)
  (make-<sockaddr-in6>
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
    (let* ((socklen	(max sizeof-sockaddr_in sizeof-sockaddr_in6))
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
    (let* ((socklen	(%socklen-from-sock sock))
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
    (let* ((socklen	(%socklen-from-sock sock))
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
	;;We do NOT want to send the terminating zero.
	(send sock buf.ptr (strlen buf.ptr)) flags)))))

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
	;;We do NOT want to send the terminating zero.
	(sendto sock buf.ptr (strlen buf.ptr) flags sockaddr))))))

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
	(pointer-set-c-socklen_t! socklen* 0 socklen)
	(receive (result errno)
	    (platform:recvfrom (file-descriptor->integer sock)
			       buf.ptr buf.len
			       (socket-data-options->value flags)
			       sockaddr* socklen*)
	  (cond ((= -1 result)
		 (raise-errno-error 'recvfrom errno (list sock buf.ptr buf.len flags)))
		((< socklen (pointer-ref-c-socklen_t socklen* 0))
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
	(receive (buf.len sockaddr-from-which)
	    (recvfrom sock buf.ptr max-len flags)
	  (values (cstring->string buf.ptr buf.len) sockaddr-from-which)))))))

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
