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

    ;; host names
    gethostbyname		gethostbyaddr
    sethostent			gethostent
    (rename (platform:endhostent endhostent))

    ;; networks database
    getnetbyname		getnetbyaddr
    setnetent			getnetent
    (rename (platform:endnetent endnetent))

    ;; protocols database
    getprotobyname		getprotobynumber
    setprotoent			getprotoent
    (rename (platform:endprotoent endprotoent))

    ;; services database
    getservbyname
    getservbyport		getservbyport*
    setservent			getservent
    (rename (platform:endservent endservent))

    ;; interface names
    if-nametoindex		if-indextoname
    if-nameindex

    ;; socket operations
    socket			socketpair
    shutdown			shutdown*
    connect
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

    ;; C language data structure -> record
    pointer-><sockaddr-in>	pointer-><sockaddr-in6>
    pointer-><sockaddr-un>
    pointer-><hostent>		pointer-><netent>
    pointer-><protoent>		pointer-><servent>)
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


;;;; helpers

(define (%address-bytevector->address-struct! bv addr.ptr addr.len)
  ;;Copy bytes from a bytevector to a block of raw memory.
  ;;
  (do ((i 0 (+ 1 i)))
      ((= i addr.len))
    (pointer-set-c-uint8! addr.ptr i (bytevector-u8-ref bv i))))

(define (%address-struct->address-bytevector addr.ptr addr.len)
  ;;Allocate a new bytevector and copy into it bytes from a block of raw
  ;;memory; return the bytevector.
  ;;
  (let ((bv (make-bytevector addr.len)))
    (do ((i 0 (+ 1 i)))
	((= i addr.len)
	 bv)
      (bytevector-u8-set! bv i (pointer-ref-c-uint8 addr.ptr i)))))

(define (%network-number->bytevector number)
  ;;Given  an exact integer  representing a  network address  in network
  ;;byte order (big endian), return a bytevector holding the same bytes.
  ;;
  (begin0-let ((bv (make-bytevector 4)))
    (bytevector-u32-set! bv 0 number (endianness big))))

(define (%bytevector->network-number bv)
  ;;Given a bytevector  holding a network address in  network byte order
  ;;(big endian), return an integer number representing it.
  ;;
  (bytevector-u32-ref bv 0 (endianness big)))

(define (%strncpy* dst.ptr src.ptr len)
  ;;STRNCPY does NOT set the terminating zero byte; this function does.
  ;;
  (strncpy dst.ptr src.ptr len)
  (pointer-set-c-signed-char! dst.ptr len 0))

(define-syntax %pointer->record
  (syntax-rules ()
    ((_ (?converter (?form0 ?form ...)))
     (let ((pointer (?form0 ?form ...)))
       (if (pointer-null? pointer)
	   #f
	 (?converter pointer))))))

(define (%sockaddr-un-length pathname)
  ;;Compute the number of bytes  required to hold a "struct sockaddr_un"
  ;;value embedding the given pathname (as Scheme string).
  ;;
  ;;The  "sockaddr_un" structure has  two fields:  the pathname  and the
  ;;socket family  integer; the length  of the pathname field  cannot be
  ;;trusted to be  enough, so, to be safe, we  allocate more bytes (that
  ;;is: we waste bytes with the current implementation).
  ;;
  ;;NOTE For  a correct  allocation we should  take the  UTF8 bytevector
  ;;length, add one for the terminating zero, then add the offset of the
  ;;pathname field in the structure.
  ;;
  (+ sizeof-sockaddr_un (bytevector-length (string->utf8 pathname))))

;;; --------------------------------------------------------------------
;;;The  following  helper functions  are  duplicated  in (glibc  sockets
;;;primitives).

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


;;;; internet address conversion

(define (inet-aton address-name)
  (with-compensations
    (let* ((addr.ptr	(malloc-small/c))
	   (result	(platform:inet_aton (string->cstring/c address-name) addr.ptr)))
      (if (= 0 result)
	  #f
	(%address-struct->address-bytevector addr.ptr sizeof-in_addr)))))

(define (inet-ntoa address-bytevector)
  (cstring->string (platform:inet_ntoa (bytevector-u32-native-ref address-bytevector 0))))

;;; --------------------------------------------------------------------

(define (inet-pton address-format address-name)
  (with-compensations
    (let* ((format	(socket-address-format->value address-format))
	   (addr.ptr	(malloc-small/c))
	   (result	(platform:inet_pton format (string->cstring/c address-name) addr.ptr)))
      (if (= 0 result)
	  #f
	(%address-struct->address-bytevector addr.ptr (if (= AF_INET format)
							  sizeof-in_addr
							sizeof-in6_addr))))))

(define (inet-ntop address-format address-bytevector)
  (with-compensations
    (let ((format (socket-address-format->value address-format)))
      (receive (addr.len str.len)
	  (if (= AF_INET format)
	      (values sizeof-in_addr INET_ADDRSTRLEN)
	    (values sizeof-in6_addr INET6_ADDRSTRLEN))
	(let ((addr.ptr	(malloc-small/c)))
	  (%address-bytevector->address-struct! address-bytevector addr.ptr addr.len)
	  (cstring->string (platform:inet_ntop format addr.ptr (malloc-block/c str.len) str.len)))))))


;;;; host names

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
      (%address-bytevector->address-struct! address-bytevector addr.ptr addr.len)
      (let ((hostent* (platform:gethostbyaddr addr.ptr addr.len
					      (cond ((= addr.len sizeof-in_addr)
						     AF_INET)
						    ((= addr.len sizeof-in6_addr)
						     AF_INET6)
						    (else
						     (assertion-violation 'gethostbyaddr
						       "wrong size for Internet address bytevector"
						       address-bytevector))))))
	(if (pointer-null? hostent*)
	    (%raise-h-errno-error 'gethostbyaddr address-bytevector)
	  (pointer-><hostent> hostent*))))))

;;; --------------------------------------------------------------------

(define sethostent
  (case-lambda
   (()
    (sethostent #f))
   ((stay-open?)
    (platform:sethostent (if stay-open? 1 0)))))

(define (gethostent)
  (%pointer->record (pointer-><hostent> (platform:gethostent))))


;;;; networks database

(define (pointer-><netent> netent*)
  (make-<netent> (cstring->string		(struct-netent-n_name-ref	netent*))
		 (argv->strings			(struct-netent-n_aliases-ref	netent*))
		 (value->socket-address-format	(struct-netent-n_addrtype-ref	netent*))
		 (%network-number->bytevector	(struct-netent-n_net-ref	netent*))))

(define (getnetbyname net-name)
  (with-compensations
    (%pointer->record (pointer-><netent> (platform:getnetbyname (string->cstring/c net-name))))))

(define getnetbyaddr
  (case-lambda
   ((address-bytevector)
    (getnetbyaddr address-bytevector (socket-address-format inet)))
   ((address-bytevector address-format)
    (%pointer->record (pointer-><netent>
		       (platform:getnetbyaddr (%bytevector->network-number address-bytevector)
					      (socket-address-format->value address-format)))))))

(define setnetent
  (case-lambda
   (()
    (setnetent #f))
   ((stay-open?)
    (platform:setnetent (if stay-open? 1 0)))))

(define (getnetent)
  (%pointer->record (pointer-><netent> (platform:getnetent))))


;;;; protocols database

(define (pointer-><protoent> protoent*)
  (make-<protoent> (cstring->string	(struct-protoent-p_name-ref	protoent*))
		   (argv->strings	(struct-protoent-p_aliases-ref	protoent*))
		   (struct-protoent-p_proto-ref protoent*)))

(define (getprotobyname proto-name)
  (with-compensations
    (%pointer->record (pointer-><protoent> (platform:getprotobyname (string->cstring/c proto-name))))))

(define (getprotobynumber number)
  (%pointer->record (pointer-><protoent> (platform:getprotobynumber number))))

(define setprotoent
  (case-lambda
   (()
    (setprotoent #f))
   ((stay-open?)
    (platform:setprotoent (if stay-open? 1 0)))))

(define (getprotoent)
  (%pointer->record (pointer-><protoent> (platform:getprotoent))))


;;;; services database

(define (pointer-><servent> servent*)
  (make-<servent> (cstring->string	(struct-servent-s_name-ref	servent*))
		  (argv->strings	(struct-servent-s_aliases-ref	servent*))
		  (platform:ntohs	(struct-servent-s_port-ref	servent*))
		  (cstring->string	(struct-servent-s_proto-ref	servent*))))

(define (getservbyname service-name protocol-name)
  (with-compensations
    (let ((servent* (platform:getservbyname (string->cstring/c service-name)
					    (string->cstring/c protocol-name))))
      (if (pointer-null? servent*)
	  #f
	(pointer-><servent> servent*)))))

(define (getservbyport port protocol-name)
  (with-compensations
    (let ((servent* (platform:getservbyport port (string->cstring/c protocol-name))))
      (if (pointer-null? servent*)
	  #f
	(pointer-><servent> servent*)))))

(define (getservbyport* port protocol-name)
  (getservbyport (platform:htons port) protocol-name))

(define setservent
  (case-lambda
   (()
    (setservent #f))
   ((stay-open?)
    (platform:setservent (if stay-open? 1 0)))))

(define (getservent)
  (%pointer->record (pointer-><servent> (platform:getservent))))


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
			     (error 'if-nameindex "unable to acquire list of interface names")))
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

(define-syntax shutdown*
  (syntax-rules ()
    ((_ ?sock ?mode)
     (shutdown ?sock (shutdown-mode ?mode)))))


;;;; conversion from sockaddr records to sockaddr structures

(define (%socklen-from-sock sock)
  (let ((ns (<socket>-namespace sock)))
    (cond ((= AF_UNIX  ns)	4096) ;let's play it safe
	  ((= AF_INET  ns)	sizeof-sockaddr_in)
	  ((= AF_INET6 ns)	sizeof-sockaddr_in6)
	  (else (assertion-violation '%socklen-from-sock "invalid socket namespace value" sock)))))

(define (%sockaddr->pointer&length sockaddr malloc)
  (cond ((<sockaddr-un>?  sockaddr)	(<sockaddr-un>->pointer&length  sockaddr malloc))
	((<sockaddr-in>?  sockaddr)	(<sockaddr-in>->pointer&length  sockaddr malloc))
	((<sockaddr-in6>? sockaddr)	(<sockaddr-in6>->pointer&length sockaddr malloc))
	(else (assertion-violation '%sockaddr->pointer&length "invalid socket address object" sockaddr))))

(define (<sockaddr-un>->pointer&length sockaddr malloc)
  (let* ((pathname  (<sockaddr-un>-path sockaddr))
	 (sockaddr* (malloc (%sockaddr-un-length pathname))))
    (with-compensations
      (let* ((cstr.ptr	(string->cstring/c pathname))
	     (cstr.len	(strlen cstr.ptr)))
	(struct-sockaddr_un-sun_family-set! sockaddr* AF_LOCAL)
	(%strncpy* (struct-sockaddr_un-sun_path-ref sockaddr*) cstr.ptr cstr.len)
	(values sockaddr* (platform:SUN_LEN sockaddr*))))))

(define (<sockaddr-in>->pointer&length sockaddr malloc)
  (let ((sockaddr* (malloc sizeof-sockaddr_in)))
    (struct-sockaddr_in-sin_family-set! sockaddr* AF_INET)
    (struct-sockaddr_in-sin_port-set!   sockaddr* (platform:htons (<sockaddr-in>-port sockaddr)))
    (with-compensations
      (let ((in_addr* (struct-sockaddr_in-sin_addr-ref sockaddr*)))
	(%address-bytevector->address-struct! (<sockaddr-in>-addr sockaddr) in_addr* sizeof-in_addr)
	(values sockaddr* sizeof-sockaddr_in)))))

(define (<sockaddr-in6>->pointer&length sockaddr malloc)
  (let ((sockaddr* (malloc sizeof-sockaddr_in6)))
    (struct-sockaddr_in6-sin6_family-set! sockaddr* AF_INET6)
    (struct-sockaddr_in6-sin6_port-set!   sockaddr* (platform:htons (<sockaddr-in6>-port sockaddr)))
    (with-compensations
      (let ((in6_addr* (struct-sockaddr_in6-sin6_addr-ref sockaddr*)))
	(%address-bytevector->address-struct! (<sockaddr-in6>-addr sockaddr) in6_addr* sizeof-in6_addr)
	(values sockaddr* sizeof-sockaddr_in6)))))


;;;; conversion from sockaddr structures to sockaddr records

(define (pointer->sockaddr sockaddr*)
  (let ((address-format (struct-sockaddr-sa_family-ref sockaddr*)))
    (cond ((= address-format AF_LOCAL)	(pointer-><sockaddr-un>  sockaddr*))
	  ((= address-format AF_INET)	(pointer-><sockaddr-in>  sockaddr*))
	  ((= address-format AF_INET6)	(pointer-><sockaddr-in6> sockaddr*))
	  (else (assertion-violation 'pointer->sockaddr
		  "unrecognised address format tag in sockaddr structure" address-format)))))

(define (pointer-><sockaddr-un> sockaddr*)
  (make-<sockaddr-un> (cstring->string (struct-sockaddr_un-sun_path-ref sockaddr*))))

(define (pointer-><sockaddr-in> sockaddr*)
  (make-<sockaddr-in>
   (pointer->bytevector (struct-sockaddr_in-sin_addr-ref sockaddr*) sizeof-in_addr)
   (platform:ntohs (struct-sockaddr_in-sin_port-ref sockaddr*))))

(define (pointer-><sockaddr-in6> sockaddr*)
  (make-<sockaddr-in6>
   (pointer->bytevector (struct-sockaddr_in6-sin6_addr-ref sockaddr*) sizeof-in6_addr)
   (platform:ntohs (struct-sockaddr_in6-sin6_port-ref sockaddr*))))


;;;; addresses

(define (bind sock sockaddr)
  (with-compensations
    (receive (sockaddr* socklen)
	(%sockaddr->pointer&length sockaddr malloc-block/c)
      (receive (result errno)
	  (platform:bind (file-descriptor->integer sock) sockaddr* socklen)
	(when (= -1 result)
	  (raise-errno-error 'bind errno (list sock sockaddr)))))))

(define (connect sock sockaddr)
  (with-compensations
    (receive (sockaddr* socklen)
	(%sockaddr->pointer&length sockaddr malloc-block/c)
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
	       (error 'accept "sockaddr structure of source address too big" socklen))
	      (else
	       (values (make-<socket> result
				      (<socket>-namespace sock)
				      (<socket>-style     sock)
				      (<socket>-protocol  sock))
		       (pointer->sockaddr sockaddr*))))))))


(define (%sockaddr&socklen-pointers sock)
  (let* ((socklen	(%socklen-from-sock sock))
	 (sockaddr*	(malloc-block/c socklen))
	 (socklen*	(malloc-small/c)))
    (pointer-set-c-socklen_t! socklen* socklen)
    (values sockaddr* socklen*)))

(define (getsockname sock)
  (with-compensations
    (receive (sockaddr* socklen*)
	(%sockaddr&socklen-pointers sock)
      (receive (result errno)
	  (platform:getsockname (file-descriptor->integer sock) sockaddr* socklen*)
	(cond ((= -1 result)
	       (raise-errno-error 'getsockname errno sock))
	      ((< (%socklen-from-sock sock) (pointer-ref-c-socklen_t socklen*))
	       (error 'getsockname "sockaddr structure of source address too big"
		      (%socklen-from-sock sock)))
	      (else
	       (pointer->sockaddr sockaddr*)))))))

(define (getpeername sock)
  (with-compensations
    (receive (sockaddr* socklen*)
	(%sockaddr&socklen-pointers sock)
      (receive (result errno)
	  (platform:getpeername (file-descriptor->integer sock) sockaddr* socklen*)
	(cond ((= -1 result)
	       (raise-errno-error 'getpeername errno sock))
	      ((< (%socklen-from-sock sock) (pointer-ref-c-socklen_t socklen*))
	       (error 'getpeername "sockaddr structure of source address too big"
		      (%socklen-from-sock sock)))
	      (else
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
      (send sock (bytevector->pointer bv malloc-block/c) (bytevector-length bv) flags)))))

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
      (let ((buf.ptr (malloc-block/c max-len)))
	(cstring->string buf.ptr (recv sock buf.ptr max-len flags)))))))

(define recv/bytevector
  (case-lambda
   ((sock max-len)
    (recv/bytevector sock max-len (socket-data-options)))
   ((sock max-len flags)
    (with-compensations
      (let ((buf.ptr (malloc-block/c max-len)))
	(pointer->bytevector buf.ptr (recv sock buf.ptr max-len flags)))))))

(define recv/memblock
  (case-lambda
   ((sock max-len malloc)
    (recv/memblock sock max-len (socket-data-options) malloc))
   ((sock max-len flags malloc)
    (let ((buf.ptr (malloc max-len)))
      (make-<memblock> buf.ptr (recv sock buf.ptr max-len flags) max-len)))))


(define sendto
  (case-lambda
   ((sock buf.ptr buf.len sockaddr)
    (sendto sock buf.ptr buf.len (socket-data-options) sockaddr))
   ((sock buf.ptr buf.len flags sockaddr)
    (with-compensations
      (receive (sockaddr* socklen)
	  (%sockaddr->pointer&length sockaddr malloc-block/c)
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
      (sendto sock (pointer->bytevector bv malloc-block/c) (bytevector-length bv) flags sockaddr)))))

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
		 (error 'recvfrom "sockaddr structure of source address too big" socklen))
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
	(receive (buf.len sockaddr-from-which)
	    (recvfrom sock buf.ptr max-len flags)
	  (values (pointer->bytevector buf.ptr buf.len) sockaddr-from-which)))))))

(define recvfrom/memblock
  (case-lambda
   ((sock max-len malloc)
    (recvfrom/memblock sock max-len (socket-data-options) malloc))
   ((sock max-len flags malloc)
    (let ((buf.ptr (malloc max-len)))
      (receive (buf.len sockaddr-from-which)
	  (recvfrom sock buf.ptr max-len flags)
	(values (make-<memblock> buf.ptr buf.len max-len) sockaddr-from-which))))))


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
