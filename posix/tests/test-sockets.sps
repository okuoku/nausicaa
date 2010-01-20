;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for sockets functions
;;;Date: Mon Dec 21, 2009
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


(import (nausicaa)
  (pretty-print)
  (foreign cstrings)
  (foreign memory)
  (compensations)
  (posix sizeof)
  (posix typedefs)
  (prefix (posix system) posix:)
  (prefix (posix sockets) posix:)
  (prefix (posix fd) posix:)
  (prefix (posix file) posix:)
  (prefix (posix process) posix:)
  (prefix (glibc sockets) glibc:)
  (prefix (glibc time) glibc:)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing POSIX socket functions\n")

(define tcp/ip-host "127.0.0.1")
(define tcp/ip-port 8080)


(parametrise ((check-test-name	'inet-addr))

  (let ((host "255.255.255.255"))
    (check
	(posix:inet-ntoa (posix:inet-aton host))
      => host)

    (check
	(posix:inet-ntop (socket-namespace inet)
			 (posix:inet-aton host))
      => host))

  (let ((host "0.0.0.0"))
    (check
	(posix:inet-ntoa (posix:inet-aton host))
      => host)

    (check
	(posix:inet-ntop (socket-namespace inet)
			 (posix:inet-aton host))
      => host))

  (let ((host "127.0.0.1"))
    (check
	(posix:inet-ntoa (posix:inet-aton host))
      => host)

    (check
	(posix:inet-ntop (socket-namespace inet) (posix:inet-aton host))
      => host))

  (let ((host '#vu8(127 0 0 1)))
    (check
	(posix:inet-aton (posix:inet-ntoa host))
      => host))

  (let ((host "127.0.0.1"))
    (check
	(posix:inet-ntop (socket-namespace inet)
			 (posix:inet-pton (socket-namespace inet) host))
      => host))

  (let ((host "0:0:0:0:0:0:0:1"))
    (check
  	(posix:inet-ntop (socket-namespace inet6)
  			 (posix:inet-pton (socket-namespace inet6) host))
      => "::1"))

  (let ((host "::1"))
    (check
  	(posix:inet-ntop (socket-namespace inet6)
  			 (posix:inet-pton (socket-namespace inet6) host))
      => "::1"))


  #t)


(parametrise ((check-test-name	'host-names))

;;;We assume  that the system has  a "/etc/hosts" file with  at least an
;;;entry like:
;;;
;;;	127.0.0.1	rapitore.luna rapitore localhost
;;;

  (check
      (let ((hostent (posix:gethostbyname "localhost")))
;;;	(write hostent)(newline)
	(not (member "localhost" (cons* (<hostent>-name hostent)
					(<hostent>-aliases hostent)))))
    => #f)

  (check
      (let ((hostent (glibc:gethostbyname2 "localhost" (socket-address-format inet))))
;;;	(write hostent)(newline)
	(not (member "localhost" (cons* (<hostent>-name hostent)
					(<hostent>-aliases hostent)))))
    => #f)

  (check
      (let ((hostent (posix:gethostbyaddr '#vu8(127 0 0 1))))
;;;	(write hostent)(newline)
	(not (member "localhost" (cons* (<hostent>-name hostent)
					(<hostent>-aliases hostent)))))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((hostent (glibc:gethostbyname_r "localhost")))
;;;	(write hostent)(newline)
	(not (member "localhost" (cons* (<hostent>-name hostent)
					(<hostent>-aliases hostent)))))
    => #f)

  (check
      (let ((hostent (glibc:gethostbyname2_r "localhost" (socket-address-format inet))))
;;;	(write hostent)(newline)
	(not (member "localhost" (cons* (<hostent>-name hostent)
					(<hostent>-aliases hostent)))))
    => #f)

  (check
      (let ((hostent (glibc:gethostbyaddr_r '#vu8(127 0 0 1))))
;;;	(write hostent)(newline)
	(not (member "localhost" (cons* (<hostent>-name hostent)
					(<hostent>-aliases hostent)))))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	  (compensate
	      (posix:sethostent)
	    (with
	     (posix:endhostent)))
	(let ((ell '()))
	  (do ((hostent (posix:gethostent) (posix:gethostent)))
	      ((not hostent)
	       (for-all <hostent>? ell))
	    (set! ell (cons hostent ell)))))
    => #t)

  (check
      (with-compensations
	  (compensate
	      (posix:sethostent #t)
	    (with
	     (posix:endhostent)))
	(let loop ((ell		'())
		   (hostent	(posix:gethostent)))
	  (if hostent
	      (loop (cons hostent ell) (posix:gethostent))
	    (for-all <hostent>? ell))))
    => #t)

  #t)


(parametrise ((check-test-name	'net-names))

;;;We assume that the system has a "/etc/networks" file with at least an
;;;entry like:
;;;
;;;	loopback	127.0.0.0
;;;

  (check
      (let ((netent (posix:getnetbyname "loopback")))
;;;	(write netent)(newline)
	(not (member "loopback" (cons* (<netent>-name netent)
				       (<netent>-aliases netent)))))
    => #f)

  (check
      (let ((netent (posix:getnetbyaddr '#vu8(127 0 0 0))))
;;;	(write netent)(newline)
	(not (member "loopback" (cons* (<netent>-name netent)
				       (<netent>-aliases netent)))))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	  (compensate
	      (posix:setnetent #t)
	    (with
	     (posix:endnetent)))
	(let loop ((ell		'())
		   (netent	(posix:getnetent)))
	  (if netent
	      (loop (cons netent ell) (posix:getnetent))
	    (for-all <netent>? ell))))
    => #t)

  #f)


(parametrise ((check-test-name	'proto-names))

;;;We assume that  the system has a "/etc/protocols"  file with at least
;;;an entry like:
;;;
;;;	tcp	6	TCP
;;;

  (check
      (let ((protoent (posix:getprotobyname "tcp")))
	(write protoent)(newline)
	(not (member "tcp" (cons* (<protoent>-name protoent)
				  (<protoent>-aliases protoent)))))
    => #f)

  (check
      (let ((protoent (posix:getprotobynumber 6)))
	(write protoent)(newline)
	(not (member "tcp" (cons* (<protoent>-name protoent)
				  (<protoent>-aliases protoent)))))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	  (compensate
	      (posix:setprotoent #t)
	    (with
	     (posix:endprotoent)))
	(let loop ((ell		'())
		   (protoent	(posix:getprotoent)))
	  (if protoent
	      (loop (cons protoent ell) (posix:getprotoent))
	    (begin
;;;	      (pretty-print ell)(newline)
	      (for-all <protoent>? ell)))))
    => #t)

  #f)


(parametrise ((check-test-name	'serv-names))

;;;We assume that the system has a "/etc/services" file with at least an
;;;entry like:
;;;
;;;	smtp		 25/tcp	   mail		#Simple Mail Transfer
;;;

  (check
      (let ((servent (posix:getservbyname 'smtp 'tcp)))
	(write servent)(newline)
	(and (<servent>? servent)
	     (not (not (member "smtp" (cons* (<servent>-name servent)
					     (<servent>-aliases servent)))))))
    => #t)

  (check
      (let ((servent (posix:getservbyport* 25 'tcp)))
	(write servent)(newline)
	(and (<servent>? servent)
	     (not (not (member "smtp" (cons* (<servent>-name servent)
					     (<servent>-aliases servent)))))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	  (compensate
	      (posix:setservent #t)
	    (with
	     (posix:endservent)))
	(let loop ((ell		'())
		   (servent	(posix:getservent)))
	  (if servent
	      (loop (cons servent ell) (posix:getservent))
	    (begin
;;;	      (pretty-print ell)(newline)
	      (for-all <servent>? ell)))))
    => #t)

  #f)


(parametrise ((check-test-name	'if-name))

  (check
      (posix:if-indextoname (posix:if-nametoindex "eth0"))
    => "eth0")

  (check
      (posix:if-indextoname (posix:if-nametoindex "lo"))
    => "lo")

  (check
      (let ((l (posix:if-nameindex)))
;;;	(pretty-print l)(newline)
	(for-all <if-nameindex>? l))
    => #t)

  #t)


(parametrise ((check-test-name	'enumerations))

  (check (shutdown-mode->value (shutdown-mode read))	=> SHUT_RD)
  (check (shutdown-mode->value (shutdown-mode write))	=> SHUT_WR)
  (check (shutdown-mode->value (shutdown-mode both))	=> SHUT_RDWR)

  (check (value->shutdown-mode SHUT_RD)		(=> enum-set=?) (shutdown-mode read))
  (check (value->shutdown-mode SHUT_WR)		(=> enum-set=?) (shutdown-mode write))
  (check (value->shutdown-mode SHUT_RDWR)	(=> enum-set=?) (shutdown-mode both))

;;; --------------------------------------------------------------------

  (check (socket-namespace->value (socket-namespace local))	=> PF_LOCAL)
  (check (socket-namespace->value (socket-namespace unix))	=> PF_UNIX)
  (check (socket-namespace->value (socket-namespace file))	=> PF_FILE)
  (check (socket-namespace->value (socket-namespace inet))	=> PF_INET)
  (check (socket-namespace->value (socket-namespace inet6))	=> PF_INET6)
  (check (socket-namespace->value (socket-namespace unspec))	=> PF_UNSPEC)

  (check (value->socket-namespace PF_LOCAL)	(=> enum-set=?) (socket-namespace local))
  (check (value->socket-namespace PF_UNIX)	(=> enum-set=?) (socket-namespace local))
  (check (value->socket-namespace PF_FILE)	(=> enum-set=?) (socket-namespace local))
  (check (value->socket-namespace PF_INET)	(=> enum-set=?) (socket-namespace inet))
  (check (value->socket-namespace PF_INET6)	(=> enum-set=?) (socket-namespace inet6))
  (check (value->socket-namespace PF_UNSPEC)	(=> enum-set=?) (socket-namespace unspec))

;;; --------------------------------------------------------------------

  (check (socket-style->value (socket-style stream))		=> SOCK_STREAM)
  (check (socket-style->value (socket-style datagram))		=> SOCK_DGRAM)
  (check (socket-style->value (socket-style raw))		=> SOCK_RAW)
  (check (socket-style->value (socket-style seqpacket))		=> SOCK_SEQPACKET)

  (check (value->socket-style SOCK_STREAM)	(=> enum-set=?) (socket-style stream))
  (check (value->socket-style SOCK_DGRAM)	(=> enum-set=?) (socket-style datagram))
  (check (value->socket-style SOCK_RAW)		(=> enum-set=?) (socket-style raw))
  (check (value->socket-style SOCK_SEQPACKET)	(=> enum-set=?) (socket-style seqpacket))

;;; --------------------------------------------------------------------

  (check (socket-protocol->value (socket-protocol zero))	=> 0)

  (check (value->socket-protocol 0)		(=> enum-set=?) (socket-protocol zero))

;;; --------------------------------------------------------------------

  (check (socket-address-format->value (socket-address-format local))	=> AF_LOCAL)
  (check (socket-address-format->value (socket-address-format unix))	=> AF_UNIX)
  (check (socket-address-format->value (socket-address-format file))	=> AF_FILE)
  (check (socket-address-format->value (socket-address-format inet))	=> AF_INET)
  (check (socket-address-format->value (socket-address-format inet6))	=> AF_INET6)
  (check (socket-address-format->value (socket-address-format unspec))	=> AF_UNSPEC)

  (check (value->socket-address-format AF_LOCAL)	(=> enum-set=?) (socket-address-format local))
  (check (value->socket-address-format AF_UNIX)		(=> enum-set=?) (socket-address-format local))
  (check (value->socket-address-format AF_FILE)		(=> enum-set=?) (socket-address-format local))
  (check (value->socket-address-format AF_INET)		(=> enum-set=?) (socket-address-format inet))
  (check (value->socket-address-format AF_INET6)	(=> enum-set=?) (socket-address-format inet6))
  (check (value->socket-address-format AF_UNSPEC)	(=> enum-set=?) (socket-address-format unspec))

;;; --------------------------------------------------------------------

  (check (shutdown-mode->value (shutdown-mode read))	=> SHUT_RD)
  (check (shutdown-mode->value (shutdown-mode write))	=> SHUT_WR)
  (check (shutdown-mode->value (shutdown-mode both))	=> SHUT_RDWR)

  (check (value->shutdown-mode SHUT_RD)		(=> enum-set=?) (shutdown-mode read))
  (check (value->shutdown-mode SHUT_WR)		(=> enum-set=?) (shutdown-mode write))
  (check (value->shutdown-mode SHUT_RDWR)	(=> enum-set=?) (shutdown-mode both))

;;; --------------------------------------------------------------------

  (check (socket-data-options->value (socket-data-options oob))		=> MSG_OOB)
  (check (socket-data-options->value (socket-data-options peek))	=> MSG_PEEK)
  (check (socket-data-options->value (socket-data-options dontroute))	=> MSG_DONTROUTE)

  (check (value->socket-data-options MSG_OOB)		(=> enum-set=?) (socket-data-options oob))
  (check (value->socket-data-options MSG_PEEK)		(=> enum-set=?) (socket-data-options peek))
  (check (value->socket-data-options MSG_DONTROUTE)	(=> enum-set=?) (socket-data-options dontroute))

  (check
      (socket-data-options->value (socket-data-options oob peek dontroute))
    => (bitwise-ior MSG_OOB MSG_PEEK MSG_DONTROUTE))

  (check
      (socket-data-options->value (socket-data-options oob peek))
    => (bitwise-ior MSG_OOB MSG_PEEK))

  (check
      (socket-data-options->value (socket-data-options oob dontroute))
    => (bitwise-ior MSG_OOB MSG_DONTROUTE))

  (check
      (socket-data-options->value (socket-data-options peek dontroute))
    => (bitwise-ior MSG_PEEK MSG_DONTROUTE))

  (check
      (socket-data-options->value (socket-data-options))
    => 0)

  (check
      (value->socket-data-options (bitwise-ior MSG_OOB MSG_PEEK MSG_DONTROUTE))
    (=> enum-set=?) (socket-data-options oob peek dontroute))

  (check
      (value->socket-data-options (bitwise-ior MSG_OOB MSG_PEEK))
    (=> enum-set=?) (socket-data-options oob peek))

  (check
      (value->socket-data-options (bitwise-ior MSG_OOB MSG_DONTROUTE))
    (=> enum-set=?) (socket-data-options oob dontroute))

  (check
      (value->socket-data-options (bitwise-ior MSG_PEEK MSG_DONTROUTE))
    (=> enum-set=?) (socket-data-options peek dontroute))

  (check
      (value->socket-data-options 0)
    (=> enum-set=?) (socket-data-options))

;;; --------------------------------------------------------------------

  (check (socket-option->value (socket-option debug))		=> SO_DEBUG)
  (check (socket-option->value (socket-option reuseaddr))	=> SO_REUSEADDR)
  (check (socket-option->value (socket-option keepalive))	=> SO_KEEPALIVE)
  (check (socket-option->value (socket-option dontroute))	=> SO_DONTROUTE)
  (check (socket-option->value (socket-option linger))		=> SO_LINGER)
  (check (socket-option->value (socket-option broadcast))	=> SO_BROADCAST)
  (check (socket-option->value (socket-option oobinline))	=> SO_OOBINLINE)
  (check (socket-option->value (socket-option sndbuf))		=> SO_SNDBUF)
  (check (socket-option->value (socket-option rcvbuf))		=> SO_RCVBUF)
  (check (socket-option->value (socket-option style))		=> SO_STYLE)
  (check (socket-option->value (socket-option type))		=> SO_TYPE)
  (check (socket-option->value (socket-option error))		=> SO_ERROR)

  (check (value->socket-option SO_DEBUG)	(=> enum-set=?) (socket-option debug))
  (check (value->socket-option SO_REUSEADDR)	(=> enum-set=?) (socket-option reuseaddr))
  (check (value->socket-option SO_KEEPALIVE)	(=> enum-set=?) (socket-option keepalive))
  (check (value->socket-option SO_DONTROUTE)	(=> enum-set=?) (socket-option dontroute))
  (check (value->socket-option SO_LINGER)	(=> enum-set=?) (socket-option linger))
  (check (value->socket-option SO_BROADCAST)	(=> enum-set=?) (socket-option broadcast))
  (check (value->socket-option SO_OOBINLINE)	(=> enum-set=?) (socket-option oobinline))
  (check (value->socket-option SO_SNDBUF)	(=> enum-set=?) (socket-option sndbuf))
  (check (value->socket-option SO_RCVBUF)	(=> enum-set=?) (socket-option rcvbuf))
  (check (value->socket-option SO_STYLE)	(=> enum-set=?) (socket-option style))
  (check (value->socket-option SO_TYPE)		(=> enum-set=?) (socket-option style))
  (check (value->socket-option SO_ERROR)	(=> enum-set=?) (socket-option error))

  #t)


(parametrise ((check-test-name	'options))

  (define (make-sock/c)
    (letrec ((sock (compensate
		       (posix:socket* (namespace local)
				      (style stream))
		     (with
		      (posix:close sock)))))
      sock))

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(posix:getsockopt (make-sock/c) (socket-option debug)))
    => #f)

  ;;SETSOCKOPT fails with "permission denied".
  ;;
  ;; (check
  ;;     (with-compensations
  ;; 	(let ((sock (make-sock/c)))
  ;; 	  (posix:setsockopt sock (socket-option debug) #t)
  ;; 	  (posix:getsockopt sock (socket-option debug))))
  ;;   => #t)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(posix:getsockopt (make-sock/c) (socket-option reuseaddr)))
    => #f)

  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (posix:setsockopt sock (socket-option reuseaddr) #t)
	  (posix:getsockopt sock (socket-option reuseaddr))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(posix:getsockopt (make-sock/c) (socket-option keepalive)))
    => #f)

  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (posix:setsockopt sock (socket-option keepalive) #t)
	  (posix:getsockopt sock (socket-option keepalive))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(posix:getsockopt (make-sock/c) (socket-option dontroute)))
    => #f)

  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (posix:setsockopt sock (socket-option dontroute) #t)
	  (posix:getsockopt sock (socket-option dontroute))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(posix:getsockopt (make-sock/c) (socket-option broadcast)))
    => #f)

  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (posix:setsockopt sock (socket-option broadcast) #t)
	  (posix:getsockopt sock (socket-option broadcast))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(posix:getsockopt (make-sock/c) (socket-option oobinline)))
    => #f)

  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (posix:setsockopt sock (socket-option oobinline) #t)
	  (posix:getsockopt sock (socket-option oobinline))))
    => #t)

;;; --------------------------------------------------------------------

  ;; (check
  ;;     (with-compensations
  ;; 	(posix:getsockopt (make-sock/c) (socket-option sndbuf)))
  ;;   => 111616)

;;;This test fails  for no documented reason; it looks  like the size of
;;;the buffer  set by  SO_SNDBUF is a  suggestion to the  system, rather
;;;than  a  strict  order.   See  also  the  proof  C  language  program
;;;"src/proofs/socket-options.c".
;;;
  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (posix:setsockopt sock (socket-option sndbuf) 1000)
	  (posix:getsockopt sock (socket-option sndbuf))))
    => 1000)

;;; --------------------------------------------------------------------

  ;; (check
  ;;     (with-compensations
  ;; 	(posix:getsockopt (make-sock/c) (socket-option rcvbuf)))
  ;;   => 111616)

;;;This test fails  for no documented reason; it looks  like the size of
;;;the buffer  set by  SO_RCVBUF is a  suggestion to the  system, rather
;;;than  a  strict  order.   See  also  the  proof  C  language  program
;;;"src/proofs/socket-options.c".
;;;
  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (posix:setsockopt sock (socket-option rcvbuf) 1000)
	  (posix:getsockopt sock (socket-option rcvbuf))))
    => 1000)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(posix:getsockopt (make-sock/c) (socket-option linger)))
    => #f)

  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (posix:setsockopt sock (socket-option linger) 100)
	  (posix:getsockopt sock (socket-option linger))))
    => 100)

  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (posix:setsockopt sock (socket-option linger) 0)
	  (posix:getsockopt sock (socket-option linger))))
    => #f)

  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (posix:setsockopt sock (socket-option linger) 100)
	  (posix:setsockopt sock (socket-option linger) #f)
	  (posix:getsockopt sock (socket-option linger))))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(posix:getsockopt (make-sock/c) (socket-option style)))
    (=> enum-set=?) (socket-style stream))

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(posix:getsockopt (make-sock/c) (socket-option error)))
    => 0)

  #t)


(parametrise ((check-test-name	'local-stream))

  (check	;strings
      (with-result
       (with-compensations
	 (let* ((pathname	(string-append (posix:getenv "TMPDIR") "/proof"))
		(sockaddr	(make-<sockaddr-un> pathname)))
	   (push-compensation (when (file-exists? pathname)
				(posix:unlink pathname)))
	   (letrec ((master-sock	(compensate
					    (posix:socket* (namespace local) (style stream))
					  (with
					   (posix:close master-sock))))
		    (client-sock	(compensate
					    (posix:socket* (namespace local) (style stream))
					  (with
					   (posix:close client-sock)))))
	     (posix:setsockopt master-sock (socket-option reuseaddr) #t)
	     (posix:bind   master-sock sockaddr)
	     (posix:listen master-sock 2)
	     (posix:connect client-sock sockaddr)
	     (receive (server-sock client-address)
		 (posix:accept master-sock)
	       (push-compensation (posix:close server-sock))
	       (add-result (<sockaddr-un>? sockaddr))
	       (posix:send/string server-sock "ciao client")
	       (add-result (posix:recv/string client-sock 100))
	       (posix:send/string client-sock "ciao server")
	       (add-result (posix:recv/string server-sock 100))
	       (posix:send/string server-sock "quit client")
	       (add-result (posix:recv/string client-sock 100))
	       (posix:send/string client-sock "quit server")
	       (add-result (posix:recv/string server-sock 100))
	       (posix:shutdown server-sock (shutdown-mode both))
	       (posix:shutdown client-sock (shutdown-mode both))
	       #t)))))
    => '(#t (#t "ciao client" "ciao server" "quit client" "quit server")))

;;; --------------------------------------------------------------------

  (check 	;bytevectors
      (with-result
       (with-compensations
	 (let* ((pathname	(string-append (posix:getenv "TMPDIR") "/proof"))
		(sockaddr	(make-<sockaddr-un> pathname)))
	   (push-compensation (when (file-exists? pathname)
				(posix:unlink pathname)))
	   (letrec ((master-sock	(compensate
					    (posix:socket (socket-namespace local) (socket-style stream))
					  (with
					   (posix:close master-sock))))
		    (client-sock	(compensate
					    (posix:socket (socket-namespace local) (socket-style stream))
					  (with
					   (posix:close client-sock)))))
	     (posix:setsockopt master-sock (socket-option reuseaddr) #t)
	     (posix:bind   master-sock sockaddr)
	     (posix:listen master-sock 2)
	     (posix:connect client-sock sockaddr)
	     (receive (server-sock client-address)
		 (posix:accept master-sock)
	       (push-compensation (posix:close server-sock))
	       (add-result (<sockaddr-un>? sockaddr))
	       (posix:send/bytevector server-sock (string->utf8 "ciao client"))
	       (add-result (utf8->string (posix:recv/bytevector client-sock 100)))
	       (posix:send/bytevector client-sock (string->utf8 "ciao server"))
	       (add-result (utf8->string (posix:recv/bytevector server-sock 100)))
	       (posix:send/bytevector server-sock (string->utf8 "quit client"))
	       (add-result (utf8->string (posix:recv/bytevector client-sock 100)))
	       (posix:send/bytevector client-sock (string->utf8 "quit server"))
	       (add-result (utf8->string (posix:recv/bytevector server-sock 100)))
	       #t)))))
    => '(#t (#t "ciao client" "ciao server" "quit client" "quit server")))

;;; --------------------------------------------------------------------

  (check	;memblocks
      (with-result
       (with-compensations
	 (let* ((pathname	(string-append (posix:getenv "TMPDIR") "/proof"))
		(sockaddr	(make-<sockaddr-un> pathname)))
	   (push-compensation (when (file-exists? pathname)
				(posix:unlink pathname)))
	   (letrec ((master-sock	(compensate
					    (posix:socket (socket-namespace local) (socket-style stream))
					  (with
					   (posix:close master-sock))))
		    (client-sock	(compensate
					    (posix:socket (socket-namespace local) (socket-style stream))
					  (with
					   (posix:close client-sock)))))
	     (posix:setsockopt master-sock (socket-option reuseaddr) #t)
	     (posix:bind   master-sock sockaddr)
	     (posix:listen master-sock 2)
	     (posix:connect client-sock sockaddr)
	     (receive (server-sock client-address)
		 (posix:accept master-sock)
	       (push-compensation (posix:close server-sock))
	       (add-result (<sockaddr-un>? sockaddr))
	       (posix:send/memblock server-sock (string->memblock "ciao client" malloc/c))
	       (add-result (memblock->string (posix:recv/memblock client-sock 100 malloc/c)))
	       (posix:send/memblock client-sock (string->memblock "ciao server" malloc/c))
	       (add-result (memblock->string (posix:recv/memblock server-sock 100 malloc/c)))
	       (posix:send/memblock server-sock (string->memblock "quit client" malloc/c))
	       (add-result (memblock->string (posix:recv/memblock client-sock 100 malloc/c)))
	       (posix:send/memblock client-sock (string->memblock "quit server" malloc/c))
	       (add-result (memblock->string (posix:recv/memblock server-sock 100 malloc/c)))
	       #t)))))
    => '(#t (#t "ciao client" "ciao server" "quit client" "quit server")))

;;; --------------------------------------------------------------------

  (check	;processes
       (with-compensations
	 (let* ((pathname (string-append (posix:getenv "TMPDIR") "/proof"))
		(sockaddr (make-<sockaddr-un> pathname)))

	   (let ((pid (posix:fork)))
	     (if pid
		 (begin ;parent, server
		   (push-compensation (when (file-exists? pathname)
					(posix:remove pathname)))
		   (letrec ((server-sock (compensate
					     (posix:socket (socket-namespace local)
							   (socket-style stream))
					   (with
					    (posix:close server-sock)))))
		     (posix:setsockopt server-sock (socket-option reuseaddr) #t)
		     (posix:bind   server-sock sockaddr)
		     (posix:listen server-sock 2)
		     (receive (client-sock client-address)
			 (posix:accept server-sock)
		       (push-compensation (posix:close client-sock))
		       (with-result
			(add-result (<sockaddr-un>? client-address))
			(posix:send/string client-sock "ciao client")
			(add-result (posix:recv/string client-sock 100))
			(posix:send/string client-sock "quit client")
			(add-result (posix:recv/string client-sock 100))
			(receive (pid1 status)
			    (posix:waitpid pid 0)
			  (pid=? pid pid1))))))
	       (begin ;child, client
		 (letrec ((client-sock (compensate
					   (posix:socket (socket-namespace local)
							 (socket-style stream))
					 (with
					  (posix:close client-sock)))))
		   ;;Give the  server some time to setup  itself and reach
		   ;;the ACCEPT call.
		   (glibc:sleep 1)
		   (posix:connect client-sock sockaddr)
		   (posix:recv/string client-sock 100)
		   (posix:send/string client-sock "ciao server")
		   (posix:recv/string client-sock 100)
		   (posix:send/string client-sock "quit server")
		   (exit)))))))
    => '(#t (#t "ciao server" "quit server")))

  #t)


(parametrise ((check-test-name	'local-datagram))

  (define (make-sock/c)
    (letrec ((sock (compensate
		       (posix:socket* (namespace local)
				      (style datagram))
		     (with
		      (posix:close sock)))))
      sock))

  (check	;strings
      (with-result
       (with-compensations
	 (let* ((TMPDIR		(posix:getenv "TMPDIR"))
		(one-pathname	(string-append TMPDIR "/proof-one"))
		(two-pathname	(string-append TMPDIR "/proof-two")))
	   (when (file-exists? one-pathname) (posix:unlink one-pathname))
	   (when (file-exists? two-pathname) (posix:unlink two-pathname))
	   (let ((one-addr	(make-<sockaddr-un> one-pathname))
		 (two-addr	(make-<sockaddr-un> two-pathname)))
	     (push-compensation (when (file-exists? one-pathname)
				  (posix:unlink one-pathname)))
	     (push-compensation (when (file-exists? two-pathname)
				  (posix:unlink two-pathname)))
	     (let ((one-sock (make-sock/c))
		   (two-sock (make-sock/c)))
	       (posix:bind one-sock one-addr)
	       (posix:bind two-sock two-addr)
	       (posix:sendto/string one-sock "ciao i'm one" two-addr)
	       (receive (result peer-address)
		   (posix:recvfrom/string two-sock 100)
		 (add-result result)
		 (posix:sendto/string two-sock "ciao i'm two" peer-address))
	       (receive (result peer-address)
		   (posix:recvfrom/string one-sock 100)
		 (add-result result)
		 (posix:sendto/string one-sock "one quits" peer-address))
	       (receive (result peer-address)
		   (posix:recvfrom/string two-sock 100)
		 (add-result result)
		 (posix:sendto/string one-sock "two quits" peer-address))
	       (receive (result peer-address)
	       	 (posix:recvfrom/string one-sock 100)
	         (add-result result))
	       #t)))))
    => '(#t ("ciao i'm one" "ciao i'm two" "one quits" "two quits")))

;;; --------------------------------------------------------------------

  (check	;processes
      (with-result
       (with-compensations
	 (let* ((TMPDIR		(posix:getenv "TMPDIR"))
		(one-pathname	(string-append TMPDIR "/proof-one"))
		(two-pathname	(string-append TMPDIR "/proof-two")))
	   (when (file-exists? one-pathname) (posix:unlink one-pathname))
	   (when (file-exists? two-pathname) (posix:unlink two-pathname))
	   (let ((one-addr	(make-<sockaddr-un> one-pathname))
		 (two-addr	(make-<sockaddr-un> two-pathname)))
	     (push-compensation (when (file-exists? one-pathname)
				  (posix:unlink one-pathname)))
	     (push-compensation (when (file-exists? two-pathname)
				  (posix:unlink two-pathname)))
	     (let ((pid (posix:fork)))
	       (if pid
		   (let ((sock (make-sock/c))) ;parent, one
		     (posix:bind sock one-addr)
		     (posix:sendto/string sock "ciao i'm one" two-addr)
		     (receive (result peer-address)
			 (posix:recvfrom/string sock 100)
		       (add-result result)
		       (posix:sendto/string sock "one quits" peer-address))
		     (receive (result peer-address)
			 (posix:recvfrom/string sock 100)
		       (add-result result))
		     (receive (pid1 status)
			 (posix:waitpid pid 0)
		       (pid=? pid pid1)))
		 (let ((sock (make-sock/c))) ;child, two
		   (posix:bind sock two-addr)
		   (receive (result peer-address)
		       (posix:recvfrom/string sock 100)
		     (posix:sendto/string sock "ciao i'm two" peer-address))
		   (receive (result peer-address)
		       (posix:recvfrom/string sock 100)
		     (posix:sendto/string sock "two quits" peer-address))
		   (exit))))))))
    => '(#t ("ciao i'm two" "two quits")))

  #t)


(parametrise ((check-test-name	'inet-stream))

  (define (make-sock/c)
    (letrec ((sock (compensate
		       (posix:socket* (namespace inet)
				      (style stream))
		     (with
		      (posix:close sock)))))
      sock))

  ;; (check	;strings
  ;;     (with-result
  ;;      (with-compensations
  ;; 	 (let ((server-sockaddr (make-<sockaddr-in> addr tcp/ip-port)))
  ;; 	   (let ((master-sock	(make-sock/c))
  ;; 		 (client-sock	(make-sock/c)))
  ;; 	     (posix:setsockopt master-sock (socket-option reuseaddr) #t)
  ;; 	     (posix:bind   master-sock sockaddr)
  ;; 	     (posix:listen master-sock 2)
  ;; 	     (posix:connect client-sock sockaddr)
  ;; 	     (receive (server-sock client-address)
  ;; 		 (posix:accept master-sock)
  ;; 	       (push-compensation (posix:close server-sock))
  ;; 	       (add-result (<sockaddr-un>? sockaddr))
  ;; 	       (posix:send/string server-sock "ciao client")
  ;; 	       (add-result (posix:recv/string client-sock 100))
  ;; 	       (posix:send/string client-sock "ciao server")
  ;; 	       (add-result (posix:recv/string server-sock 100))
  ;; 	       (posix:send/string server-sock "quit client")
  ;; 	       (add-result (posix:recv/string client-sock 100))
  ;; 	       (posix:send/string client-sock "quit server")
  ;; 	       (add-result (posix:recv/string server-sock 100))
  ;; 	       #t)))))
  ;;   => '(#t (#t "ciao client" "ciao server" "quit client" "quit server")))

  #t)


;;;; done

(check-report)

;;; end of file
