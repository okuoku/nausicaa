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
;;;Copyright (c) 2009-2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (nausicaa ffi cstrings)
  (nausicaa ffi memory)
  (nausicaa posix sizeof)
  (nausicaa posix typedefs)
  (prefix (nausicaa posix system) px.)
  (prefix (nausicaa posix sockets) px.)
  (prefix (nausicaa posix fd) px.)
  (prefix (nausicaa posix file) px.)
  (prefix (nausicaa posix process) px.)
  (prefix (nausicaa glibc sockets) glibc.)
  (prefix (nausicaa glibc time) glibc.)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing POSIX socket functions\n")

(define tcp/ip-host "127.0.0.1")
(define tcp/ip-port 8080)

(define (wait-for-awhile)
  (glibc.nanosleep* (make-<timespec> 0 (* 2 (expt 10 6)))))


(parametrise ((check-test-name	'inet-addr))

  (let ((host "255.255.255.255"))
    (check
	(px.inet-ntoa (px.inet-aton host))
      => host)

    (check
	(px.inet-ntop (socket-address-format inet)
			 (px.inet-aton host))
      => host))

  (let ((host "0.0.0.0"))
    (check
	(px.inet-ntoa (px.inet-aton host))
      => host)

    (check
	(px.inet-ntop (socket-address-format inet)
			 (px.inet-aton host))
      => host))

  (let ((host "127.0.0.1"))
    (check
	(px.inet-ntoa (px.inet-aton host))
      => host)

    (check
	(px.inet-ntop (socket-address-format inet) (px.inet-aton host))
      => host))

  (let ((host '#vu8(127 0 0 1)))
    (check
	(px.inet-aton (px.inet-ntoa host))
      => host))

  (let ((host "127.0.0.1"))
    (check
	(px.inet-ntop (socket-address-format inet)
			 (px.inet-pton (socket-address-format inet) host))
      => host))

  (let ((host "0:0:0:0:0:0:0:1"))
    (check
  	(px.inet-ntop (socket-address-format inet6)
  			 (px.inet-pton (socket-address-format inet6) host))
      => "::1"))

  (let ((host "::1"))
    (check
  	(px.inet-ntop (socket-address-format inet6)
  			 (px.inet-pton (socket-address-format inet6) host))
      => "::1"))

  #t)


(parametrise ((check-test-name	'host-names))

;;;We assume  that the system has  a "/etc/hosts" file with  at least an
;;;entry like:
;;;
;;;	127.0.0.1	rapitore.luna rapitore localhost
;;;

  (check
      (let ((hostent (px.gethostbyname "localhost")))
;;;	(write hostent)(newline)
	(not (member "localhost" (cons* (<hostent>-name hostent)
					(<hostent>-aliases hostent)))))
    => #f)

  (check
      (let ((hostent (glibc.gethostbyname2 "localhost" (socket-address-format inet))))
;;;	(write hostent)(newline)
	(not (member "localhost" (cons* (<hostent>-name hostent)
					(<hostent>-aliases hostent)))))
    => #f)

  (check
      (let ((hostent (px.gethostbyaddr '#vu8(127 0 0 1))))
;;;	(write hostent)(newline)
	(not (member "localhost" (cons* (<hostent>-name hostent)
					(<hostent>-aliases hostent)))))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((hostent (glibc.gethostbyname_r "localhost")))
;;;	(write hostent)(newline)
	(not (member "localhost" (cons* (<hostent>-name hostent)
					(<hostent>-aliases hostent)))))
    => #f)

  (check
      (let ((hostent (glibc.gethostbyname2_r "localhost" (socket-address-format inet))))
;;;	(write hostent)(newline)
	(not (member "localhost" (cons* (<hostent>-name hostent)
					(<hostent>-aliases hostent)))))
    => #f)

  (check
      (let ((hostent (glibc.gethostbyaddr_r '#vu8(127 0 0 1))))
;;;	(write hostent)(newline)
	(not (member "localhost" (cons* (<hostent>-name hostent)
					(<hostent>-aliases hostent)))))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	  (compensate
	      (px.sethostent)
	    (with
	     (px.endhostent)))
	(let ((ell '()))
	  (do ((hostent (px.gethostent) (px.gethostent)))
	      ((not hostent)
	       (for-all <hostent>? ell))
	    (set! ell (cons hostent ell)))))
    => #t)

  (check
      (with-compensations
	  (compensate
	      (px.sethostent #t)
	    (with
	     (px.endhostent)))
	(let loop ((ell		'())
		   (hostent	(px.gethostent)))
	  (if hostent
	      (loop (cons hostent ell) (px.gethostent))
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
      (let ((netent (px.getnetbyname "loopback")))
;;;	(write netent)(newline)
	(not (member "loopback" (cons* (<netent>-name netent)
				       (<netent>-aliases netent)))))
    => #f)

  (check
      (let ((netent (px.getnetbyaddr '#vu8(127 0 0 0))))
;;;	(write netent)(newline)
	(not (member "loopback" (cons* (<netent>-name netent)
				       (<netent>-aliases netent)))))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	  (compensate
	      (px.setnetent #t)
	    (with
	     (px.endnetent)))
	(let loop ((ell		'())
		   (netent	(px.getnetent)))
	  (if netent
	      (loop (cons netent ell) (px.getnetent))
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
      (let ((protoent (px.getprotobyname "tcp")))
;;;	(write protoent)(newline)
	(not (member "tcp" (cons* (<protoent>-name protoent)
				  (<protoent>-aliases protoent)))))
    => #f)

  (check
      (let ((protoent (px.getprotobynumber 6)))
;;;	(write protoent)(newline)
	(not (member "tcp" (cons* (<protoent>-name protoent)
				  (<protoent>-aliases protoent)))))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	  (compensate
	      (px.setprotoent #t)
	    (with
	     (px.endprotoent)))
	(let loop ((ell		'())
		   (protoent	(px.getprotoent)))
	  (if protoent
	      (loop (cons protoent ell) (px.getprotoent))
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
      (let ((servent (px.getservbyname 'smtp 'tcp)))
;;;	(write servent)(newline)
	(and (<servent>? servent)
	     (not (not (member "smtp" (cons* (<servent>-name servent)
					     (<servent>-aliases servent)))))))
    => #t)

  (check
      (let ((servent (px.getservbyport* 25 'tcp)))
;;;	(write servent)(newline)
	(and (<servent>? servent)
	     (not (not (member "smtp" (cons* (<servent>-name servent)
					     (<servent>-aliases servent)))))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	  (compensate
	      (px.setservent #t)
	    (with
	     (px.endservent)))
	(let loop ((ell		'())
		   (servent	(px.getservent)))
	  (if servent
	      (loop (cons servent ell) (px.getservent))
	    (begin
;;;	      (pretty-print ell)(newline)
	      (for-all <servent>? ell)))))
    => #t)

  #f)


(parametrise ((check-test-name	'if-name))

  (check
      (px.if-indextoname (px.if-nametoindex "eth0"))
    => "eth0")

  (check
      (px.if-indextoname (px.if-nametoindex "lo"))
    => "lo")

  (check
      (let ((l (px.if-nameindex)))
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

  ;; (check (socket-protocol->value (socket-protocol zero))	=> 0)

  ;; (check (value->socket-protocol 0)		(=> enum-set=?) (socket-protocol zero))

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
		       (px.socket* (namespace local)
				      (style stream))
		     (with
		      (px.close sock)))))
      sock))

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(px.getsockopt (make-sock/c) (socket-option debug)))
    => #f)

  ;;SETSOCKOPT fails with "permission denied".
  ;;
  ;; (check
  ;;     (with-compensations
  ;; 	(let ((sock (make-sock/c)))
  ;; 	  (px.setsockopt sock (socket-option debug) #t)
  ;; 	  (px.getsockopt sock (socket-option debug))))
  ;;   => #t)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(px.getsockopt (make-sock/c) (socket-option reuseaddr)))
    => #f)

  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (px.setsockopt sock (socket-option reuseaddr) #t)
	  (px.getsockopt sock (socket-option reuseaddr))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(px.getsockopt (make-sock/c) (socket-option keepalive)))
    => #f)

  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (px.setsockopt sock (socket-option keepalive) #t)
	  (px.getsockopt sock (socket-option keepalive))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(px.getsockopt (make-sock/c) (socket-option dontroute)))
    => #f)

  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (px.setsockopt sock (socket-option dontroute) #t)
	  (px.getsockopt sock (socket-option dontroute))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(px.getsockopt (make-sock/c) (socket-option broadcast)))
    => #f)

  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (px.setsockopt sock (socket-option broadcast) #t)
	  (px.getsockopt sock (socket-option broadcast))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(px.getsockopt (make-sock/c) (socket-option oobinline)))
    => #f)

  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (px.setsockopt sock (socket-option oobinline) #t)
	  (px.getsockopt sock (socket-option oobinline))))
    => #t)

;;; --------------------------------------------------------------------

  ;; (check
  ;;     (with-compensations
  ;; 	(px.getsockopt (make-sock/c) (socket-option sndbuf)))
  ;;   => 111616)

;;;This test fails  for no documented reason; it looks  like the size of
;;;the buffer  set by  SO_SNDBUF is a  suggestion to the  system, rather
;;;than  a  strict  order.   See  also  the  proof  C  language  program
;;;"src/proofs/socket-options.c".
;;;
  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (px.setsockopt sock (socket-option sndbuf) 1000)
	  (px.getsockopt sock (socket-option sndbuf))))
    => 1000)

;;; --------------------------------------------------------------------

  ;; (check
  ;;     (with-compensations
  ;; 	(px.getsockopt (make-sock/c) (socket-option rcvbuf)))
  ;;   => 111616)

;;;This test fails  for no documented reason; it looks  like the size of
;;;the buffer  set by  SO_RCVBUF is a  suggestion to the  system, rather
;;;than  a  strict  order.   See  also  the  proof  C  language  program
;;;"src/proofs/socket-options.c".
;;;
  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (px.setsockopt sock (socket-option rcvbuf) 1000)
	  (px.getsockopt sock (socket-option rcvbuf))))
    => 1000)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(px.getsockopt (make-sock/c) (socket-option linger)))
    => #f)

  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (px.setsockopt sock (socket-option linger) 100)
	  (px.getsockopt sock (socket-option linger))))
    => 100)

  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (px.setsockopt sock (socket-option linger) 0)
	  (px.getsockopt sock (socket-option linger))))
    => #f)

  (check
      (with-compensations
	(let ((sock (make-sock/c)))
	  (px.setsockopt sock (socket-option linger) 100)
	  (px.setsockopt sock (socket-option linger) #f)
	  (px.getsockopt sock (socket-option linger))))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(px.getsockopt (make-sock/c) (socket-option style)))
    (=> enum-set=?) (socket-style stream))

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(px.getsockopt (make-sock/c) (socket-option error)))
    => 0)

  #t)


(parametrise ((check-test-name	'local-stream))

  (define (make-sock/c)
    (letrec ((sock (compensate
		       (px.socket* (namespace local) (style stream))
		     (with
		      (px.close sock)))))
      sock))

  (check	;strings
      (with-result
       (with-compensations
	 (let* ((pathname	(string-append (px.getenv "TMPDIR") "/proof"))
		(sockaddr	(make-<sockaddr-un> pathname)))
	   (when (file-exists? pathname)
	     (px.unlink pathname))
	   (push-compensation (when (file-exists? pathname)
				(px.unlink pathname)))
	   (letrec ((master-sock (make-sock/c))
		    (client-sock (make-sock/c)))
	     (px.setsockopt master-sock (socket-option reuseaddr) #t)
	     (px.bind   master-sock sockaddr)
	     (px.listen master-sock 2)
	     (px.connect client-sock sockaddr)
	     (receive (server-sock client-address)
		 (px.accept master-sock)
	       (push-compensation (px.close server-sock))
	       (add-result (<sockaddr-un>? sockaddr))
	       (px.send/string server-sock "ciao client")
	       (add-result (px.recv/string client-sock 100))
	       (px.send/string client-sock "ciao server")
	       (add-result (px.recv/string server-sock 100))
	       (px.send/string server-sock "quit client")
	       (add-result (px.recv/string client-sock 100))
	       (px.send/string client-sock "quit server")
	       (add-result (px.recv/string server-sock 100))
	       (px.shutdown* server-sock both)
	       (px.shutdown* client-sock both)
	       #t)))))
    => '(#t (#t "ciao client" "ciao server" "quit client" "quit server")))

;;; --------------------------------------------------------------------

  (check 	;bytevectors
      (with-result
       (with-compensations
	 (let* ((pathname	(string-append (px.getenv "TMPDIR") "/proof"))
		(sockaddr	(make-<sockaddr-un> pathname)))
	   (when (file-exists? pathname)
	     (px.unlink pathname))
	   (push-compensation (when (file-exists? pathname)
				(px.unlink pathname)))
	   (letrec ((master-sock (make-sock/c))
		    (client-sock (make-sock/c)))
	     (px.setsockopt master-sock (socket-option reuseaddr) #t)
	     (px.bind   master-sock sockaddr)
	     (px.listen master-sock 2)
	     (px.connect client-sock sockaddr)
	     (receive (server-sock client-address)
		 (px.accept master-sock)
	       (push-compensation (px.close server-sock))
	       (add-result (<sockaddr-un>? sockaddr))
	       (px.send/bytevector server-sock (string->utf8 "ciao client"))
	       (add-result (utf8->string (px.recv/bytevector client-sock 100)))
	       (px.send/bytevector client-sock (string->utf8 "ciao server"))
	       (add-result (utf8->string (px.recv/bytevector server-sock 100)))
	       (px.send/bytevector server-sock (string->utf8 "quit client"))
	       (add-result (utf8->string (px.recv/bytevector client-sock 100)))
	       (px.send/bytevector client-sock (string->utf8 "quit server"))
	       (add-result (utf8->string (px.recv/bytevector server-sock 100)))
	       #t)))))
    => '(#t (#t "ciao client" "ciao server" "quit client" "quit server")))

;;; --------------------------------------------------------------------

  (check	;memblocks
      (with-result
       (with-compensations
	 (let* ((pathname	(string-append (px.getenv "TMPDIR") "/proof"))
		(sockaddr	(make-<sockaddr-un> pathname)))
	   (when (file-exists? pathname)
	     (px.unlink pathname))
	   (push-compensation (when (file-exists? pathname)
				(px.unlink pathname)))
	   (letrec ((master-sock (make-sock/c))
		    (client-sock (make-sock/c)))
	     (px.setsockopt master-sock (socket-option reuseaddr) #t)
	     (px.bind   master-sock sockaddr)
	     (px.listen master-sock 2)
	     (px.connect client-sock sockaddr)
	     (receive (server-sock client-address)
		 (px.accept master-sock)
	       (push-compensation (px.close server-sock))
	       (add-result (<sockaddr-un>? sockaddr))
	       (px.send/memblock server-sock (string->memblock "ciao client" malloc/c))
	       (add-result (memblock->string (px.recv/memblock client-sock 100 malloc/c)))
	       (px.send/memblock client-sock (string->memblock "ciao server" malloc/c))
	       (add-result (memblock->string (px.recv/memblock server-sock 100 malloc/c)))
	       (px.send/memblock server-sock (string->memblock "quit client" malloc/c))
	       (add-result (memblock->string (px.recv/memblock client-sock 100 malloc/c)))
	       (px.send/memblock client-sock (string->memblock "quit server" malloc/c))
	       (add-result (memblock->string (px.recv/memblock server-sock 100 malloc/c)))
	       #t)))))
    => '(#t (#t "ciao client" "ciao server" "quit client" "quit server")))

;;; --------------------------------------------------------------------

  (check	;processes
      (with-compensations
	(let* ((pathname (string-append (px.getenv "TMPDIR") "/proof"))
	       (sockaddr (make-<sockaddr-un> pathname)))
	  (when (file-exists? pathname)
	    (px.unlink pathname))
	  (let ((pid (px.fork)))
	    (if pid
		(begin ;parent, server
		  (push-compensation (when (file-exists? pathname)
				       (px.remove pathname)))
		  (letrec ((server-sock (make-sock/c)))
		    (px.setsockopt server-sock (socket-option reuseaddr) #t)
		    (px.bind   server-sock sockaddr)
		    (px.listen server-sock 2)
		    (receive (client-sock client-address)
			(px.accept server-sock)
		      (push-compensation (px.close client-sock))
		      (with-result
		       (add-result (<sockaddr-un>? client-address))
		       (px.send/string client-sock "ciao client")
		       (add-result (px.recv/string client-sock 100))
		       (px.send/string client-sock "quit client")
		       (add-result (px.recv/string client-sock 100))
		       (receive (pid1 status)
			   (px.waitpid pid 0)
			 (pid=? pid pid1))))))
	      (begin ;child, client
		(letrec ((client-sock (make-sock/c)))
		  ;;Give the  server some time to setup  itself and reach
		  ;;the ACCEPT call.
		  (wait-for-awhile)
		  (px.connect client-sock sockaddr)
		  (px.recv/string client-sock 100)
		  (px.send/string client-sock "ciao server")
		  (px.recv/string client-sock 100)
		  (px.send/string client-sock "quit server")
		  (exit)))))))
    => '(#t (#t "ciao server" "quit server")))

;;; --------------------------------------------------------------------

  (let* ((pathname (string-append (px.getenv "TMPDIR") "/proof")))
    (when (file-exists? pathname)
      (px.unlink pathname))

    (check	;get peer address
	(with-result
	 (with-compensations
	   (letrec ((sockaddr (make-<sockaddr-un> pathname)))
	     (letrec ((master-sock (make-sock/c))
		      (client-sock (make-sock/c)))
	       (px.setsockopt master-sock (socket-option reuseaddr) #t)
	       (compensate
		   (px.bind master-sock sockaddr)
		 (with
		  (when (file-exists? pathname)
		    (px.unlink pathname))))
	       (px.listen master-sock 2)
	       (px.connect client-sock sockaddr)
	       (let ((client-sockaddr (px.getsockname client-sock))
		     (server-sockaddr (px.getpeername client-sock)))
		 (add-result (<sockaddr-un>-pathname server-sockaddr))
		 (add-result (<sockaddr-un>-pathname client-sockaddr))
		 #t)))))
      => `(#t (,pathname ""))))

  #t)


(parametrise ((check-test-name	'local-datagram))

  (define (make-sock/c)
    (letrec ((sock (compensate
		       (px.socket* (namespace local)
				      (style datagram))
		     (with
		      (px.close sock)))))
      sock))

  (check	;strings
      (with-result
       (with-compensations
	 (let* ((TMPDIR		(px.getenv "TMPDIR"))
		(one-pathname	(string-append TMPDIR "/proof-one"))
		(two-pathname	(string-append TMPDIR "/proof-two")))
	   (when (file-exists? one-pathname) (px.unlink one-pathname))
	   (when (file-exists? two-pathname) (px.unlink two-pathname))
	   (let ((one-addr	(make-<sockaddr-un> one-pathname))
		 (two-addr	(make-<sockaddr-un> two-pathname)))
	     (push-compensation (when (file-exists? one-pathname)
				  (px.unlink one-pathname)))
	     (push-compensation (when (file-exists? two-pathname)
				  (px.unlink two-pathname)))
	     (let ((one-sock (make-sock/c))
		   (two-sock (make-sock/c)))
	       (px.bind one-sock one-addr)
	       (px.bind two-sock two-addr)
	       (px.sendto/string one-sock "ciao i'm one" two-addr)
	       (receive (result peer-address)
		   (px.recvfrom/string two-sock 100)
		 (add-result result)
		 (px.sendto/string two-sock "ciao i'm two" peer-address))
	       (receive (result peer-address)
		   (px.recvfrom/string one-sock 100)
		 (add-result result)
		 (px.sendto/string one-sock "one quits" peer-address))
	       (receive (result peer-address)
		   (px.recvfrom/string two-sock 100)
		 (add-result result)
		 (px.sendto/string one-sock "two quits" peer-address))
	       (receive (result peer-address)
	       	 (px.recvfrom/string one-sock 100)
	         (add-result result))
	       #t)))))
    => '(#t ("ciao i'm one" "ciao i'm two" "one quits" "two quits")))

;;; --------------------------------------------------------------------

  (check	;processes
      (with-result
       (with-compensations
	 (let* ((TMPDIR		(px.getenv "TMPDIR"))
		(one-pathname	(string-append TMPDIR "/proof-one"))
		(two-pathname	(string-append TMPDIR "/proof-two")))
	   (when (file-exists? one-pathname) (px.unlink one-pathname))
	   (when (file-exists? two-pathname) (px.unlink two-pathname))
	   (let ((one-addr	(make-<sockaddr-un> one-pathname))
		 (two-addr	(make-<sockaddr-un> two-pathname)))
	     (push-compensation (when (file-exists? one-pathname)
				  (px.unlink one-pathname)))
	     (push-compensation (when (file-exists? two-pathname)
				  (px.unlink two-pathname)))
	     (let ((pid (px.fork)))
	       (if pid
		   (let ((sock (make-sock/c))) ;parent, one
		     (px.bind sock one-addr)
		     (wait-for-awhile)
		     (px.sendto/string sock "ciao i'm one" two-addr)
		     (receive (result peer-address)
			 (px.recvfrom/string sock 100)
		       (add-result result)
		       (px.sendto/string sock "one quits" peer-address))
		     (receive (result peer-address)
			 (px.recvfrom/string sock 100)
		       (add-result result))
		     (receive (pid1 status)
			 (px.waitpid pid 0)
		       (pid=? pid pid1)))
		 (let ((sock (make-sock/c))) ;child, two
		   (px.bind sock two-addr)
		   (wait-for-awhile)
		   (receive (result peer-address)
		       (px.recvfrom/string sock 100)
		     (px.sendto/string sock "ciao i'm two" peer-address))
		   (receive (result peer-address)
		       (px.recvfrom/string sock 100)
		     (px.sendto/string sock "two quits" peer-address))
		   (exit))))))))
    => '(#t ("ciao i'm two" "two quits")))

;;; --------------------------------------------------------------------

  (let* ((TMPDIR	(px.getenv "TMPDIR"))
	 (one-pathname	(string-append TMPDIR "/proof-one"))
	 (two-pathname	(string-append TMPDIR "/proof-two")))

    (check	;get sock name
	;;Notice that  on Linux GETPEERNAME does not  work with datagram
	;;sockets, it raises the "endpoint not connected" error.
	(with-result
	 (with-compensations
	   (when (file-exists? one-pathname) (px.unlink one-pathname))
	   (when (file-exists? two-pathname) (px.unlink two-pathname))
	   (let ((one-addr (make-<sockaddr-un> one-pathname))
		 (two-addr (make-<sockaddr-un> two-pathname))
		 (one-sock (make-sock/c))
		 (two-sock (make-sock/c)))
	       (compensate
		   (px.bind one-sock one-addr)
		 (with
		  (when (file-exists? one-pathname)
		    (px.unlink one-pathname))))
	       (compensate
		   (px.bind two-sock two-addr)
		 (with
		  (when (file-exists? two-pathname)
		    (px.unlink two-pathname))))
	       (px.sendto/string one-sock "ciao i'm one" two-addr)
	       (let ((one-sockaddr (px.getsockname one-sock)))
		 (add-result (<sockaddr-un>-pathname one-sockaddr))
		 #t))))
      => `(#t (,one-pathname))))

  #t)


(parametrise ((check-test-name	'inet-stream))

  (define (make-sock/c)
    (letrec ((sock (compensate
		       (px.socket* (namespace inet)
				      (style stream))
		     (with
		      (px.close sock)))))
      sock))

  (define (make-sock6/c)
    (letrec ((sock (compensate
		       (px.socket* (namespace inet6)
				      (style stream))
		     (with
		      (px.close sock)))))
      sock))

  (check	;inet
      (with-result
       (with-compensations
  	 (let ((hostent (px.gethostbyname "localhost")))
	   (when hostent
	     (let ((sockaddr	(make-<sockaddr-in> (car (<hostent>-addrlist hostent)) tcp/ip-port))
		   (master-sock	(make-sock/c))
		   (client-sock	(make-sock/c)))
	       (px.setsockopt master-sock (socket-option reuseaddr) #t)
	       (px.bind   master-sock sockaddr)
	       (px.listen master-sock 2)
	       (px.connect client-sock sockaddr)
	       (receive (server-sock client-address)
		   (px.accept master-sock)
		 (push-compensation (px.close server-sock))
		 (px.send/string server-sock "ciao client")
		 (add-result (px.recv/string client-sock 100))
		 (px.send/string client-sock "ciao server")
		 (add-result (px.recv/string server-sock 100))
		 (px.send/string server-sock "quit client")
		 (add-result (px.recv/string client-sock 100))
		 (px.send/string client-sock "quit server")
		 (add-result (px.recv/string server-sock 100))
		 #t))))))
    => '(#t ("ciao client" "ciao server" "quit client" "quit server")))

  (check	;inet6
      (with-result
       (with-compensations
	 (let ((sockaddr	(make-<sockaddr-in6> '#vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) tcp/ip-port))
	       (master-sock	(make-sock6/c))
	       (client-sock	(make-sock6/c)))
	   (px.setsockopt master-sock (socket-option reuseaddr) #t)
	   (px.bind   master-sock sockaddr)
	   (px.listen master-sock 2)
	   (px.connect client-sock sockaddr)
	   (receive (server-sock client-address)
	       (px.accept master-sock)
	     (push-compensation (px.close server-sock))
	     (px.send/string server-sock "ciao client")
	     (add-result (px.recv/string client-sock 100))
	     (px.send/string client-sock "ciao server")
	     (add-result (px.recv/string server-sock 100))
	     (px.send/string server-sock "quit client")
	     (add-result (px.recv/string client-sock 100))
	     (px.send/string client-sock "quit server")
	     (add-result (px.recv/string server-sock 100))
	     #t))))
    => '(#t ("ciao client" "ciao server" "quit client" "quit server")))

;;; --------------------------------------------------------------------

  (check	;get peer name, inet
      (with-result
       (with-compensations
  	 (let ((hostent (px.gethostbyname "localhost")))
	   (when hostent
	     (let ((sockaddr	(make-<sockaddr-in> (car (<hostent>-addrlist hostent)) tcp/ip-port))
		   (master-sock	(make-sock/c))
		   (client-sock	(make-sock/c)))
	       (px.setsockopt master-sock (socket-option reuseaddr) #t)
	       (px.bind   master-sock sockaddr)
	       (px.listen master-sock 2)
	       (px.connect client-sock sockaddr)
	       (receive (server-sock client-address)
		   (px.accept master-sock)
		 (let ((master-sockaddr (px.getsockname master-sock))
		       (server-sockaddr (px.getsockname server-sock))
		       (client-sockaddr (px.getpeername server-sock)))
		   (add-result (px.inet-ntoa (<sockaddr-in>-addr server-sockaddr)))
		   (add-result (px.inet-ntoa (<sockaddr-in>-addr client-sockaddr)))
		   (add-result (<sockaddr-in>-port master-sockaddr))
		   #t)))))))
    => `(#t ("127.0.0.1" "127.0.0.1" ,tcp/ip-port)))

  (check	;get peer name, inet6
      (with-result
       (with-compensations
	 (let ((sockaddr	(make-<sockaddr-in6> '#vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) tcp/ip-port))
	       (master-sock	(make-sock6/c))
	       (client-sock	(make-sock6/c)))
	   (px.setsockopt master-sock (socket-option reuseaddr) #t)
	   (px.bind   master-sock sockaddr)
	   (px.listen master-sock 2)
	   (px.connect client-sock sockaddr)
	   (receive (server-sock client-address)
	       (px.accept master-sock)
	     (let ((master-sockaddr (px.getsockname master-sock))
		   (server-sockaddr (px.getsockname server-sock))
		   (client-sockaddr (px.getpeername server-sock)))
	       (add-result (px.inet-ntop (socket-address-format inet6)
					    (<sockaddr-in6>-addr server-sockaddr)))
	       (add-result (px.inet-ntop (socket-address-format inet6)
					    (<sockaddr-in6>-addr client-sockaddr)))
	       (add-result (<sockaddr-in6>-port master-sockaddr))
	       #t)))))
    => `(#t ("::1" "::1" ,tcp/ip-port)))

  #t)


(parametrise ((check-test-name	'socketpair))

  (check
      (with-result
       (with-compensations
  	 (receive (one-sock two-sock)
	     (px.socketpair* (namespace local) (style stream))
	   (push-compensation (px.close one-sock))
	   (push-compensation (px.close two-sock))
	   (px.send/string one-sock "ciao two")
	   (add-result (px.recv/string two-sock 100))
	   (px.send/string two-sock "ciao one")
	   (add-result (px.recv/string one-sock 100))
	   (px.send/string one-sock "quit two")
	   (add-result (px.recv/string two-sock 100))
	   (px.send/string two-sock "quit one")
	   (add-result (px.recv/string one-sock 100))
	   #t)))
    => '(#t ("ciao two" "ciao one" "quit two" "quit one")))

  #t)


;;;; done

(check-report)

;;; end of file
