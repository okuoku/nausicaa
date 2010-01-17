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
  (prefix (glibc time) glibc:)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing POSIX socket functions\n")


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

;;;If we do  not set the "reuseaddr" option for  the server socket, this
;;;sleep is required  to make the sockaddr-un address  available for the
;;;following  tests; without  it, we  get the  "address already  in use"
;;;error.  It is cleaner to set the "reuseaddr" option.
;;;
;;;  (glibc:sleep 1)

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
	 (let* ((TMPDIR			(posix:getenv "TMPDIR"))
		(server-pathname	(string-append TMPDIR "/proof-server"))
		(client-pathname	(string-append TMPDIR "/proof-client"))
		(server-addr		(make-<sockaddr-un> server-pathname))
		(to-addr		(make-<sockaddr-un> server-pathname))
		(client-addr		(make-<sockaddr-un> client-pathname)))
	   (push-compensation (when (file-exists? server-pathname)
				(posix:unlink server-pathname)))
	   (push-compensation (when (file-exists? client-pathname)
				(posix:unlink client-pathname)))
	   (let ((server-sock (make-sock/c))
		 (client-sock (make-sock/c)))
	     ;; (posix:setsockopt server-sock (socket-option reuseaddr) #t)
	     ;; (posix:setsockopt client-sock (socket-option reuseaddr) #t)
	     (posix:bind server-sock server-addr)
	     (posix:bind client-sock client-addr)
 	     (glibc:sleep 1)
;;;	     (posix:system "ls -l /tmp/marco/proof*")
	     (posix:sendto/string client-sock "ciao server" to-addr)
	     (receive (result peer-address)
	     	 (posix:recvfrom/string server-sock 100)
	       (add-result result)
 	     (glibc:sleep 1)
	       (posix:sendto/string server-sock "ciao client" peer-address))
	     (receive (result peer-address)
	     	 (posix:recvfrom/string client-sock 100)
	       (add-result result))
	     ;; (posix:sendto/string client-sock "quit client" to-addr)
	     ;; (receive (result peer-address)
	     ;; 	 (posix:recvfrom/string server-sock 100)
	     ;;   (add-result result)
	     ;;   (posix:sendto/string server-sock "quit server" peer-address))
	     ;; (receive (result peer-address)
	     ;; 	 (posix:recvfrom/string client-sock 100)
	     ;;   (add-result result))
	     #t))))
    => '(#t (#t "ciao server" "ciao client" "quit server" "quit client")))

;;   (check	;strings
;;       (with-result
;;        (with-compensations
;; 	 (let* ((TMPDIR		(posix:getenv "TMPDIR"))
;; 		(server-pathname	(string-append TMPDIR "/proof1"))
;; 		(client-pathname	(string-append TMPDIR "/proof2"))
;; 		(sockaddr1	(make-<sockaddr-un> server-pathname))
;; 		(sockaddr2	(make-<sockaddr-un> client-pathname)))
;; 	   (push-compensation (when (file-exists? server-pathname)
;; 				(posix:unlink server-pathname)))
;; 	   (push-compensation (when (file-exists? client-pathname)
;; 				(posix:unlink client-pathname)))
;; 	   (letrec ((sock1 (compensate
;; 				     (posix:socket (socket-namespace local) (socket-style datagram))
;; 				   (with
;; 				    (posix:close sock1))))
;; 		    (sock2 (compensate
;; 				     (posix:socket (socket-namespace local) (socket-style datagram))
;; 				   (with
;; 				    (posix:close sock2)))))
;; 	     (posix:setsockopt sock1 (socket-option reuseaddr) #t)
;; 	     (posix:setsockopt sock2 (socket-option reuseaddr) #t)
;; 	     (posix:bind sock1 sockaddr1)
;; 	     (posix:bind sock2 sockaddr2)
;; ;;;	     (posix:system "ls -l /tmp/marco")
;; 	     (glibc:sleep 1)
;; 	     (posix:sendto/string sock1 "ciao client" sockaddr2)
;; 	     (add-result (posix:recvfrom/string sock2 100))
;; 	     (posix:sendto/string sock2 "ciao server" sockaddr1)
;; 	     (add-result (posix:recvfrom/string sock1 100))
;; 	     (posix:sendto/string sock1 "quit client" sockaddr2)
;; 	     (add-result (posix:recvfrom/string sock2 100))
;; 	     (posix:sendto/string sock2 "quit server" sockaddr1)
;; 	     (add-result (posix:recvfrom/string sock1 100))
;; 	     #t))))
;;     => '(#t (#t "ciao client" "ciao server" "quit client" "quit server")))

;;; --------------------------------------------------------------------

;;   (check	;bytevectors
;;       (with-result
;;        (with-compensations
;; 	 (let* ((pathname	(string-append (posix:getenv "TMPDIR") "/proof"))
;; 		(sockaddr	(make-<sockaddr-un> pathname)))
;; 	   (push-compensation (when (file-exists? pathname)
;; 				(posix:unlink pathname)))
;; 	   (letrec ((master-sock	(compensate
;; 					    (posix:socket (socket-namespace local) (socket-style datagram))
;; 					  (with
;; 					   (posix:close master-sock))))
;; 		    (client-sock	(compensate
;; 					    (posix:socket (socket-namespace local) (socket-style datagram))
;; 					  (with
;; 					   (posix:close client-sock)))))
;; 	     (posix:bind   master-sock sockaddr)
;; 	     (posix:listen master-sock 2)
;; 	     (posix:connect client-sock sockaddr)
;; 	     (receive (server-sock client-address)
;; 		 (posix:accept master-sock)
;; 	       (push-compensation (posix:close server-sock))
;; 	       (add-result (<sockaddr-un>? sockaddr))
;; 	       (posix:sendto/bytevector server-sock (string->utf8 "ciao client"))
;; 	       (add-result (utf8->string (posix:recvfrom/bytevector client-sock 100)))
;; 	       (posix:sendto/bytevector client-sock (string->utf8 "ciao server"))
;; 	       (add-result (utf8->string (posix:recvfrom/bytevector server-sock 100)))
;; 	       (posix:sendto/bytevector server-sock (string->utf8 "quit client"))
;; 	       (add-result (utf8->string (posix:recvfrom/bytevector client-sock 100)))
;; 	       (posix:sendto/bytevector client-sock (string->utf8 "quit server"))
;; 	       (add-result (utf8->string (posix:recvfrom/bytevector server-sock 100)))
;; 	       #t)))))
;;     => '(#t (#t "ciao client" "ciao server" "quit client" "quit server")))

;; ;;; --------------------------------------------------------------------

;;   (check	;memblocks
;;       (with-result
;;        (with-compensations
;; 	 (let* ((pathname	(string-append (posix:getenv "TMPDIR") "/proof"))
;; 		(sockaddr	(make-<sockaddr-un> pathname)))
;; 	   (push-compensation (when (file-exists? pathname)
;; 				(posix:unlink pathname)))
;; 	   (letrec ((master-sock	(compensate
;; 					    (posix:socket (socket-namespace local) (socket-style datagram))
;; 					  (with
;; 					   (posix:close master-sock))))
;; 		    (client-sock	(compensate
;; 					    (posix:socket (socket-namespace local) (socket-style datagram))
;; 					  (with
;; 					   (posix:close client-sock)))))
;; 	     (posix:bind   master-sock sockaddr)
;; 	     (posix:listen master-sock 2)
;; 	     (posix:connect client-sock sockaddr)
;; 	     (receive (server-sock client-address)
;; 		 (posix:accept master-sock)
;; 	       (push-compensation (posix:close server-sock))
;; 	       (add-result (<sockaddr-un>? sockaddr))
;; 	       (posix:sendto/memblock server-sock (string->memblock "ciao client" malloc/c))
;; 	       (add-result (memblock->string (posix:recvfrom/memblock client-sock 100 malloc/c)))
;; 	       (posix:sendto/memblock client-sock (string->memblock "ciao server" malloc/c))
;; 	       (add-result (memblock->string (posix:recvfrom/memblock server-sock 100 malloc/c)))
;; 	       (posix:sendto/memblock server-sock (string->memblock "quit client" malloc/c))
;; 	       (add-result (memblock->string (posix:recvfrom/memblock client-sock 100 malloc/c)))
;; 	       (posix:sendto/memblock client-sock (string->memblock "quit server" malloc/c))
;; 	       (add-result (memblock->string (posix:recvfrom/memblock server-sock 100 malloc/c)))
;; 	       #t)))))
;;     => '(#t (#t "ciao client" "ciao server" "quit client" "quit server")))

;; ;;; --------------------------------------------------------------------

;;   (check	;processes
;;        (with-compensations
;; 	 (let* ((pathname (string-append (posix:getenv "TMPDIR") "/proof"))
;; 		(sockaddr (make-<sockaddr-un> pathname)))

;; 	   (if (posix:fork)
;; 	       (begin ;parent, server
;; 		 (push-compensation (when (file-exists? pathname)
;; 				      (posix:remove pathname)))
;; 		 (letrec ((server-sock (compensate
;; 					   (posix:socket (socket-namespace local)
;; 							 (socket-style datagram))
;; 					 (with
;; 					  (posix:close server-sock)))))
;; 		   (posix:bind   server-sock sockaddr)
;; 		   (posix:listen server-sock 2)
;; 		   (receive (client-sock client-address)
;; 		       (posix:accept server-sock)
;; 		     (push-compensation (posix:close client-sock))
;; 		     (with-result
;; 		      (add-result (<sockaddr-un>? client-address))
;; 		      (posix:sendto/string client-sock "ciao client")
;; 		      (add-result (posix:recvfrom/string client-sock 100))
;; 		      (posix:sendto/string client-sock "quit client")
;; 		      (add-result (posix:recvfrom/string client-sock 100))
;; 		      #t))))
;; 	     (begin ;child, client
;; 	       (letrec ((client-sock (compensate
;; 					 (posix:socket (socket-namespace local)
;; 						       (socket-style datagram))
;; 				       (with
;; 					(posix:close client-sock)))))
;; 		 ;;Give the  server some time to setup  itself and reach
;; 		 ;;the ACCEPT call.
;; 		 (glibc:sleep 1)
;; 		 (posix:connect client-sock sockaddr)
;; 		 (posix:recvfrom/string client-sock 100)
;; 		 (posix:sendto/string client-sock "ciao server")
;; 		 (posix:recvfrom/string client-sock 100)
;; 		 (posix:sendto/string client-sock "quit server")
;; 		 (exit))))))
;;     => '(#t (#t "ciao server" "quit server")))

  #t)


;;;; done

(check-report)

;;; end of file
