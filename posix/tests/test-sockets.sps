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


(import (nausicaa)
  (compensations)
  (pretty-print)
  (posix sizeof)
  (posix typedefs)
  (prefix (posix system) posix:)
  (prefix (posix sockets) posix:)
  (prefix (posix sockets primitives) prims:)
  (prefix (posix fd) posix:)
  (prefix (posix file) posix:)
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
	(for-all <struct-if-nameindex>? l))
    => #t)

  #t)


(parametrise ((check-test-name	'enumerations))

  (check (socket-shutdown-mode->value (shutdown-mode read))	=> SHUT_RD)
  (check (socket-shutdown-mode->value (shutdown-mode write))	=> SHUT_WR)
  (check (socket-shutdown-mode->value (shutdown-mode both))	=> SHUT_RDWR)

  (check (value->socket-shutdown-mode SHUT_RD)		(=> enum-set=?) (shutdown-mode read))
  (check (value->socket-shutdown-mode SHUT_WR)		(=> enum-set=?) (shutdown-mode write))
  (check (value->socket-shutdown-mode SHUT_RDWR)	(=> enum-set=?) (shutdown-mode both))

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

  (check (value->socket-style SOCK_STREAM)	(=> enum-set=?) (socket-style stream))
  (check (value->socket-style SOCK_DGRAM)	(=> enum-set=?) (socket-style datagram))
  (check (value->socket-style SOCK_RAW)		(=> enum-set=?) (socket-style raw))

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

  (check (socket-shutdown-mode->value (shutdown-mode read))	=> SHUT_RD)
  (check (socket-shutdown-mode->value (shutdown-mode write))	=> SHUT_WR)
  (check (socket-shutdown-mode->value (shutdown-mode both))	=> SHUT_RDWR)

  (check (value->socket-shutdown-mode SHUT_RD)		(=> enum-set=?) (shutdown-mode read))
  (check (value->socket-shutdown-mode SHUT_WR)		(=> enum-set=?) (shutdown-mode write))
  (check (value->socket-shutdown-mode SHUT_RDWR)	(=> enum-set=?) (shutdown-mode both))

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
		       (posix:socket (socket-namespace local)
				     (socket-style stream))
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


(parametrise ((check-test-name	'local))

  (check
      (with-compensations
	(let* ((pathname	(string-append (posix:getenv "TMPDIR") "/proof"))
	       (sockaddr	(make-<struct-sockaddr-un> pathname)))
	  (push-compensation (when (file-exists? pathname)
			       (posix:remove pathname)))
	  (letrec ((master-sock	(compensate
				    (posix:socket (socket-namespace local) (socket-style stream))
				  (with
				   (posix:close master-sock))))
		   (client-sock	(compensate
				    (posix:socket (socket-namespace local) (socket-style stream))
				  (with
				   (posix:close client-sock)))))
	    (posix:bind   master-sock sockaddr)
	    (posix:listen master-sock 2)
	    (posix:connect client-sock sockaddr)
	    (receive (server-sock client-address)
		(posix:accept master-sock)
	      (push-compensation (posix:close server-sock))
(write client-address)(newline)
	      (posix:send/string server-sock "ciao client" (socket-data-options))
	      ;; (posix:recv/string client-sock 100           (socket-data-options))
	      ;; (posix:send/string client-sock "ciao server" (socket-data-options))
	      ;; (posix:recv/string server-sock 100           (socket-data-options))

	      #t))))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
