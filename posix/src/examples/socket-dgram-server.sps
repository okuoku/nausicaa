;;;!ikarus
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: example network server with datagram socket
;;;Date: Fri Jan 22, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (rnrs)
  (receive)
  (compensations)
  (only (strings) string-index)
  (pretty-print)
  (foreign errno)
  (prefix (posix typedefs) px:)
  (prefix (posix fd) px:)
  (prefix (posix sockets) px:))

(define server-addr	'#vu8(127 0 0 1))
(define server-port	8080)
(define max-input-len	4096)

(define (main)
  (guard (E ((errno-condition? E)
	     (fatal (condition-message E)))
	    (else
	     (%pretty-print E)))
    (%display "server start\n")
    (with-compensations
      (define hostent
	(or (px:gethostbyaddr server-addr)
	    (fatal "unable to bind to socket address")))
      (define sockaddr
	(px:make-<sockaddr-in>
	 (px:<hostent>-addr hostent) server-port))
      (define sock
	(compensate
	    (px:socket* (namespace inet) (style datagram))
	  (with
	   (px:close sock))))
      (px:setsockopt sock (px:socket-option reuseaddr) #t)
      (px:bind sock sockaddr)
      (%display (string-append
		 "server listening to: "
		 (px:<hostent>-name hostent)
		 ":" (number->string server-port) "\n"))
      (receive (str client-addr)
	  (px:recvfrom/string sock max-input-len)
	(%display (string-append "server received: " str))
	(px:sendto/string sock str client-addr)
	(exit 0)))))

(define (fatal . strings)
  (%display (apply string-append strings))
  (%newline)
  (exit 1))

(define (%display thing)
  (display thing (current-error-port)))

(define (%pretty-print thing)
  (pretty-print thing (current-error-port)))

(define (%newline)
  (newline (current-error-port)))

(main)

;;; end of file
