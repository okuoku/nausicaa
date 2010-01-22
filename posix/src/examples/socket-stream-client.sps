;;;!ikarus
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: stream socket client example
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

(define progname "telnet.sps")

(define (main)
  (guard (E ((errno-condition? E)
	     (%display (condition-message E))
	     (%newline))
	    (else
	     (%pretty-print E)
	     (%newline)))
    (with-compensations
      (%display "client start\n")
      (receive (hostname port)
	  (parse-command-line)
	(let ((hostent (px:gethostbyname hostname)))
	  (unless hostent
	    (%display "unable to bind to socket address"))
	  (letrec
	      ((sock (compensate
			 (px:socket* (namespace inet)
				     (style stream))
		       (with
			(px:close sock)))))
	    (%display (string-append
		       "client connecting to: "
		       (px:<hostent>-name hostent)
		       ":" (number->string port) "\n"))
	    (px:connect sock
			(px:make-<sockaddr-in>
			 (car (px:<hostent>-addrlist hostent))
			 port))
	    (let loop ((line (get-line (current-input-port))))
	      (if (eof-object? line)
		  (exit 0)
		(begin
		  (px:send/string sock
				  (string-append line "\n"))
		  (display (string-append
			    "reply: "
			    (px:recv/string sock 4096)
			    "\n"))
		  (loop (get-line
			 (current-input-port))))))))))))

(define (parse-command-line)
  (let ((args (command-line)))
    (unless (= 3 (length args))
      (%display (string-append
		 "usage: " progname " <hostname> <port>")
      (exit 1)))
    (guard (E (else
	       (%display "wrong port specification\n")
	       (exit 1)))
      (values (cadr args) (string->number (caddr args))))))

(define (%display thing)
  (display thing (current-error-port)))

(define (%pretty-print thing)
  (pretty-print thing (current-error-port)))

(define (%newline)
  (newline (current-error-port)))

(main)

;;; end of file
