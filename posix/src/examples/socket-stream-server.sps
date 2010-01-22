;;;!ikarus
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: example network server with stream socket
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

(define addr '#vu8(127 0 0 1))
(define port 8080)
(define max-input-len 4096)

(define (main)
  (guard (E ((errno-condition? E)
	     (fatal (condition-message E)))
	    (else
	     (%pretty-print E)))
    (%display "server start\n")
    (with-compensations
      (define hostent
	(or (px:gethostbyaddr addr)
	    (fatal "unable to bind to socket address")))
      (define sockaddr
	(px:make-<sockaddr-in>
	 (px:<hostent>-addr hostent) port))
      (define master-sock
	(compensate
	    (px:socket* (namespace inet) (style stream))
	  (with
	   (px:close master-sock))))
      (px:setsockopt master-sock
		     (px:socket-option reuseaddr) #t)
      (px:bind master-sock sockaddr)
      (%display (string-append
		 "server listening to: "
		 (px:<hostent>-name hostent)
		 ":" (number->string port) "\n"))
      (px:listen master-sock 1)
      (receive (client-sock client-addr)
	  (px:accept master-sock)
	(handle-connection client-sock client-addr)
	(exit 0)))))

(define (handle-connection client-sock client-addr)
  (with-compensations
    (define client-hostent
      (px:gethostbyaddr
       (px:<sockaddr-in>-addr client-addr)))
    (define (get-string)
      (px:recv/string client-sock max-input-len))
    (push-compensation (px:close client-sock))
    (%display (string-append
	       "server accepted connection from "
	       (px:<hostent>-name client-hostent)
	       "\n"))
    (let loop ((accu "")
	       (str  (get-string)))
      (when (< max-input-len (string-length accu))
	(fatal "server: input too long\n"))
      (%display (string-append "server received: " str))
      (let ((idx (string-index str #\newline)))
	(if idx
	    (px:send/string client-sock
			    (substring str 0 idx))
	  (loop (string-append accu str) (get-string)))))))

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
