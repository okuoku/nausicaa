;;;
;;;Part of: Nausicaa/OSSP/sa
;;;Contents: tests
;;;Date: Sun Dec 14, 2008
;;;Time-stamp: <2008-12-15 22:21:45 marco>
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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



;;;; setup

(import (rnrs)
  (uriel printing)
  (uriel test)
  (uriel lang)
  (uriel ffi)
  (ossp-sa)
  (ossp-sa sizeof)
  (srfi receive))

(check-set-mode! 'report-failed)


;;;; address

(define the-path "unix:/tmp/proof")
(define the-addr "inet://127.0.0.1:8080")

(check
    (with-compensations
      (let ((address (make-sa-address/compensated)))
	#f))
  => #f)

(check
    (with-compensations
      (let ((address (make-sa-address/compensated the-path)))
	(sa-address-ref address)))
  => the-path)

(check
    (with-compensations
      (let ((address (make-sa-address/compensated the-addr)))
	(sa-address-ref address)))
  => the-addr)


;;;; socket

(define-c-function primitive-fork
  (int fork (void)))

(check
    (with-compensations
      (let ((server	(make-sa-socket/compensated))
	    (client	(make-sa-socket/compensated))
	    (address	(make-sa-address/compensated the-addr)))
	(sa-type server SA_TYPE_STREAM)
	(sa-type client SA_TYPE_STREAM)
	(sa-option server SA_OPTION_REUSEADDR 1)
	(sa-option client SA_OPTION_REUSEADDR 1)
	(sa-bind server address)
	(sa-listen server 1)
	(let ((pid (primitive-fork)))
	  (if (= 0 pid)
	      (begin
		;;the child
		(sa-connect client address))
	    (begin
	      ;;the parent
	      (receive (address socket)
		  (sa-accept server)
		(sa-write-string socket "hello\n")))))))
	#f))
  => #f)





;;;; done

(check-report)

;;; end of file
