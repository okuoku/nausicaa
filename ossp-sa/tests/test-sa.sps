;;;
;;;Part of: Nausicaa/OSSP/sa
;;;Contents: tests
;;;Date: Sun Dec 14, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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
  (uriel lang)
  (uriel foreign)
  (uriel test)
  (ossp-sa)
  (ossp-sa sizeof))

(check-set-mode! 'report-failed)

(format (current-error-port)
  "~%*** WARNING *** Remember to turn off firewall rules for 127.0.0.1:8080!!!~%~%")



(parameterize ((testname 'adress))

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

  )



(parameterize ((testname 'socket))

  (define d (shared-object self-shared-object))
  (define-c-function primitive-fork
    (int fork (void)))

  (define the-addr "inet://127.0.0.1:8080")

  (check
      (with-result
       (with-compensations
	 (let ((server	(make-sa-socket/compensated))
	       (client	(make-sa-socket/compensated))
	       (address	(make-sa-address/compensated the-addr)))
	   (sa-type server SA_TYPE_STREAM)
	   (sa-type client SA_TYPE_STREAM)
	   (sa-option server SA_OPTION_REUSEADDR 1)
	   (sa-option client SA_OPTION_REUSEADDR 1)
	   (sa-bind server address)
	   (sa-listen server 5)
	   (do ((i 0 (+ 1 i)))
	       ((= i 10000))
	     #f)
	   (let ((pid (primitive-fork)))
	     (debug "pid ~s~%" pid)
	     (if (= 0 pid)
		 (begin ;;the child
		   (debug "child: start~%")
		   (sa-connect client address)
		   (debug "child: connected~%")
		   (let ((got (sa-read-string client 1024)))
		     (debug "child: writing string~%")
		     (sa-write-string client got)
		     (sa-flush client))
		   (sa-shutdown client "rw")
		   (debug "child: exit~%")
		   (exit))
	       (begin ;;the parent
		 (debug "parent: start~%")
		 (receive (address socket)
		     (sa-accept server)
		   (debug "parent: accepted connection~%")
		   (sa-write-string socket "hello\n")
		   (sa-flush socket)
		   (debug "parent: written hello~%")
		   (add-result (sa-read-string socket 1024))
		   (debug "parent: read answer~%")
		   (sa-shutdown socket "rw")
		   (sa-shutdown server "rw")
		   #t)))))))
    => '(#t ("hello\n")))

  )


;;;; done

(check-report)

;;; end of file
