;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: marshaling interface to sockets functions
;;;Date: Tue Jan 19, 2010
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


(library (glibc sockets primitives)
  (export

    ;; host names
    gethostbyname2

    )
  (import (rnrs)
    (compensations)
    (only (foreign ffi)
	  pointer-ref-c-signed-int
	  pointer-set-c-signed-int!)
    (foreign memory)
    (foreign cstrings)
    (posix typedefs)
    (posix sizeof)
    (prefix (glibc sockets platform) platform:))



;;;; host names
;;
;;The helper  functions POINTER-><HOSTENT> and  %RAISE-H-ERRNO-ERROR are
;;duplicated in (posix sockets primitives).

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

;;; --------------------------------------------------------------------

(define (gethostbyname2 host-name address-format)
  (with-compensations
    (let ((hostent* (platform:gethostbyname2 (string->cstring/c host-name)
					     (socket-address-format->value address-format))))
      (if (pointer-null? hostent*)
	  (%raise-h-errno-error 'gethostbyname2 host-name)
	(pointer-><hostent> hostent*)))))


;;;; done

)

;;; end of file
