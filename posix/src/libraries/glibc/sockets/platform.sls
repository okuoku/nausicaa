;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: callouts for the sockets functions
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


(library (glibc sockets platform)
  (export
    gethostbyname2		gethostbyname2_r
    gethostbyname_r		gethostbyaddr_r
    h_errno)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (posix shared-object)
    (posix sizeof))

  (define h_errno_pointer
    (lookup-shared-object* libc-shared-object "h_errno"))
  (define (h_errno)
    (pointer-ref-c-signed-int h_errno_pointer 0))

  (define-c-functions libc-shared-object
    (gethostbyname2		(pointer gethostbyname2 (char* int)))
    (gethostbyname_r		(int gethostbyname_r (char* void* char* size_t void* void*)))
    (gethostbyname2_r		(int gethostbyname2_r (char* int void* char* size_t void* void*)))
    (gethostbyaddr_r		(int gethostbyaddr_r
				     (char* size_t int void* void* size_t void* void*)))))

;;; end of file
