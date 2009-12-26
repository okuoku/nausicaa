;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Gcrypt
;;;Contents: primitive functions
;;;Date: Sat Dec 26, 2009
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


(library (foreign crypto gpg-error primitives)
  (export
    gpg-strerror
    gpg-strerror-r
    gpg-strsource
    (rename (platform:gpg_err_init			gpg-err-init)
	    (platform:gpg_err_code_from_errno		gpg-err-code-from-errno)
	    (platform:gpg_err_code_to_errno		gpg-err-code-to-errno)
	    (platform:gpg_err_code_from_syserror	gpg-err-code-from-syserror)
	    (platform:gpg_err_make			gpg-err-make)
	    (platform:gpg_error				gpg-error)
	    (platform:gpg_err_code			gpg-err-code)
	    (platform:gpg_err_source			gpg-err-source)
	    (platform:gpg_err_make_from_errno		gpg-err-make-from-errno)
	    (platform:gpg_error_from_errno		gpg-error-from-errno)
	    (platform:gpg_error_from_syserror		gpg-error-from-syserror)))
  (import (rnrs)
    (compensations)
    (foreign ffi)
    (foreign memory)
    (foreign cstrings)
    (prefix (foreign crypto gpg-error platform) platform:)
    (foreign crypto gpg-error sizeof))


(define (gpg-strerror errcode)
  (cstring->string (platform:gpg_strerror errcode)))

(define (gpg-strsource errcode)
  (cstring->string (platform:gpg_strsource errcode)))

(define (gpg-strerror-r errcode)
  ;;We go for a slam dunk  here, assuming that the error message is less
  ;;than 1024 characters long.
  ;;
  (with-compensations
    (let* ((buf.len	1024)
	   (buf.ptr	(malloc-block/c buf.len)))
      (if (= 0 (platform:gpg_strerror_r errcode buf.ptr buf.len))
	  (cstring->string buf.ptr)
	"unable to retrieve error description"))))


;;;; done

)

;;; end of file
