;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/MHD
;;;Contents: tests
;;;Date: Wed Dec  2, 2009
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
  (foreign ffi)
;;;  (foreign memory)
  (foreign cstrings)
  (foreign net mhd)
  (foreign net curl)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing GNU Libmicrohttpd\n")


(parametrise ((check-test-name	'basic))

  (check
      (let ((port	8888)
	    (page	"<html><body>Hello, browser!</body></html>"))
	(with-compensations
	  (define (cb connection url method http-version upload-data.ptr upload-data-len.ptr* new?)
	    (with-compensations
	      (let* ((page-cstr	(string->cstring/c page)))
		(letrec ((response (compensate
				       (mhd-create-response-from-data (strlen page-cstr) page-cstr
								      MHD_NO MHD_NO)
				     (with
				      (mhd-destroy-response response)))))
		  (mhd-queue-response connection MHD_HTTP_OK response)))))

	  (letrec ((daemon (compensate
			       (mhd-start-daemon (mhd-flags USE_SELECT_INTERNALLY USE_DEBUG)
						 8080 #f
						 (make-mhd-access-contents-callback cb))
			     (with
			      (mhd-stop-daemon daemon)))))
	    (mhd-run daemon))))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
