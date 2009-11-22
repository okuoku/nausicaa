;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/cURL
;;;Contents: test downloading web pages with SSL
;;;Date: Sun Nov 22, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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
  (foreign net curl)
  (foreign net curl compensated)
  (foreign memory)
  (foreign cstrings)
  (strings)
  (irregex)
  (checks))


(check-set-mode! 'report-failed)
(display "*** proof of SSL\n")


(parametrise ((check-test-name	'gna))

  (check
      (with-compensations
	(let* ((handle	(curl-easy-init/c))
	       (out	"")
	       (cb	(lambda (buffer item-size item-count)
			  (let ((len (* item-size item-count)))
			    (set! out (string-append out (cstring->string buffer len)))
			    len))))
	  (curl-easy-setopt handle CURLOPT_URL "https://gna.org/")
	  (curl-easy-setopt handle CURLOPT_WRITEFUNCTION (curl-make-write-callback cb))
	  (curl-easy-setopt handle CURLOPT_WRITEDATA pointer-null)
	  (curl-easy-perform handle)
	  out))
    => "<html><body><p>proof page</p></body></html>\n")

  #t)


;;;; done

(check-report)

;;; end of file
