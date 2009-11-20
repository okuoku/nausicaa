;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/cURL
;;;Contents: tests for cURL platform bindings
;;;Date: Sat Oct 24, 2009
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
  (foreign net curl platform)
  (foreign net curl sizeof)
  (foreign memory)
  (foreign cstrings)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing cURL platform\n")

(check
    (curl_global_init CURL_GLOBAL_ALL)
  => CURLE_OK)


(parametrise ((check-test-name	'version))

  (check
      (let ((s (cstring->string (curl_version))))
	(display s)
	(newline)
	(string? s))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
