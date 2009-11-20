;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/cURL
;;;Contents: tests for cURL compound library
;;;Date: Fri Nov 20, 2009
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
  (foreign net curl)
  (foreign memory)
  (foreign cstrings)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing cURL\n")

(check
    (curl-global-init CURL_GLOBAL_ALL)
  => CURLE_OK)


(parametrise ((check-test-name	'version))

  (check
      (let ((s (curl-version)))
	(display s)
	(newline)
	(string? s))
    => #t)

  (check
      (let ((s (curl-version-info)))
	(display "version-info age:\t\t")
	(write (<curl-version-info>-age s))
	(newline)

	(display "version-info version:\t\t")
	(write (<curl-version-info>-version s))
	(newline)

	(display "version-info version-num:\t")
	(write (number->string (<curl-version-info>-version-num s) 16))
	(newline)

	(display "version-info host:\t\t")
	(write (<curl-version-info>-host s))
	(newline)

	(display "version-info features:\n\t")
	(write (<curl-version-info>-features s))
	(newline)

	(display "version-info ssl-version:\t")
	(write (<curl-version-info>-ssl-version s))
	(newline)

	(display "version-info ssl-version-num:\t")
	(write (<curl-version-info>-ssl-version-num s))
	(newline)

	(display "version-info libz-version:\t")
	(write (<curl-version-info>-libz-version s))
	(newline)

	(display "version-info protocols:\n\t")
	(write (<curl-version-info>-protocols s))
	(newline)

	(display "version-info ares:\t\t")
	(write (<curl-version-info>-ares s))
	(newline)

	(display "version-info ares-num:\t\t")
	(write (<curl-version-info>-ares-num s))
	(newline)

	(display "version-info iconv:\t\t")
	(write (<curl-version-info>-iconv s))
	(newline)

	(display "version-info libssh-version:\t")
	(write (<curl-version-info>-libssh-version s))
	(newline)

	(<curl-version-info>? s))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
