;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Gcrypt
;;;Contents: test loading of platform library
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


(import (nausicaa)
  (foreign ffi pointers)
  (foreign cstrings)
  (foreign crypto gcrypt platform)
  (foreign crypto gcrypt sizeof)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing Gcrypt platform\n")


(parametrise ((check-test-name	'version))

  (check
      (pointer-null? (gcry_check_version (string->cstring GCRYPT_VERSION)))
    => #f)

  #t)


;;;; done

(check-report)

;;; end of file
