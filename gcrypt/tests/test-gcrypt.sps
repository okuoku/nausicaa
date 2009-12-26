;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Gcrypt
;;;Contents: test high-level API
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
  (foreign crypto gcrypt)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing Gcrypt platform\n")

(assert (gcry-check-version))
(gcry-control/int GCRYCTL_DISABLE_SECMEM 0)
(gcry-control/int GCRYCTL_INITIALIZATION_FINISHED 0)


(parametrise ((check-test-name	'version))

  (check
      #f
    => #f)

  #t)


;;;; done

(check-report)

;;; end of file
