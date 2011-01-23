;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for cstrings
;;;Date: Thu Dec 17, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009-2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(import (nausicaa)
  (nausicaa checks)
  (prefix (nausicaa ffi cstrings) ffi)
  (prefix (nausicaa glibc cstrings platform) cstr.))

(check-set-mode! 'report-failed)
(display "*** testing Glibc strings\n")


(parametrise ((check-test-name	'length))

  (check
      (with-compensations
        (cstr.strlen (ffi.string->cstring/c "ciao")))
    => 4)

  #t)


;;;; done

(check-report)

;;; end of file
