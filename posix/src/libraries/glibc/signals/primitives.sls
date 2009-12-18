;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: marshaling functions for interprocess signal callouts
;;;Date: Fri Dec 18, 2009
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


(library (glibc signals primitives)
  (export

    ;; description messages
    strsignal		psignal
    )
  (import (rnrs)
    (compensations)
    (foreign cstrings)
    (prefix (glibc signals platform) platform:))


;;;; description messages

(define (strsignal signum)
  (cstring->string (platform:strsignal signum)))

(define (psignal signum message)
  (with-compensations
    (platform:psignal signum (string->cstring/c message))))



;;;; done

)

;;; end of file
