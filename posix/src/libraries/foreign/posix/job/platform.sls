;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to platform functions for job control
;;;Date: Thu Jan  1, 2009
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


#!r6rs
(library (foreign posix job platform)
  (export
    platform-ctermid
    platform-setsid
    platform-getsid
    platform-getpgrp
    platform-setpgid
    platform-tcgetpgrp
    platform-tcsetpgrp
    platform-tcgetsid)
  (import (rnrs)
    (foreign ffi)
    (foreign posix sizeof))


;;;; code

(define-c-function/with-errno platform-ctermid
  (char* ctermid (char*)))

(define-c-function/with-errno platform-setsid
  (pid_t setsid (void)))

(define-c-function/with-errno platform-getsid
  (pid_t getsid (pid_t)))

(define-c-function/with-errno platform-getpgrp
  (pid_t getpgrp (void)))

(define-c-function/with-errno platform-setpgid
  (int setpgid (pid_t pid_t)))

(define-c-function/with-errno platform-tcgetpgrp
  (pid_t tcgetpgrp (int)))

(define-c-function/with-errno platform-tcsetpgrp
  (pid_t tcsetpgrp (int pid_t)))

(define-c-function/with-errno platform-tcgetsid
  (pid_t tcgetsid (int)))




;;;; done

)

;;; end of file
