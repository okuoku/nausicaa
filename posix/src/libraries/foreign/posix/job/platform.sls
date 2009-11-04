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


(library (foreign posix job platform)
  (export
    ctermid		setsid		getsid
    getpgrp		setpgid
    tcgetpgrp		tcsetpgrp	tcgetsid)
  (import (rnrs)
    (foreign ffi)
    (foreign posix sizeof))

(define dummy
  (shared-object self-shared-object))


(define-c-function/with-errno ctermid
  (char* ctermid (char*)))

(define-c-function/with-errno setsid
  (pid_t setsid (void)))

(define-c-function/with-errno getsid
  (pid_t getsid (pid_t)))

(define-c-function/with-errno getpgrp
  (pid_t getpgrp (void)))

(define-c-function/with-errno setpgid
  (int setpgid (pid_t pid_t)))

(define-c-function/with-errno tcgetpgrp
  (pid_t tcgetpgrp (int)))

(define-c-function/with-errno tcsetpgrp
  (pid_t tcsetpgrp (int pid_t)))

(define-c-function/with-errno tcgetsid
  (pid_t tcgetsid (int)))


;;;; done

)

;;; end of file
