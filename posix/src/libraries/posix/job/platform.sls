;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to platform functions for job control
;;;Date: Thu Jan  1, 2009
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


(library (posix job platform)
  (export
    ctermid		setsid		getsid
    getpgrp		setpgid
    tcgetpgrp		tcsetpgrp	tcgetsid)
  (import (rnrs)
    (foreign ffi)
    (posix shared-object)
    (posix sizeof))

  (define-c-functions/with-errno libc-shared-object
    (ctermid		(char* ctermid (char*)))
    (setsid		(pid_t setsid (void)))
    (getsid		(pid_t getsid (pid_t)))
    (getpgrp		(pid_t getpgrp (void)))
    (setpgid		(int setpgid (pid_t pid_t)))
    (tcgetpgrp		(pid_t tcgetpgrp (int)))
    (tcsetpgrp		(pid_t tcsetpgrp (int pid_t)))
    (tcgetsid		(pid_t tcgetsid (int)))))

;;; end of file
