;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to job control functions
;;;Date: Fri Dec 19, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (posix job)
  (export
    ctermid	ctermid-function
    setsid	setsid-function
    getsid	getsid-function
    getpgrp	getpgrp-function
    setpgid	setpgid-function
    tcgetpgrp	tcgetpgrp-function
    tcsetpgrp	tcsetpgrp-function
    tcgetsid	tcgetsid-function)
  (import (rnrs)
    (compensations)
    (posix helpers)
    (prefix (posix job primitives) primitive:))

;;; terminal identification

  (define-parametrised ctermid)

;;; process group

  (define-parametrised setsid)
  (define-parametrised getsid pid)
  (define-parametrised getpgrp)
  (define-parametrised setpgid pid pgid)

;;; terminal access

  (define-parametrised tcgetpgrp fd)
  (define-parametrised tcsetpgrp fd pgid)
  (define-parametrised tcgetsid fd)

  )

;;; end of file
