;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: parametrised API for glibc system inspection
;;;Date: Tue Dec 15, 2009
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


(library (foreign glibc system)
  (export
    setfsent		endfsent
    getfsent		getfsspec		getfsfile
    setmntent		endmntent
    getmntent		addmntent
    )
  (import (rnrs)
    (foreign posix helpers)
    (prefix (foreign glibc system primitives) primitive:))

  (define-parametrised setfsent)
  (define-parametrised endfsent)
  (define-parametrised getfsent)
  (define-parametrised getfsspec spec)
  (define-parametrised getfsfile file)

  (define-parametrised setmntent file-name open-mode)
  (define-parametrised endmntent stream)
  (define-parametrised getmntent stream)
  (define-parametrised addmntent stream mntent)

  )

;;; end of file
