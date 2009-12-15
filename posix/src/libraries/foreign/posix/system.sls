;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: parametrised interface to system inspection functions
;;;Date: Wed Dec  9, 2009
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


(library (foreign posix system)
  (export
    sysconf pathconf fpathconf confstr
    gethostname sethostname
    getdomainname setdomainname
    uname
    )
  (import (rnrs)
    (foreign posix helpers)
    (prefix (foreign posix system primitives) primitive:))

  (define-parametrised sysconf param)
  (define-parametrised pathconf pathname param)
  (define-parametrised fpathconf fd param)
  (define-parametrised confstr param)

  (define-parametrised gethostname)
  (define-parametrised sethostname host-name)
  (define-parametrised getdomainname)
  (define-parametrised setdomainname domain-name)
  (define-parametrised uname)
  )

;;; end of file
