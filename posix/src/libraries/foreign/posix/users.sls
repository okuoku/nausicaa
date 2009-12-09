;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to the users and groups functions
;;;Date: Tue Jan  6, 2009
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


(library (foreign posix users)
  (export
    getuid		getuid-function
    getgid		getgid-function
    geteuid		geteuid-function
    getegid		getegid-function

    setuid		setuid-function
    setgid		setgid-function
    seteuid		seteuid-function
    setegid		setegid-function

    getgroups		getgroups-function
    setgroups		setgroups-function
    initgroups		initgroups-function
    getgrouplist	getgrouplist-function

    getlogin		cuserid
    getpwuid		getpwnam

    )
  (import (rnrs)
    (foreign posix helpers)
    (prefix (foreign posix users primitives) primitive:))


(define-parametrised getuid)
(define-parametrised getgid)
(define-parametrised geteuid)
(define-parametrised getegid)
(define-parametrised getgroups)

(define-parametrised setuid uid)
(define-parametrised setgid gid)
(define-parametrised seteuid uid)
(define-parametrised setegid gid)
(define-parametrised setgroups gid-list)

(define-parametrised initgroups user-name gid)
(define-parametrised getgrouplist user-name gid)

(define-parametrised getlogin)
(define-parametrised cuserid)

(define-parametrised getpwuid uid)
(define-parametrised getpwnam user-name)


;;;; done

)

;;; end of file
