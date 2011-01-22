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
;;;Copyright (c) 2009, 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (nausicaa posix system)
  (export
    sysconf		confstr
    pathconf		fpathconf
    gethostname		sethostname
    getdomainname	setdomainname
    uname
    mount		umount2			umount

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
    getgrgid		getgrnam
    fgetpwent		fgetgrent

    getenv setenv environ environ-table
    (rename (primitive:environ->table	environ->table)
	    (primitive:table->environ	table->environ)))
  (import (rnrs)
    (nausicaa posix helpers)
    (prefix (nausicaa posix system primitives) primitive:))


(define-parametrised sysconf param)
(define-parametrised pathconf pathname param)
(define-parametrised fpathconf fd param)
(define-parametrised confstr param)

(define-parametrised gethostname)
(define-parametrised sethostname host-name)
(define-parametrised getdomainname)
(define-parametrised setdomainname domain-name)
(define-parametrised uname)

(define-parametrised mount special-file mount-point fstype options data*)
(define-parametrised umount2)
(define-parametrised umount)

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
(define-parametrised fgetpwent stream)

(define-parametrised getgrgid uid)
(define-parametrised getgrnam user-name)
(define-parametrised fgetgrent stream)

;; environment variables
(define-parametrised setenv ((varname newvalue) (varname newvalue replace)))
(define-parametrised getenv varname)
(define-parametrised environ)
(define-parametrised environ-table)


;;; end of file

)

;;; end of file
