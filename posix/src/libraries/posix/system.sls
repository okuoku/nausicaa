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


(library (posix system)
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

    getenv setenv environ
    environ-table environ->table table->environ)
  (import (rnrs)
    (begin0)
    (only (strings) string-index)
    (posix helpers)
    (prefix (posix system primitives) primitive:))


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


(define-primitive-parameter setenv-function primitive:setenv)
(define-primitive-parameter getenv-function primitive:getenv)
(define-primitive-parameter environ-function primitive:environ)

(define setenv
  (case-lambda
   ((varname newvalue)
    (setenv varname newvalue #t))
   ((varname newvalue replace)
    ((setenv-function) varname newvalue replace))))

(define (getenv varname)
  ((getenv-function) varname))

(define (environ)
  ((environ-function)))

(define (environ-table)
  (environ->table (environ)))

(define (environ->table environ)
  (begin0-let ((table (make-eq-hashtable)))
    (for-each (lambda (str)
		(let ((idx (string-index str #\=)))
		  (hashtable-set! table
				  (string->symbol (substring str 0 idx))
				  (substring str (+ 1 idx) (string-length str)))))
      environ)))

(define (table->environ table)
  (let-values (((names values) (hashtable-entries table)))
    (let ((len (vector-length names))
	  (environ '()))
      (do ((i 0 (+ 1 i)))
	  ((= i len)
	   environ)
	(set! environ (cons (string-append (let ((n (vector-ref names i)))
					     (if (string? n)
						 n
					       (symbol->string n)))
					   "="
					   (vector-ref values i))
			    environ))))))


;;; end of file

  )

;;; end of file
