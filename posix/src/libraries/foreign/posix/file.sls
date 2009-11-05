;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: file system functions
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


(library (foreign posix file)
  (export

    ;; working directory
    getcwd		getcwd-function
    chdir		chdir-function
    fchdir		fchdir-function
    (rename (getcwd pwd))

    ;; directory access
    opendir		opendir-function
    fdopendir		fdopendir-function
    dirfd		dirfd-function

    closedir		closedir-function
    readdir		readdir-function
    rewinddir		rewinddir-function

    telldir		telldir-function
    seekdir		seekdir-function
    scandir		scandir-function

    opendir/compensated		(rename (opendir/compensated opendir/c))
    fdopendir/compensated	(rename (fdopendir/compensated fdopendir/c))
    directory-list	directory-list/fd

    ;; links
    link		link-function
    symlink		symlink-function
    readlink		readlink-function
    realpath		realpath-function

    ;; removing
    unlink		unlink-function
    rmdir		rmdir-function
    remove		remove-function

    ;; renaming
    rename		rename-function

    ;; mkdir
    mkdir		mkdir-function

    ;; temporary files
    tmpnam		tmpnam-function
    mktemp		mktemp-function
    mkstemp		mkstemp-function

    ;; changing owner
    chown		chown-function
    fchown		fchown-function

    ;; changing permissions
    umask		umask-function
    chmod		chmod-function
    fchmod		fchmod-function
    (rename (primitive:getumask	getumask))

    ;; access test
    access		access-function

    ;; file times
    utime		utime-function

    ;; file size
    file-size		file-size-function
    ftruncate		ftruncate-function)
  (import (except (rnrs)
		  remove truncate)
    (compensations)
    (only (foreign ffi pointers)
	  pointer-null?)
    (foreign cstrings)
    (foreign posix helpers)
    (prefix (foreign posix file primitives) primitive:)
    (only (foreign posix fd)
	  open close lseek)
    (foreign posix sizeof))


;; working directory

(define-parametrised getcwd)
(define-parametrised chdir pathname)
(define-parametrised fchdir fd)

;; directory access

(define-parametrised opendir pathname)
(define-parametrised fdopendir fd)
(define-parametrised dirfd stream)
(define-parametrised closedir stream)
(define-parametrised readdir stream)
(define-parametrised rewinddir stream)
(define-parametrised telldir stream)
(define-parametrised seekdir stream position)
(define-parametrised scandir dir-pathname selector-callback cmp-callback)

;; links

(define-parametrised link oldname newname)
(define-parametrised symlink oldname newname)
(define-parametrised readlink pathname)
(define-parametrised realpath pathname)

;; changing owner

(define-parametrised chown pathname owner-id group-id)
(define-parametrised fchown fd owner-id group-id)

;; changing permissions

(define-parametrised umask mask)
(define-parametrised chmod pathname mode)
(define-parametrised fchmod fd mode)

;; testing access

(define-parametrised access fd mask)

;; file times

(define-primitive-parameter utime-function		primitive:utime)

(define utime
  (case-lambda
   ((pathname access-time modification-time)
    ((utime-function) pathname access-time modification-time))
   ((pathname)
    ((utime-function) pathname))))

;; file size

(define-parametrised file-size obj)
(define-parametrised ftruncate obj length)

;; removing

(define-parametrised unlink pathname)
(define-parametrised remove pathname)
(define-parametrised rmdir pathname)

;; renaming

(define-parametrised rename oldname newname)

;; making directories

(define-parametrised mkdir pathname mode)

;; temporary files

(define-parametrised tmpnam)
(define-parametrised mktemp template)
(define-parametrised mkstemp template)


(define (opendir/compensated pathname)
  (letrec ((stream (compensate
		       (opendir pathname)
		     (with
		      (closedir stream)))))
    stream))

(define (fdopendir/compensated fd)
  (letrec ((stream (compensate
		       (fdopendir fd)
		     (with
		      (closedir stream)))))
    stream))

(define (directory-list pathname)
  (with-compensations
    (let ((dir		(opendir/compensated pathname))
	  (layout	'()))
      (do ((entry (readdir dir) (readdir dir)))
	  ((pointer-null? entry)
	   layout)
	(set! layout
	      (cons (cstring->string (struct-dirent-d_name-ref entry))
		    layout))))))

(define (directory-list/fd fd)
  (with-compensations
    (let ((dir		(fdopendir/compensated fd))
	  (layout	'()))
      (do ((entry (readdir dir) (readdir dir)))
	  ((pointer-null? entry)
	   layout)
	(set! layout
	      (cons (cstring->string (struct-dirent-d_name-ref entry))
		    layout))))))


;;;; done

)

;;; end of file
