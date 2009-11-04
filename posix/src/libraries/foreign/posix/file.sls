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


#!r6rs
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
    utimes		utimes-function
    lutimes		lutimes-function

    ;; file size
    file-size		file-size-function
    ftruncate		ftruncate-function)
  (import (except (nausicaa)
		  remove truncate)
    (compensations)
    (foreign cstrings)
    (foreign posix primitives)
    (except (foreign posix fd)
	    read write)
    (foreign posix sizeof))


;;;; working directory

(define-primitive-parameter getcwd-function primitive:getcwd)
(define-primitive-parameter chdir-function  primitive:chdir)
(define-primitive-parameter fchdir-function primitive:fchdir)

(define (getcwd)
  ((getcwd-function)))

(define (chdir pathname)
  ((chdir-function) pathname))

(define (fchdir fd)
  ((fchdir-function) fd))


;;;; directory access

(define-primitive-parameter opendir-function		primitive:opendir)
(define-primitive-parameter fdopendir-function		primitive:fdopendir)
(define-primitive-parameter dirfd-function		primitive:dirfd)
(define-primitive-parameter closedir-function		primitive:closedir)
(define-primitive-parameter readdir-function		primitive:readdir)
(define-primitive-parameter rewinddir-function		primitive:rewinddir)
(define-primitive-parameter telldir-function		primitive:telldir)
(define-primitive-parameter seekdir-function		primitive:seekdir)

(define (opendir pathname)
  ((opendir-function) pathname))

(define (fdopendir fd)
  ((fdopendir-function) fd))

(define (dirfd stream)
  ((dirfd-function) stream))

(define (closedir stream)
  ((closedir-function) stream))

(define (readdir stream)
  ((readdir-function) stream))

(define (rewinddir stream)
  ((rewinddir-function) stream))

(define (telldir stream)
  ((telldir-function) stream))

(define (seekdir stream position)
  ((seekdir-function) stream position))

;;; --------------------------------------------------------------------

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


;;;; links

(define-primitive-parameter link-function		primitive:link)
(define-primitive-parameter symlink-function		primitive:symlink)
(define-primitive-parameter readlink-function		primitive:readlink)
(define-primitive-parameter realpath-function		primitive:realpath)

(define (link oldname newname)
  ((link-function) oldname newname))

(define (symlink oldname newname)
  ((symlink-function) oldname newname))

(define (readlink pathname)
  ((readlink-function) pathname))

(define (realpath pathname)
  ((realpath-function) pathname))


;;;; changing owner

(define-primitive-parameter chown-function		primitive:chown)
(define-primitive-parameter fchown-function		primitive:fchown)

(define (chown pathname owner-id group-id)
  ((chown-function) pathname owner-id group-id))

(define (fchown fd owner-id group-id)
  ((fchown-function) fd owner-id group-id))


;;;; changing permissions

(define-primitive-parameter umask-function		primitive:umask)
(define-primitive-parameter chmod-function		primitive:chmod)
(define-primitive-parameter fchmod-function		primitive:fchmod)

(define (umask mask)
  ((umask-function) mask))

(define (chmod pathname mode)
  ((chmod-function) pathname mode))

(define (fchmod fd mode)
  ((fchmod-function) fd mode))


;;;; testing access

(define-primitive-parameter access-function		primitive:access)

(define (access fd mask)
  ((access-function) fd mask))


;;;; file times

(define-primitive-parameter utime-function		primitive:utime)

(define utime
  (case-lambda
   ((pathname access-time modification-time)
    ((utime-function) pathname access-time modification-time))
   ((pathname)
    ((utime-function) pathname))))


;;;; file size

(define-primitive-parameter file-size-function		primitive:file-size)
(define-primitive-parameter ftruncate-function		primitive:ftruncate)

(define (file-size obj)
  ((file-size-function) obj))

(define (ftruncate obj length)
  ((ftruncate-function) obj length))


;;;; removing

(define-primitive-parameter unlink-function		primitive:unlink)
(define-primitive-parameter remove-function		primitive:remove)
(define-primitive-parameter rmdir-function		primitive:rmdir)

(define (unlink pathname)
  ((unlink-function) pathname))

(define (remove pathname)
  ((remove-function) pathname))

(define (rmdir pathname)
  ((rmdir-function) pathname))


;;;; renaming

(define-primitive-parameter rename-function		primitive:rename)

(define (rename oldname newname)
  ((rename-function) oldname newname))


;;;; making directories

(define-primitive-parameter mkdir-function		primitive:mkdir)

(define (mkdir pathname mode)
  ((mkdir-function) pathname mode))


;;;; temporary files

(define-primitive-parameter tmpnam-function		primitive:tmpnam)
(define-primitive-parameter mktemp-function		primitive:mktemp)
(define-primitive-parameter mkstemp-function		primitive:mkstemp)

(define (tmpnam)
  ((tmpnam-function)))

(define (mktemp template)
  ((mktemp-function) template))

(define (mkstemp template)
  ((mkstemp-function) template))


;;;; done

)

;;; end of file
