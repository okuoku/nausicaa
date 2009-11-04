;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: posix interface to file functions
;;;Date: Wed Nov  4, 2009
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


(library (foreign posix file primitives)
  (export
    ;; working directory
    getcwd		chdir		fchdir

    ;; directory access
    opendir		fdopendir	dirfd
    closedir		readdir		rewinddir
    telldir		seekdir		scandir

    ;; links
    link		symlink		readlink
    realpath

    ;; removing
    unlink		rmdir		remove

    ;; renaming
    rename

    ;; mkdir
    mkdir

    ;; temporary files
    tmpnam		mktemp		mkstemp

    ;; changing owner
    chown		fchown

    ;; changing permissions
    chmod		fchmod
    (rename (platform:umask	umask))

    ;; access test
    access

    ;; file times
    utime		utimes		lutimes

    ;; file size
    file-size		ftruncate)
  (import (except (rnrs)
		  remove truncate)
    (compensations)
    (foreign cstrings)
    (prefix (foreigh posix file platform) platform:))


;;;; working directory

(define (getcwd)
  (let loop ((buflen 1024))
    (with-compensations
      (let ((buffer (malloc-block/c buflen)))
	(receive (cstr errno)
	    (platform:getcwd buffer buflen)
	  (if (and (= 0 (pointer->integer cstr))
		   (or (= EINVAL errno)
		       (= ERANGE errno)))
	      (loop (* 2 buflen))
	    (begin
	      (when (pointer-null? cstr)
		(raise-errno-error 'getcwd errno))
	      (cstring->string buffer))))))))

(define (chdir directory-pathname)
  (with-compensations
    (receive (result errno)
	(platform:chdir (string->cstring/c directory-pathname))
      (unless (= 0 result)
	(raise-errno-error 'chdir errno
			   directory-pathname))
      result)))

(define (fchdir fd)
  (receive (result errno)
      (platform:fchdir fd)
    (unless (= 0 result)
      (raise-errno-error 'fchdir errno fd))
    result))


;;;; directory access

(define (opendir pathname)
  (with-compensations
    (receive (result errno)
	(platform:opendir (string->cstring/c pathname))
      (when (pointer-null? result)
	(raise-errno-error 'opendir errno pathname))
      result)))

(define (fdopendir fd)
  (receive (result errno)
      (platform:fdopendir fd)
    (when (pointer-null? result)
      (raise-errno-error 'fdopendir errno fd))
    result))

(define (dirfd stream)
  (receive (result errno)
      (platform:dirfd stream)
    (when (= -1 result)
      (raise-errno-error 'dirfd errno stream))
    result))

(define (closedir stream)
  (receive (result errno)
      (platform:closedir stream)
    (when (= -1 result)
      (raise-errno-error 'closedir errno stream))
    result))

(define (readdir stream)
  (receive (result errno)
      (platform:readdir stream)
    ;;Here  we assume  that errno  is  set to  zero by  PLATFORM:READDIR
    ;;before the call to the foreign function.
    (when (and (pointer-null? result)
	       (not (= 0 errno)))
      (raise-errno-error 'readdir errno stream))
    result))

(define (rewinddir stream)
  (platform:rewinddir stream))

(define (telldir stream)
  (platform:telldir stream))

(define (seekdir stream position)
  (platform:seekdir stream position))


;;;; links

(define (%real-link func funcname oldname newname)
  (with-compensations
    (receive (result errno)
	(func (string->cstring/c oldname)
	      (string->cstring/c newname))
      (when (= -1 result)
	(raise-errno-error funcname errno
			   (list oldname newname)))
      result)))

(define (link oldname newname)
  (%real-link platform:link 'link oldname newname))

(define (symlink oldname newname)
  (%real-link platform:symlink 'symlink oldname newname))

(define (readlink pathname)
  (with-compensations
    (let ((c-pathname	(string->cstring/c pathname)))
      (receive (size errno)
	  (platform:readlink c-pathname pointer-null 0)
	(when (= -1 size)
	  (raise-errno-error 'readlink errno pathname))
	(let ((buffer	(malloc-block/c size)))
	  (receive (result errno)
	      (platform:readlink c-pathname buffer size)
	    (when (= -1 size)
	      (raise-errno-error 'readlink errno pathname))
	    (cstring->string buffer result)))))))

(define (realpath pathname)
  (with-compensations
    (receive (buffer errno)
	(platform:realpath (string->cstring/c pathname)
			   pointer-null)
      (when (pointer-null? buffer)
	(raise-errno-error 'realpath errno pathname))
      (begin0
	  (cstring->string buffer)
	(free buffer)))))


;;;; changing owner

(define (chown pathname owner-id group-id)
  (with-compensations
    (receive (result errno)
	(platform:chown (string->cstring/c pathname)
			owner-id group-id)
      (when (= -1 result)
	(raise-errno-error 'chown errno (list pathname owner-id group-id)))
      result)))

(define (fchown fd owner-id group-id)
  (with-compensations
    (receive (result errno)
	(platform:fchown fd owner-id group-id)
      (when (= -1 result)
	(raise-errno-error 'fchown errno (list fd owner-id group-id)))
      result)))


;;;; changing permissions

(define (umask mask)
  (with-compensations
    (receive (result errno)
	(platform:umask mask)
      (when (= -1 result)
	(raise-errno-error 'umask errno mask))
      result)))

(define (getumask)
  (let ((m (umask 0)))
    (umask m)
    m))

(define (chmod pathname mode)
  (with-compensations
    (receive (result errno)
	(platform:chmod (string->cstring/c pathname) mode)
      (when (= -1 result)
	(raise-errno-error 'chmod errno (list pathname mode)))
      result)))

(define (fchmod fd mode)
  (with-compensations
    (receive (result errno)
	(platform:fchmod fd mode)
      (when (= -1 result)
	(raise-errno-error 'fchmod errno (list fd mode)))
      result)))


;;;; testing access

(define (access pathname mask)
  (with-compensations
    (receive (result errno)
	(platform:access (string->cstring/c pathname) mask)
      (when (and (= -1 result)
		 (not (= 0 errno))
		 (not (= EACCES errno))
		 (not (= ENOENT errno)))
	(raise-errno-error 'access errno (list pathname mask)))
      (= 0 result))))


;;;; file times

(define utime
  (case-lambda
   ((pathname access-time modification-time)
    (with-compensations
      (let ((*struct-utimbuf (malloc-block/c sizeof-struct-utimbuf)))
	(struct-utimbuf-actime-set!  *struct-utimbuf access-time)
	(struct-utimbuf-modtime-set! *struct-utimbuf modification-time)
	(receive (result errno)
	    (platform:utime (string->cstring/c pathname)
			    *struct-utimbuf)
	  (when (= -1 result)
	    (raise-errno-error 'utime errno (list pathname access-time modification-time)))
	  result))))
   ((pathname)
    (receive (result errno)
	(platform:utime (string->cstring/c pathname) pointer-null)
      (when (= -1 result)
	(raise-errno-error 'utime errno pathname))
      result))))


;;;; file size

(define (file-size obj)
  (cond ((or (string? obj) (symbol? obj))
	 (with-compensations
	   (letrec ((fd (compensate
			    (open obj O_RDONLY 0)
			  (with
			   (close fd)))))
	     (file-size fd))))
	((and (integer? obj) (<= 0 obj))
	 (with-compensations
	   (letrec ((pos (compensate
			     (lseek obj 0 SEEK_CUR)
			   (with
			    (lseek obj pos SEEK_SET)))))
	     (lseek obj 0 SEEK_END))))
	(else
	 (error 'file-size
	   "expected file descriptor or file pathname" obj))))

(define (ftruncate obj length)
  (cond ((and (integer? obj) (<= 0 obj))
	 (receive (result errno)
	     (platform:ftruncate obj length)
	   (when (= -1 result)
	     (raise-errno-error 'ftruncate errno
				(list obj length)))
	   result))
	((or (string? obj) (symbol? obj))
	 (with-compensations
	   (letrec ((fd (compensate
			    (open obj O_WRONLY 0)
			  (with
			   (close fd)))))
	     (ftruncate fd length))))
	(else
	 (error 'ftruncate
	   "expected file descriptor or file pathname" obj))))


;;;; removing

(define (unlink pathname)
  (with-compensations
    (receive (result errno)
	(platform:unlink (string->cstring/c pathname))
      (when (= -1 result)
	(raise-errno-error 'unlink errno pathname))
      result)))

(define (remove pathname)
  (with-compensations
    (receive (result errno)
	(platform:remove (string->cstring/c pathname))
      (when (= -1 result)
	(raise-errno-error 'remove errno pathname))
      result)))

(define (rmdir pathname)
  (with-compensations
    (receive (result errno)
	(platform:rmdir (string->cstring/c pathname))
      (when (= -1 result)
	(raise-errno-error 'rmdir errno pathname))
      result)))


;;;; renaming

(define (rename oldname newname)
  (with-compensations
    (receive (result errno)
	(platform:rename (string->cstring/c oldname)
			 (string->cstring/c newname))
      (when (= -1 result)
	(raise-errno-error 'rename errno (list oldname newname)))
      result)))


;;;; making directories

(define (mkdir pathname mode)
  (with-compensations
    (receive (result errno)
	(platform:mkdir (string->cstring/c pathname) mode)
      (when (= -1 result)
	(raise-errno-error 'primitive-mkdir errno (list pathname mode)))
      result)))


;;;; temporary files

(define (tmpnam)
  (with-compensations
    (let ((p	(malloc-block/c (+ 1 L_tmpnam))))
      (platform:tmpnam p)
      (cstring->string p))))

(define (mktemp template)
  (with-compensations
    (let ((p	(string->cstring/c template)))
      (receive (result errno)
	  (platform:mktemp p)
	(when (pointer-null? result)
	  (raise-errno-error 'mktemp errno template))
	(cstring->string p)))))

(define (mkstemp template)
  (with-compensations
    (let ((p	(string->cstring/c template)))
      (receive (result errno)
	  (platform:mkstemp p)
	(when (= -1 result)
	  (raise-errno-error 'mktemp errno template))
	(values result (cstring->string p))))))


;;;; done

)

;;; end of file
