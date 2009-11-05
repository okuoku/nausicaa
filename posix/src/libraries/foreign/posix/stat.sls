;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: high-level interface to the stat functions
;;;Date: Fri Jan  2, 2009
;;;
;;;Abstract
;;;
;;;	This is an interface  to "stat()", "fstat()" and "lstat()" which
;;;	makes use of the  stubs functions in "libnausicaa-posix.so" from
;;;	the Nausicaa/Stubs project.
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


(library (foreign posix stat)
  (export

    stat		stat-function
    lstat		lstat-function
    fstat		fstat-function

    S_ISDIR		file-is-directory?
    S_ISCHR		file-is-character-special?
    S_ISBLK		file-is-block-special?
    S_ISREG		file-is-regular?
    S_ISFIFO		file-is-fifo?
    S_ISLNK		file-is-symbolic-link?
    S_ISSOCK		file-is-socket?

    S_TYPEISMQ		file-is-message-queue?
    S_TYPEISSEM		file-is-semaphore?
    S_TYPEISSHM		file-is-shared-memory?

    file-user-readable?		file-user-writable?	file-user-executable?
    file-group-readable?	file-group-writable?	file-group-executable?
    file-other-readable?	file-other-writable?	file-other-executable?
    file-setuid?		file-setgid?		file-sticky?

    lfile-user-readable?	lfile-user-writable?	lfile-user-executable?
    lfile-group-readable?	lfile-group-writable?	lfile-group-executable?
    lfile-other-readable?	lfile-other-writable?	lfile-other-executable?
    lfile-setuid?		lfile-setgid?		lfile-sticky?

    file-permissions		lfile-permissions)
  (import (rnrs)
    (receive)
    (compensations)
    (only (foreign memory)
	  malloc-block/c)
    (only (foreign cstrings)
	  string->cstring/c)
    (only (foreign errno)
	  raise-errno-error)
    (except (foreign posix sizeof)
	    sizeof-struct-stat)
    (foreign posix helpers)
    (foreign posix stat record-types)
    (prefix (only (foreign posix stat primitives)
		  stat lstat fstat)
	    primitive:)
    (prefix (only (foreign posix stat platform)
		  stat fstat sizeof-struct-stat)
	    platform:)
    (only (foreign posix stat primitives)
	  S_ISDIR		S_ISCHR		S_ISBLK
	  S_ISREG		S_ISFIFO	S_ISLNK
	  S_ISSOCK
	  S_TYPEISMQ		S_TYPEISSEM	S_TYPEISSHM))


;;;; stat functions

(define-parametrised stat pathname)
(define-parametrised lstat pathname)
(define-parametrised fstat fd)


(define (%type-inspection funcname pred obj)
  (cond ((and (integer? obj) (< -1 obj)) ;file descriptor
	 (pred (fstat obj)))

	((or (string? obj) (symbol? obj)) ;pathname
	 (pred (stat obj)))

	(else
	 (error funcname "expected file descriptor or file pathname" obj))))

;;; --------------------------------------------------------------------

(define (file-is-directory? obj)
  (%type-inspection 'file-is-directory?
		    (lambda (record)
		      (not (= 0 (S_ISDIR (<struct-stat>-mode record)))))
		    obj))

(define (file-is-character-special? obj)
  (%type-inspection 'file-is-character-special?
		    (lambda (record)
		      (not (= 0 (S_ISCHR (<struct-stat>-mode record)))))
		    obj))

(define (file-is-block-special? obj)
  (%type-inspection 'file-is-block-special?
		    (lambda (record)
		      (not (= 0 (S_ISBLK (<struct-stat>-mode record)))))
		    obj))

(define (file-is-regular? obj)
  (%type-inspection 'file-is-regular?
		    (lambda (record)
		      (not (= 0 (S_ISREG (<struct-stat>-mode record)))))
		    obj))

(define (file-is-fifo? obj)
  (%type-inspection 'file-is-fifo?
		    (lambda (record)
		      (not (= 0 (S_ISFIFO (<struct-stat>-mode record)))))
		    obj))

(define (file-is-socket? obj)
  (%type-inspection 'file-is-socket?
		    (lambda (record)
		      (not (= 0 (S_ISSOCK (<struct-stat>-mode record)))))
		    obj))

(define (file-is-symbolic-link? pathname)
  (not (= 0 (S_ISLNK (<struct-stat>-mode (lstat pathname))))))


(define (%pointer-type-inspection funcname getter obj)
  (cond ((and (integer? obj) (< -1 obj)) ;file descriptor
	 (with-compensations
	   (let ((*struct-stat (malloc-block/c platform:sizeof-struct-stat)))
	     (receive (result errno)
		 (platform:fstat obj *struct-stat)
	       (when (= -1 result)
		 (raise-errno-error funcname errno obj))
	       (not (= 0 (getter *struct-stat)))))))

	((or (string? obj) (symbol? obj)) ;pathname
	 (with-compensations
	   (let ((*struct-stat (malloc-block/c platform:sizeof-struct-stat)))
	     (receive (result errno)
		 (platform:stat (string->cstring/c obj) *struct-stat)
	       (when (= -1 result)
		 (raise-errno-error funcname errno obj))
	       (not (= 0 (getter *struct-stat)))))))

	(else
	 (error funcname "expected file descriptor or file pathname" obj))))

;;; --------------------------------------------------------------------

(define (file-is-message-queue? obj)
  (%pointer-type-inspection 'file-is-message-queue? S_TYPEISMQ obj))

(define (file-is-semaphore? obj)
  (%pointer-type-inspection 'file-is-semaphore? S_TYPEISSEM obj))

(define (file-is-shared-memory? obj)
  (%pointer-type-inspection 'file-is-semaphore? S_TYPEISSHM obj))


(define (%mode-inspection the-stat funcname mask obj)
  (define (set? record)
    (not (= 0 (bitwise-and mask (<struct-stat>-mode record)))))
  (cond ((and (eq? the-stat stat) (integer? obj) (< -1 obj)) ;file descriptor
	 (set? (fstat obj)))

	((or (string? obj) (symbol? obj)) ;pathname
	 (set? (the-stat obj)))

	(else
	 (error funcname "expected file descriptor or file pathname" obj))))

;;; --------------------------------------------------------------------

(define (file-user-readable? obj)
  (%mode-inspection stat 'file-user-readable? S_IRUSR obj))

(define (file-user-writable? obj)
  (%mode-inspection stat 'file-user-writable? S_IWUSR obj))

(define (file-user-executable? obj)
  (%mode-inspection stat 'file-user-executable? S_IXUSR obj))

(define (file-group-readable? obj)
  (%mode-inspection stat 'file-user-readable? S_IRGRP obj))

(define (file-group-writable? obj)
  (%mode-inspection stat 'file-group-writable? S_IWGRP obj))

(define (file-group-executable? obj)
  (%mode-inspection stat 'file-group-executable? S_IXGRP obj))

(define (file-other-readable? obj)
  (%mode-inspection stat 'file-other-readable? S_IROTH obj))

(define (file-other-writable? obj)
  (%mode-inspection stat 'file-other-writable? S_IWOTH obj))

(define (file-other-executable? obj)
  (%mode-inspection stat 'file-other-executable? S_IXOTH obj))

(define (file-setuid? obj)
  (%mode-inspection stat 'file-setuid? S_ISUID obj))

(define (file-setgid? obj)
  (%mode-inspection stat 'file-setgid? S_ISGID obj))

(define (file-sticky? obj)
  (%mode-inspection stat 'file-sticky? S_ISVTX obj))

;;; --------------------------------------------------------------------

(define (lfile-user-readable? obj)
  (%mode-inspection lstat 'lfile-user-readable? S_IRUSR obj))

(define (lfile-user-writable? obj)
  (%mode-inspection lstat 'lfile-user-writable? S_IWUSR obj))

(define (lfile-user-executable? obj)
  (%mode-inspection lstat 'lfile-user-executable? S_IXUSR obj))

(define (lfile-group-readable? obj)
  (%mode-inspection lstat 'lfile-user-readable? S_IRGRP obj))

(define (lfile-group-writable? obj)
  (%mode-inspection lstat 'lfile-group-writable? S_IWGRP obj))

(define (lfile-group-executable? obj)
  (%mode-inspection lstat 'lfile-group-executable? S_IXGRP obj))

(define (lfile-other-readable? obj)
  (%mode-inspection lstat 'lfile-other-readable? S_IROTH obj))

(define (lfile-other-writable? obj)
  (%mode-inspection lstat 'lfile-other-writable? S_IWOTH obj))

(define (lfile-other-executable? obj)
  (%mode-inspection lstat 'lfile-other-executable? S_IXOTH obj))

(define (lfile-setuid? obj)
  (%mode-inspection lstat 'lfile-setuid? S_ISUID obj))

(define (lfile-setgid? obj)
  (%mode-inspection lstat 'lfile-setgid? S_ISGID obj))

(define (lfile-sticky? obj)
  (%mode-inspection lstat 'lfile-sticky? S_ISVTX obj))


(define (file-permissions obj)

  (define (get-mode record)
    (let ((mode (<struct-stat>-mode record)))
      (bitwise-and
       (bitwise-ior S_ISUID S_ISGID S_IRWXU S_IRWXG S_IRWXO)
       mode)))

  (cond ((and (integer? obj) (< -1 obj)) ;file descriptor
	 (get-mode (fstat obj)))

	((or (string? obj) (symbol? obj)) ;pathname
	 (get-mode (stat obj)))

	(else
	 (error 'file-permissions "expected file descriptor or file pathname" obj))))

(define (lfile-permissions obj)
  (bitwise-and (bitwise-ior S_ISUID S_ISGID S_IRWXU S_IRWXG S_IRWXO)
	       (<struct-stat>-mode (lstat obj))))


;;;; done

)

;;; end of file
