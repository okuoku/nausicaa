;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: file system functions
;;;Date: Thu Jan  1, 2009
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


(library (posix file)
  (export

    ;; working directory
    getcwd			getcwd-function
    chdir			chdir-function
    fchdir			fchdir-function
    (rename (getcwd pwd))

    ;; directory access
    opendir			opendir-function
    fdopendir			fdopendir-function
    dirfd			dirfd-function

    closedir			closedir-function
    readdir			readdir-function
    readdir_r			readdir_r-function
    rewinddir			rewinddir-function

    telldir			telldir-function
    seekdir			seekdir-function

    (rename (primitive:dirent-name->string	dirent-name->string))
    opendir/compensated		(rename (opendir/compensated opendir/c))
    fdopendir/compensated	(rename (fdopendir/compensated fdopendir/c))
    directory-entries		directory-entries/fd

    ftw				ftw-function
    nftw			nftw-function
    (rename (primitive:make-ftw-callback	make-ftw-callback)
	    (primitive:make-nftw-callback	make-nftw-callback))

    ;; links
    link			link-function
    symlink			symlink-function
    readlink			readlink-function
    realpath			realpath-function

    ;; removing
    unlink			unlink-function
    rmdir			rmdir-function
    remove			remove-function

    ;; renaming
    rename			rename-function

    ;; mkdir
    mkdir			mkdir-function

    ;; changing owner
    chown			chown-function
    fchown			fchown-function

    ;; changing permissions
    umask			umask-function
    chmod			chmod-function
    fchmod			fchmod-function
    (rename (primitive:getumask	getumask))

    ;; access test
    access			access-function

    ;; file times
    utime			utime-function
    utimes			utimes-function

    ;; file size
    file-size			file-size-function
    ftruncate			ftruncate-function
    truncate			truncate-function

    ;; temporary files
    mkstemp			mkstemp-function
    mkdtemp			mkdtemp-function

    ;; stat
    stat		stat-function
    lstat		lstat-function
    fstat		fstat-function

    (rename (primitive:S_ISDIR		S_ISDIR)
	    (primitive:S_ISCHR		S_ISCHR)
	    (primitive:S_ISBLK		S_ISBLK)
	    (primitive:S_ISREG		S_ISREG)
	    (primitive:S_ISFIFO		S_ISFIFO)
	    (primitive:S_ISLNK		S_ISLNK)
	    (primitive:S_ISSOCK		S_ISSOCK)

	    (primitive:S_TYPEISMQ	S_TYPEISMQ)
	    (primitive:S_TYPEISSEM	S_TYPEISSEM)
	    (primitive:S_TYPEISSHM	S_TYPEISSHM))

    file-is-directory?		file-is-character-special?
    file-is-block-special?	file-is-regular?
    file-is-fifo?		file-is-symbolic-link?
    file-is-socket?		file-is-message-queue?
    file-is-semaphore?		file-is-shared-memory?

    file-user-readable?		file-user-writable?	file-user-executable?
    file-group-readable?	file-group-writable?	file-group-executable?
    file-other-readable?	file-other-writable?	file-other-executable?
    file-setuid?		file-setgid?		file-sticky?

    lfile-user-readable?	lfile-user-writable?	lfile-user-executable?
    lfile-group-readable?	lfile-group-writable?	lfile-group-executable?
    lfile-other-readable?	lfile-other-writable?	lfile-other-executable?
    lfile-setuid?		lfile-setgid?		lfile-sticky?

    file-permissions		lfile-permissions
    )
  (import (except (rnrs) remove truncate)
    (receive)
    (compensations)
    (only (foreign ffi pointers) pointer-null?)
    (foreign memory)
    (foreign cstrings)
    (foreign errno)
    (posix sizeof)
    (posix typedefs)
    (posix helpers)
    (prefix (posix file primitives) primitive:)
    (only (posix fd) open close lseek)
    (prefix (only (posix file platform) stat fstat sizeof-stat) platform:))


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
(define-parametrised readdir_r stream)
(define-parametrised rewinddir stream)
(define-parametrised telldir stream)
(define-parametrised seekdir stream position)

(define-parametrised ftw pathname selector-callback descriptors)
(define-parametrised nftw pathname selector-callback descriptors flag)

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
(define-primitive-parameter utimes-function	primitive:utimes)

(define utime
  (case-lambda
   ((pathname access-time modification-time)
    ((utime-function) pathname access-time modification-time))
   ((pathname)
    ((utime-function) pathname))))

(define utimes
  (case-lambda
   ((pathname access-time-sec access-time-usec modification-time-sec modification-time-usec)
    ((utimes-function) pathname
     access-time-sec access-time-usec
     modification-time-sec modification-time-usec))
   ((pathname)
    ((utimes-function) pathname))))

;; file size

(define-parametrised file-size obj)
(define-parametrised ftruncate obj length)
(define-parametrised truncate pathname length)

;; removing

(define-parametrised unlink pathname)
(define-parametrised remove pathname)
(define-parametrised rmdir pathname)

;; renaming

(define-parametrised rename oldname newname)

;; making directories

(define-parametrised mkdir pathname mode)

;; temporary files

(define-parametrised mkstemp template)
(define-parametrised mkdtemp template)


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

;;; --------------------------------------------------------------------

(define (%directory-entries dir)
  (let loop ((entry  (readdir_r dir))
	     (layout '()))
    (if (pointer-null? entry)
	layout
      (loop (readdir_r dir)
	    (cons (primitive:dirent-name->string entry) layout)))))

(define (directory-entries pathname)
  (with-compensations
    (%directory-entries (opendir/compensated pathname))))

(define (directory-entries/fd fd)
  (with-compensations
    (%directory-entries (fdopendir/compensated fd))))


;;;; stat functions

(define-parametrised stat pathname)
(define-parametrised lstat pathname)
(define-parametrised fstat fd)


(define (%type-inspection funcname pred obj)
  (cond ((file-descriptor? obj)
	 (not (= 0 (pred (<struct-stat>-mode (fstat obj))))))

	((or (string? obj) (symbol? obj)) ;pathname
	 (not (= 0 (pred (<struct-stat>-mode (stat obj))))))

	((and (integer? obj) (exact? obj)) ;"st_mode" field in "struct stat"
	 (not (= 0 (pred obj))))

	(else
	 (error funcname "expected file descriptor, file pathname or st_mode integer" obj))))

;;; --------------------------------------------------------------------

(define (file-is-directory? obj)
  (%type-inspection 'file-is-directory? primitive:S_ISDIR obj))

(define (file-is-character-special? obj)
  (%type-inspection 'file-is-character-special? primitive:S_ISCHR obj))

(define (file-is-block-special? obj)
  (%type-inspection 'file-is-block-special? primitive:S_ISBLK obj))

(define (file-is-regular? obj)
  (%type-inspection 'file-is-regular? primitive:S_ISREG obj))

(define (file-is-fifo? obj)
  (%type-inspection 'file-is-fifo? primitive:S_ISFIFO obj))

(define (file-is-socket? obj)
  (%type-inspection 'file-is-socket? primitive:S_ISSOCK obj))

(define (file-is-symbolic-link? mode/pathname)
  (cond ((and (integer? mode/pathname) (exact? mode/pathname))
	 (not (= 0 (primitive:S_ISLNK mode/pathname))))

	((or (string? mode/pathname) (symbol? mode/pathname)) ;pathname
	 (not (= 0 (primitive:S_ISLNK (<struct-stat>-mode (lstat mode/pathname))))))

	(else
	 (error 'file-is-symbolic-link?
	   "file pathname or st_mode integer" mode/pathname))))


(define (%pointer-type-inspection funcname getter obj)
  (cond ((file-descriptor? obj)
	 (with-compensations
	   (let ((struct-stat* (malloc-block/c platform:sizeof-stat)))
	     (receive (result errno)
		 (platform:fstat (file-descriptor->integer obj) struct-stat*)
	       (when (= -1 result)
		 (raise-errno-error funcname errno obj))
	       (not (= 0 (getter struct-stat*)))))))

	((or (string? obj) (symbol? obj)) ;pathname
	 (with-compensations
	   (let ((struct-stat* (malloc-block/c platform:sizeof-stat)))
	     (receive (result errno)
		 (platform:stat (string->cstring/c obj) struct-stat*)
	       (when (= -1 result)
		 (raise-errno-error funcname errno obj))
	       (not (= 0 (getter struct-stat*)))))))

	(else
	 (error funcname "expected file descriptor or file pathname" obj))))

;;; --------------------------------------------------------------------

(define (file-is-message-queue? obj)
  (%pointer-type-inspection 'file-is-message-queue? primitive:S_TYPEISMQ obj))

(define (file-is-semaphore? obj)
  (%pointer-type-inspection 'file-is-semaphore? primitive:S_TYPEISSEM obj))

(define (file-is-shared-memory? obj)
  (%pointer-type-inspection 'file-is-semaphore? primitive:S_TYPEISSHM obj))


(define (%mode-inspection the-stat funcname mask obj)
  (define (set? mode)
    (not (= 0 (bitwise-and mask mode))))
  (cond ((and (eq? the-stat 'stat) (file-descriptor? obj))
	 (set? (<struct-stat>-mode (fstat obj))))

	((or (string? obj) (symbol? obj)) ;pathname
	 (set? (<struct-stat>-mode ((case the-stat
				      ((stat)  stat)
				      ((lstat) lstat)) obj))))

	((and (integer? obj) (exact? obj)) ;"st_mode" field in "struct stat"
	 (set? obj))

	(else
	 (error funcname "expected file descriptor, file pathname or st_mode integer" obj))))

;;; --------------------------------------------------------------------

(define (file-user-readable? obj)
  (%mode-inspection 'stat 'file-user-readable? S_IRUSR obj))

(define (file-user-writable? obj)
  (%mode-inspection 'stat 'file-user-writable? S_IWUSR obj))

(define (file-user-executable? obj)
  (%mode-inspection 'stat 'file-user-executable? S_IXUSR obj))

(define (file-group-readable? obj)
  (%mode-inspection 'stat 'file-user-readable? S_IRGRP obj))

(define (file-group-writable? obj)
  (%mode-inspection 'stat 'file-group-writable? S_IWGRP obj))

(define (file-group-executable? obj)
  (%mode-inspection 'stat 'file-group-executable? S_IXGRP obj))

(define (file-other-readable? obj)
  (%mode-inspection 'stat 'file-other-readable? S_IROTH obj))

(define (file-other-writable? obj)
  (%mode-inspection 'stat 'file-other-writable? S_IWOTH obj))

(define (file-other-executable? obj)
  (%mode-inspection 'stat 'file-other-executable? S_IXOTH obj))

(define (file-setuid? obj)
  (%mode-inspection 'stat 'file-setuid? S_ISUID obj))

(define (file-setgid? obj)
  (%mode-inspection 'stat 'file-setgid? S_ISGID obj))

(define (file-sticky? obj)
  (%mode-inspection 'stat 'file-sticky? S_ISVTX obj))

;;; --------------------------------------------------------------------

(define (lfile-user-readable? obj)
  (%mode-inspection 'lstat 'lfile-user-readable? S_IRUSR obj))

(define (lfile-user-writable? obj)
  (%mode-inspection 'lstat 'lfile-user-writable? S_IWUSR obj))

(define (lfile-user-executable? obj)
  (%mode-inspection 'lstat 'lfile-user-executable? S_IXUSR obj))

(define (lfile-group-readable? obj)
  (%mode-inspection 'lstat 'lfile-user-readable? S_IRGRP obj))

(define (lfile-group-writable? obj)
  (%mode-inspection 'lstat 'lfile-group-writable? S_IWGRP obj))

(define (lfile-group-executable? obj)
  (%mode-inspection 'lstat 'lfile-group-executable? S_IXGRP obj))

(define (lfile-other-readable? obj)
  (%mode-inspection 'lstat 'lfile-other-readable? S_IROTH obj))

(define (lfile-other-writable? obj)
  (%mode-inspection 'lstat 'lfile-other-writable? S_IWOTH obj))

(define (lfile-other-executable? obj)
  (%mode-inspection 'lstat 'lfile-other-executable? S_IXOTH obj))

(define (lfile-setuid? obj)
  (%mode-inspection 'lstat 'lfile-setuid? S_ISUID obj))

(define (lfile-setgid? obj)
  (%mode-inspection 'lstat 'lfile-setgid? S_ISGID obj))

(define (lfile-sticky? obj)
  (%mode-inspection 'lstat 'lfile-sticky? S_ISVTX obj))


(define (file-permissions obj)

  (define (get-mode record)
    (let ((mode (<struct-stat>-mode record)))
      (bitwise-and
       (bitwise-ior S_ISUID S_ISGID S_IRWXU S_IRWXG S_IRWXO)
       mode)))

  (cond ((file-descriptor? obj)
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
