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
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (posix file primitives)
  (export

    ;; working directory
    getcwd		chdir		fchdir

    ;; directory access
    opendir		fdopendir
    opendir/c		fdopendir/c
    dirfd
    closedir		readdir		readdir_r
    rewinddir		telldir		seekdir
    dirent-name->string
    ftw			nftw
    make-ftw-callback	make-nftw-callback
    directory-entries	directory-entries/fd

    ;; links
    link		symlink		readlink
    realpath

    ;; removing
    unlink		rmdir		remove

    ;; renaming
    rename

    ;; mkdir
    mkdir

    ;; changing owner
    chown		fchown

    ;; changing permissions
    chmod		fchmod
    getumask		(rename (platform:umask	umask))

    ;; access test
    access

    ;; file times, note that LUTIMES and FUTIMES are glibc stuff
    utime		utimes

    ;; file size
    file-size		ftruncate	truncate

    ;; temporary files
    mkstemp		mkdtemp

    ;; stat
    stat lstat fstat
    (rename (platform:S_ISDIR		S_ISDIR)
	    (platform:S_ISCHR		S_ISCHR)
	    (platform:S_ISBLK		S_ISBLK)
	    (platform:S_ISREG		S_ISREG)
	    (platform:S_ISFIFO		S_ISFIFO)
	    (platform:S_ISLNK		S_ISLNK)
	    (platform:S_ISSOCK		S_ISSOCK)
	    (platform:S_TYPEISMQ	S_TYPEISMQ)
	    (platform:S_TYPEISSEM	S_TYPEISSEM)
	    (platform:S_TYPEISSHM	S_TYPEISSHM))

    file-is-directory?
    file-is-character-special?
    file-is-block-special?
    file-is-regular?
    file-is-fifo?
    file-is-socket?
    file-is-symbolic-link?
    file-is-message-queue?
    file-is-semaphore?
    file-is-shared-memory?
    file-user-readable?
    file-user-writable?
    file-user-executable?
    file-group-readable?
    file-group-writable?
    file-group-executable?
    file-other-readable?
    file-other-writable?
    file-other-executable?
    file-setuid?
    file-setgid?
    file-sticky?
    lfile-user-readable?
    lfile-user-writable?
    lfile-user-executable?
    lfile-group-readable?
    lfile-group-writable?
    lfile-group-executable?
    lfile-other-readable?
    lfile-other-writable?
    lfile-other-executable?
    lfile-setuid?
    lfile-setgid?
    lfile-sticky?
    file-permissions
    lfile-permissions

    pointer-><stat>)
  (import (except (rnrs) remove truncate)
    (language-extensions)
    (compensations)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign memory)
    (only (foreign cstrings) cstring->string string->cstring/c)
    (foreign errno)
    (posix sizeof)
    (posix typedefs)
    (prefix (posix fd) posix:)
    (prefix (posix file platform) platform:))


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
      (platform:fchdir (fd->integer fd))
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
      (platform:fdopendir (fd->integer fd))
    (when (pointer-null? result)
      (raise-errno-error 'fdopendir errno fd))
    result))

(define (opendir/c pathname)
  (letrec ((stream (compensate
		       (opendir pathname)
		     (with
		      (closedir stream)))))
    stream))

(define (fdopendir/c fd)
  (letrec ((stream (compensate
		       (fdopendir fd)
		     (with
		      (closedir stream)))))
    stream))

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
    (if (and (pointer-null? result)
	     (not (= 0 errno)))
	(raise-errno-error 'readdir errno stream)
      result)))

(define (readdir_r stream)
  (let ((result (malloc-block/c sizeof-dirent)))
    (with-compensations
      ;;*FIXME*  It looks  like the  size  of the  "struct dirent"  type
      ;;cannot  be  safely  determined  with  the  "sizeof"  C  language
      ;;operator.   So the  allocation form  below  may turn  out to  be
      ;;wrong.   See the  manual page  of "readdir_r()"  on  a GNU+Linux
      ;;system for details on the correct way to determine the size.
      ;;
      ;;It  is not  implemented  here  because it  needs  to change  the
      ;;interface adding the pathname  of the directory to be inspected;
      ;;this is not desirable now, we will see in the future.
      ;;
      ;;For the time being, it  seems that on GNU+Linux systems for i686
      ;;architectures   the   size  is   correct.    Also  notice   that
      ;;MALLOC-BLOCK/C will  probably allocate a page  (4096 bytes) from
      ;;its own cache; see the documentation of Nausicaa/Scheme.
      ;;
      (let ((entry	(malloc-block/c sizeof-dirent))
	    (*result	(malloc-block/c sizeof-pointer)))
	(receive (retval errno)
	    (platform:readdir_r stream entry *result)
	  (cond ((not (= 0 retval))
		 (raise-errno-error 'readdir errno stream))
		((pointer-null? (pointer-ref-c-pointer *result 0))
		 pointer-null)
		(else
		 (memcpy result entry sizeof-dirent)
		 result)))))))

(define (rewinddir stream)
  (platform:rewinddir stream))

(define (telldir stream)
  (platform:telldir stream))

(define (seekdir stream position)
  (platform:seekdir stream position))

(define (dirent-name->string struct-dirent-pointer)
  (cstring->string (struct-dirent-d_name-ref struct-dirent-pointer)))


;;;; tree walk functions

(define (ftw pathname inspector-callback descriptors)
  (with-compensations
    (receive (result errno)
	(platform:ftw (string->cstring/c pathname) inspector-callback descriptors)
      (if (= -1 result)
	  (raise-errno-error 'ftw errno (list pathname inspector-callback descriptors))
	result))))

(define (nftw pathname inspector-callback descriptors flags)
  (with-compensations
    (receive (result errno)
	(platform:nftw (string->cstring/c pathname) inspector-callback descriptors flags)
      (if (= -1 result)
	  (raise-errno-error 'ftw errno (list pathname inspector-callback descriptors flags))
	result))))

(define (make-ftw-callback scheme-function)
  (make-c-callback* int
		    (lambda (pathname-cstr struct-stat flag)
		      (guard (E (else -1))
			(scheme-function (cstring->string pathname-cstr)
					 (if (= 0 (bitwise-and flag FTW_NS))
					     #f
					   (pointer-><stat> struct-stat))
					 flag)))
		    (char* void* int)))

(define (make-nftw-callback scheme-function)
  (make-c-callback* int
		    (lambda (pathname-cstr struct-stat flag struct-ftw)
		      (guard (E (else -1))
			(scheme-function (cstring->string pathname-cstr)
					 (if (= 0 (bitwise-and flag FTW_NS))
					     #f
					   (pointer-><stat> struct-stat))
					 flag
					 (struct-FTW-base-ref  struct-ftw)
					 (struct-FTW-level-ref struct-ftw))))
		    (char* void* int void*)))

;;; --------------------------------------------------------------------

(define (%directory-entries dir)
  (let loop ((entry  (readdir_r dir))
	     (layout '()))
    (if (pointer-null? entry)
	layout
      (loop (readdir_r dir)
	    (cons (dirent-name->string entry) layout)))))

(define (directory-entries pathname)
  (with-compensations
    (%directory-entries (opendir/c pathname))))

(define (directory-entries/fd fd)
  (with-compensations
    (%directory-entries (fdopendir/c fd))))


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

    (define (%readlink c-pathname buffer provided-size)
      (receive (required-size errno)
	  (platform:readlink c-pathname buffer provided-size)
	(cond ((= -1 required-size)
	       (raise-errno-error 'readlink errno pathname))
	      ((<= provided-size required-size)
		;READLINK  returns  the  number  of  bytes  copied  into
		;BUFFER, read carefully the documentation!!!
	       (let ((provided-size (* 2 required-size)))
		 (%readlink c-pathname (malloc-block/c provided-size) provided-size)))
	      (else
	       (cstring->string buffer required-size)))))

    (let ((provided-size 4))
      (%readlink (string->cstring/c pathname) (malloc-block/c provided-size) provided-size))))

(define (realpath pathname)
  (with-compensations
    (receive (buffer errno)
	(platform:realpath (string->cstring/c pathname) pointer-null)
      (if (pointer-null? buffer)
	  (raise-errno-error 'realpath errno pathname)
	(begin0
	    (cstring->string buffer)
	  (primitive-free buffer))))))


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
	(platform:fchown (fd->integer fd) owner-id group-id)
      (when (= -1 result)
	(raise-errno-error 'fchown errno (list fd owner-id group-id)))
      result)))


;;;; changing permissions

(define (%mode->value mode)
  (if (integer? mode)
      mode
    (access-permissions->value mode)))

(define (umask mask)
  (with-compensations
    (receive (result errno)
	(platform:umask (%mode->value mask))
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
	(platform:chmod (string->cstring/c pathname) (%mode->value mode))
      (when (= -1 result)
	(raise-errno-error 'chmod errno (list pathname mode)))
      result)))

(define (fchmod fd mode)
  (with-compensations
    (receive (result errno)
	(platform:fchmod (fd->integer fd) (%mode->value mode))
      (when (= -1 result)
	(raise-errno-error 'fchmod errno (list fd mode)))
      result)))


;;;; testing access

(define (access pathname mask)
  (with-compensations
    (receive (result errno)
	(platform:access (string->cstring/c pathname)
			 (if (integer? mask)
			     mask
			   (access-flags->value mask)))
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
      (let ((struct-utimbuf* (malloc-block/c sizeof-utimbuf)))
	(struct-utimbuf-actime-set!  struct-utimbuf* access-time)
	(struct-utimbuf-modtime-set! struct-utimbuf* modification-time)
	(receive (result errno)
	    (platform:utime (string->cstring/c pathname) struct-utimbuf*)
	  (if (= -1 result)
	      (raise-errno-error 'utime errno (list pathname access-time modification-time))
	    result)))))

   ((pathname)	;set the times to the current time
    (receive (result errno)
	(platform:utime (string->cstring/c pathname) pointer-null)
      (if (= -1 result)
	  (raise-errno-error 'utime errno pathname)
	result)))))

(define utimes
  (case-lambda
   ((pathname access-time-sec access-time-usec modification-time-sec modification-time-usec)
    (%real-utimes (lambda (*arry)
		    (platform:utimes (string->cstring/c pathname) *arry))
		  'utimes pathname
		  access-time-sec access-time-usec
		  modification-time-sec modification-time-usec))
   ((pathname)
    (%real-utimes (lambda (*arry)
		    (platform:utimes (string->cstring/c pathname) *arry))
		  'utimes pathname))))

(define %real-utimes
  (case-lambda
   ((func funcname obj
	  access-time-sec	    access-time-usec
	  modification-time-sec modification-time-usec)
    (with-compensations
      (let* ((*arry	(malloc-block/c (* 2 strideof-timeval)))
	     (*atime	*arry)
	     (*mtime	(pointer-add *arry strideof-timeval)))
	(struct-timeval-tv_sec-set!  *atime access-time-sec)
	(struct-timeval-tv_usec-set! *atime access-time-usec)
	(struct-timeval-tv_sec-set!  *mtime modification-time-sec)
	(struct-timeval-tv_usec-set! *mtime modification-time-usec)
	(receive (result errno)
	    (func *arry)
	  (when (= -1 result)
	    (raise-errno-error funcname errno
			       (list obj
				     access-time-sec access-time-usec
				     modification-time-sec modification-time-usec)))
	  result))))
   ((func funcname obj)
    (receive (result errno)
	(func pointer-null)
      (if (= -1 result)
	  (raise-errno-error funcname errno obj)
	result)))))


;;;; file size

(define (file-size obj)
  (cond ((or (string? obj) (symbol? obj))
	 (with-compensations
	   (letrec ((fd (compensate
			    (posix:open obj O_RDONLY 0)
			  (with
			   (posix:close fd)))))
	     (file-size fd))))
	((fd? obj)
	 (with-compensations
	   (letrec ((pos (compensate
			     (posix:lseek obj 0 SEEK_CUR)
			   (with
			    (posix:lseek obj pos SEEK_SET)))))
	     (posix:lseek obj 0 SEEK_END))))
	(else
	 (error 'file-size
	   "expected file descriptor or file pathname" obj))))

(define (ftruncate obj length)
  (cond ((or (string? obj) (symbol? obj))
	 (with-compensations
	   (letrec ((fd (compensate
			    (posix:open obj O_WRONLY 0)
			  (with
			   (posix:close fd)))))
	     (platform:ftruncate (fd->integer fd) length))))
	((fd? obj)
	 (receive (result errno)
	     (platform:ftruncate (fd->integer obj) length)
	   (when (= -1 result)
	     (raise-errno-error 'ftruncate errno (list obj length)))
	   result))
	(else
	 (error 'ftruncate
	   "expected file descriptor or file pathname" obj))))

(define (truncate pathname length)
  (with-compensations
    (receive (result errno)
	(platform:truncate (string->cstring/c pathname) length)
      (if (= -1 result)
	  (raise-errno-error 'truncate errno (list pathname length))
	result))))


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

(define (mkstemp template)
  (with-compensations
    (let ((p	(string->cstring/c template)))
      (receive (result errno)
	  (platform:mkstemp p)
	(if (= -1 result)
	    (raise-errno-error 'mktemp errno template)
	  (values (integer->fd result) (cstring->string p)))))))

(define (mkdtemp template)
  (with-compensations
    (let ((p	(string->cstring/c template)))
      (receive (result errno)
	  (platform:mkdtemp p)
	(if (pointer-null? result)
	    (raise-errno-error 'mkdtemp errno template)
	  (cstring->string p))))))


;;; stat interface

(define (pointer-><stat> struct-stat*)
  ;;Some "struct  stat" field  may be unimplemented,  so we  default its
  ;;value to #f.
  (let-syntax ((get	(syntax-rules ()
			  ((_ ?getter)
			   (guard (exc (else #f))
			     (?getter struct-stat*))))))
    (make-<stat>
     (get platform:struct-stat-st_mode-ref)
     (get platform:struct-stat-st_ino-ref)
     (get platform:struct-stat-st_dev-ref)
     (get platform:struct-stat-st_nlink-ref)
     (get platform:struct-stat-st_uid-ref)
     (get platform:struct-stat-st_gid-ref)
     (get platform:struct-stat-st_size-ref)
     (get platform:struct-stat-st_atime-ref)
     (get platform:struct-stat-st_atime_usec-ref)
     (get platform:struct-stat-st_mtime-ref)
     (get platform:struct-stat-st_mtime_usec-ref)
     (get platform:struct-stat-st_ctime-ref)
     (get platform:struct-stat-st_ctime_usec-ref)
     (get platform:struct-stat-st_blocks-ref)
     (get platform:struct-stat-st_blksize-ref))))

(define (%real-stat func funcname pathname)
  (with-compensations
    (let ((struct-stat*	(malloc-block/c platform:sizeof-stat)))
      (receive (result errno)
	  (func (string->cstring/c pathname) struct-stat*)
	(when (= -1 result)
	  (raise-errno-error funcname errno pathname))
	(pointer-><stat> struct-stat*)))))

(define (stat pathname)
  (%real-stat platform:stat 'stat pathname))

(define (lstat pathname)
  (%real-stat platform:lstat 'lstat pathname))

(define (fstat fd)
  (with-compensations
    (let ((struct-stat* (malloc-block/c platform:sizeof-stat)))
      (receive (result errno)
	  (platform:fstat (fd->integer fd) struct-stat*)
	(when (= -1 result)
	  (raise-errno-error 'fstat errno fd))
	(pointer-><stat> struct-stat*)))))


(define (%type-inspection funcname pred obj)
  (cond ((fd? obj)
	 (not (= 0 (pred (<stat>-mode (fstat obj))))))

	((or (string? obj) (symbol? obj)) ;pathname
	 (not (= 0 (pred (<stat>-mode (stat obj))))))

	((and (integer? obj) (exact? obj)) ;"st_mode" field in "struct stat"
	 (not (= 0 (pred obj))))

	(else
	 (error funcname "expected file descriptor, file pathname or st_mode integer" obj))))

;;; --------------------------------------------------------------------

(define (file-is-directory? obj)
  (%type-inspection 'file-is-directory? platform:S_ISDIR obj))

(define (file-is-character-special? obj)
  (%type-inspection 'file-is-character-special? platform:S_ISCHR obj))

(define (file-is-block-special? obj)
  (%type-inspection 'file-is-block-special? platform:S_ISBLK obj))

(define (file-is-regular? obj)
  (%type-inspection 'file-is-regular? platform:S_ISREG obj))

(define (file-is-fifo? obj)
  (%type-inspection 'file-is-fifo? platform:S_ISFIFO obj))

(define (file-is-socket? obj)
  (%type-inspection 'file-is-socket? platform:S_ISSOCK obj))

(define (file-is-symbolic-link? mode/pathname)
  (cond ((and (integer? mode/pathname) (exact? mode/pathname))
	 (not (= 0 (platform:S_ISLNK mode/pathname))))

	((or (string? mode/pathname) (symbol? mode/pathname)) ;pathname
	 (not (= 0 (platform:S_ISLNK (<stat>-mode (lstat mode/pathname))))))

	(else
	 (error 'file-is-symbolic-link?
	   "file pathname or st_mode integer" mode/pathname))))


(define (%pointer-type-inspection funcname getter obj)
  (cond ((fd? obj)
	 (with-compensations
	   (let ((struct-stat* (malloc-block/c platform:sizeof-stat)))
	     (receive (result errno)
		 (platform:fstat (fd->integer obj) struct-stat*)
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
  (%pointer-type-inspection 'file-is-message-queue? platform:S_TYPEISMQ obj))

(define (file-is-semaphore? obj)
  (%pointer-type-inspection 'file-is-semaphore? platform:S_TYPEISSEM obj))

(define (file-is-shared-memory? obj)
  (%pointer-type-inspection 'file-is-semaphore? platform:S_TYPEISSHM obj))


(define (%mode-inspection the-stat funcname mask obj)
  (define (set? mode)
    (not (= 0 (bitwise-and mask mode))))
  (cond ((and (eq? the-stat 'stat) (fd? obj))
	 (set? (<stat>-mode (fstat obj))))

	((or (string? obj) (symbol? obj)) ;pathname
	 (set? (<stat>-mode ((case the-stat
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
    (let ((mode (<stat>-mode record)))
      (bitwise-and
       (bitwise-ior S_ISUID S_ISGID S_IRWXU S_IRWXG S_IRWXO)
       mode)))

  (cond ((fd? obj)
	 (get-mode (fstat obj)))

	((or (string? obj) (symbol? obj)) ;pathname
	 (get-mode (stat obj)))

	(else
	 (error 'file-permissions "expected file descriptor or file pathname" obj))))

(define (lfile-permissions obj)
  (bitwise-and (bitwise-ior S_ISUID S_ISGID S_IRWXU S_IRWXG S_IRWXO)
	       (<stat>-mode (lstat obj))))


;;;; done

)

;;; end of file
