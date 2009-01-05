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



;;;; setup

(library (posix file)
  (export

    ;; working directory
    getcwd		primitive-getcwd	primitive-getcwd-function
    chdir		primitive-chdir		primitive-chdir-function
    fchdir		primitive-fchdir	primitive-fchdir-function
    (rename (getcwd pwd))

    ;; directory access
    opendir		primitive-opendir	primitive-opendir-function
    fdopendir		primitive-fdopendir	primitive-fdopendir-function
    dirfd		primitive-dirfd		primitive-dirfd-function
    closedir		primitive-closedir	primitive-closedir-function
    readdir		primitive-readdir	primitive-readdir-function
    rewinddir		primitive-rewinddir	primitive-rewinddir-function
    telldir		primitive-telldir	primitive-telldir-function
    seekdir		primitive-seekdir	primitive-seekdir-function

    opendir/compensated		(rename (opendir/compensated opendir/c))
    fdopendir/compensated	(rename (fdopendir/compensated fdopendir/c))
    directory-list	directory-list/fd

    ;; links
    link		primitive-link		primitive-link-function
    symlink		primitive-symlink	primitive-symlink-function
    readlink		primitive-readlink	primitive-readlink-function
    realpath		primitive-realpath	primitive-realpath-function

    ;; removing
    unlink		primitive-unlink	primitive-unlink-function
    rmdir		primitive-rmdir		primitive-rmdir-function
    remove		primitive-remove	primitive-remove-function

    ;; renaming
    rename		primitive-rename	primitive-rename-function

    ;; mkdir
    mkdir		primitive-mkdir		primitive-mkdir-function

    ;; temporary files
    tmpnam		primitive-tmpnam	primitive-tmpnam-function
    mktemp		primitive-mktemp	primitive-mktemp-function
    mkstemp		primitive-mkstemp	primitive-mkstemp-function
    )
  (import (except (r6rs) remove)
    (uriel lang)
    (uriel foreign)
    (posix sizeof)
    (posix file platform))

  (define dummy
    (shared-object self-shared-object))



;;;; working directory

(define (primitive-getcwd)
  (let loop ((buflen 1024))
    (with-compensations
      (let ((buffer (malloc-block/c buflen)))
	(receive (cstr errno)
	    (platform-getcwd buffer buflen)
	  (if (and (= 0 (pointer->integer cstr))
		   (or (= EINVAL errno)
		       (= ERANGE errno)))
	      (loop (* 2 buflen))
	    (begin
	      (when (pointer-null? cstr)
		(raise-errno-error 'primitive-getcwd errno))
	      (cstring->string buffer))))))))

(define (primitive-chdir directory-pathname)
  (with-compensations
    (receive (result errno)
	(platform-chdir (string->cstring/c directory-pathname))
      (unless (= 0 result)
	(raise-errno-error 'primitive-chdir errno
			   directory-pathname))
      result)))

(define (primitive-fchdir fd)
  (receive (result errno)
      (platform-fchdir fd)
    (unless (= 0 result)
      (raise-errno-error 'primitive-fchdir errno fd))
    result))

;;; --------------------------------------------------------------------

(define-primitive-parameter
  primitive-getcwd-function primitive-getcwd)

(define-primitive-parameter
  primitive-chdir-function primitive-chdir)

(define-primitive-parameter
  primitive-fchdir-function primitive-fchdir)

;;; --------------------------------------------------------------------

(define (getcwd)
  ((primitive-getcwd-function)))

(define (chdir pathname)
  ((primitive-chdir-function) pathname))

(define (fchdir fd)
  ((primitive-fchdir-function) fd))



;;;; directory access

(define (primitive-opendir pathname)
  (with-compensations
    (receive (result errno)
	(platform-opendir (string->cstring/c pathname))
      (when (pointer-null? result)
	(raise-errno-error 'primitive-opendir errno pathname))
      result)))

(define (primitive-fdopendir fd)
  (receive (result errno)
      (platform-fdopendir fd)
    (when (pointer-null? result)
      (raise-errno-error 'primitive-fdopendir errno fd))
    result))

(define (primitive-dirfd stream)
  (receive (result errno)
      (platform-dirfd stream)
    (when (= -1 result)
      (raise-errno-error 'primitive-dirfd errno stream))
    result))

(define (primitive-closedir stream)
  (receive (result errno)
      (platform-closedir stream)
    (when (= -1 result)
      (raise-errno-error 'primitive-closedir errno stream))
    result))

(define (primitive-readdir stream)
  (receive (result errno)
      (platform-readdir stream)
    ;;Here  we assume  that errno  is  set to  zero by  PLATFORM-READDIR
    ;;before the call to the foreign function.
;;;FIXME  temporarily commented out  waiting for  Ikarus and  Ypsilon to
;;;provide an "errno" setter.
;;     (when (and (pointer-null? result)
;; 	       (not (= 0 errno)))
;;       (raise-errno-error 'primitive-readdir errno stream))
    result))

(define (primitive-rewinddir stream)
  (platform-rewinddir stream))

(define (primitive-telldir stream)
  (platform-telldir stream))

(define (primitive-seekdir stream position)
  (platform-seekdir stream position))

;;; --------------------------------------------------------------------

(define-primitive-parameter
  primitive-opendir-function primitive-opendir)

(define-primitive-parameter
  primitive-fdopendir-function primitive-fdopendir)

(define-primitive-parameter
  primitive-dirfd-function primitive-dirfd)

(define-primitive-parameter
  primitive-closedir-function primitive-closedir)

(define-primitive-parameter
  primitive-readdir-function primitive-readdir)

(define-primitive-parameter
  primitive-rewinddir-function primitive-rewinddir)

(define-primitive-parameter
  primitive-telldir-function primitive-telldir)

(define-primitive-parameter
  primitive-seekdir-function primitive-seekdir)

;;; --------------------------------------------------------------------

(define (opendir pathname)
  ((primitive-opendir-function) pathname))

(define (fdopendir fd)
  ((primitive-fdopendir-function) fd))

(define (dirfd stream)
  ((primitive-dirfd-function) stream))

(define (closedir stream)
  ((primitive-closedir-function) stream))

(define (readdir stream)
  ((primitive-readdir-function) stream))

(define (rewinddir stream)
  ((primitive-rewinddir-function) stream))

(define (telldir stream)
  ((primitive-telldir-function) stream))

(define (seekdir stream position)
  ((primitive-seekdir-function) stream position))

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

(define (real-primitive-link func funcname oldname newname)
  (with-compensations
    (receive (result errno)
	(func (string->cstring/c oldname)
	      (string->cstring/c newname))
      (when (= -1 result)
	(raise-errno-error funcname errno
			   (list oldname newname)))
      result)))

(define (primitive-link oldname newname)
  (real-primitive-link platform-link 'primitive-link
		       oldname newname))

(define (primitive-symlink oldname newname)
  (real-primitive-link platform-symlink 'primitive-symlink
		       oldname newname))

(define (primitive-readlink pathname)
  (with-compensations
    (let ((c-pathname	(string->cstring/c pathname)))
      (receive (size errno)
	  (platform-readlink c-pathname pointer-null 0)
	(when (= -1 size)
	  (raise-errno-error 'primitive-readlink errno pathname))
	(let ((buffer	(malloc-block/c size)))
	  (receive (result errno)
	      (platform-readlink c-pathname buffer size)
	    (when (= -1 size)
	      (raise-errno-error 'primitive-readlink errno pathname))
	    (cstring->string/len buffer result)))))))

(define (primitive-realpath pathname)
  (with-compensations
    (receive (buffer errno)
	(platform-realpath (string->cstring/c pathname)
			   pointer-null)
      (when (pointer-null? buffer)
	(raise-errno-error 'primitive-realpath errno pathname))
      (begin0
	  (cstring->string buffer)
	(primitive-free buffer)))))

;;; --------------------------------------------------------------------

(define-primitive-parameter
  primitive-link-function primitive-link)

(define-primitive-parameter
  primitive-symlink-function primitive-symlink)

(define-primitive-parameter
  primitive-readlink-function primitive-readlink)

(define-primitive-parameter
  primitive-realpath-function primitive-realpath)

;;; --------------------------------------------------------------------

(define (link oldname newname)
  ((primitive-link-function) oldname newname))

(define (symlink oldname newname)
  ((primitive-symlink-function) oldname newname))

(define (readlink pathname)
  ((primitive-readlink-function) pathname))

(define (realpath pathname)
  ((primitive-realpath-function) pathname))



;;;; owner

(define (primitive-chown pathname owner-id group-id)
  (with-compensations
    (receive (result errno)
	(platform-chown (string->cstring/c pathname)
			owner-id group-id)
      (when (= -1 result)
	(raise-errno-error 'primitive-chown errno
			   (list pathname owner-id group-id)))
      result)))

(define (primitive-fchown fd owner-id group-id)
  (with-compensations
    (receive (result errno)
	(platform-fchown fd owner-id group-id)
      (when (= -1 result)
	(raise-errno-error 'primitive-fchown errno
			   (list fd owner-id group-id)))
      result)))

;;; --------------------------------------------------------------------

(define-primitive-parameter
  primitive-chown-function primitive-chown)

(define-primitive-parameter
  primitive-fchown-function primitive-fchown)

;;; --------------------------------------------------------------------

(define (chown pathname owner-id group-id)
  ((primitive-chown-function) pathname owner-id group-id))

(define (fchown fd owner-id group-id)
  ((primitive-fchown-function) fd owner-id group-id))



;;;; removing

(define (primitive-unlink pathname)
  (with-compensations
    (receive (result errno)
	(platform-unlink (string->cstring/c pathname))
      (when (= -1 result)
	(raise-errno-error 'primitive-unlink errno pathname))
      result)))

(define (primitive-remove pathname)
  (with-compensations
    (receive (result errno)
	(platform-remove (string->cstring/c pathname))
      (when (= -1 result)
	(raise-errno-error 'primitive-remove errno pathname))
      result)))

(define (primitive-rmdir pathname)
  (with-compensations
    (receive (result errno)
	(platform-rmdir (string->cstring/c pathname))
      (when (= -1 result)
	(raise-errno-error 'primitive-rmdir errno pathname))
      result)))

;;; --------------------------------------------------------------------

(define-primitive-parameter
  primitive-unlink-function primitive-unlink)

(define-primitive-parameter
  primitive-remove-function primitive-remove)

(define-primitive-parameter
  primitive-rmdir-function primitive-rmdir)

;;; --------------------------------------------------------------------

(define (unlink pathname)
  ((primitive-unlink-function) pathname))

(define (remove pathname)
  ((primitive-remove-function) pathname))

(define (rmdir pathname)
  ((primitive-rmdir-function) pathname))



;;;; renaming

(define (primitive-rename oldname newname)
  (with-compensations
    (receive (result errno)
	(platform-rename (string->cstring/c oldname)
			 (string->cstring/c newname))
      (when (= -1 result)
	(raise-errno-error 'primitive-rename errno
			   (list oldname newname)))
      result)))

(define-primitive-parameter
  primitive-rename-function primitive-rename)

(define (rename oldname newname)
  ((primitive-rename-function) oldname newname))



;;;; making directories

(define (primitive-mkdir pathname mode)
  (with-compensations
    (receive (result errno)
	(platform-mkdir (string->cstring/c pathname) mode)
      (when (= -1 result)
	(raise-errno-error 'primitive-mkdir errno
			   (list pathname mode)))
      result)))

(define-primitive-parameter
  primitive-mkdir-function primitive-mkdir)

(define (mkdir pathname mode)
  ((primitive-mkdir-function) pathname mode))



;;;; temporary files

(define (primitive-tmpnam)
  (with-compensations
    (let ((p	(malloc-block/c (+ 1 L_tmpnam))))
      (platform-tmpnam p)
      (cstring->string p))))

(define (primitive-mktemp template)
  (with-compensations
    (let ((p	(string->cstring/c template)))
      (receive (result errno)
	  (platform-mktemp p)
	(when (pointer-null? result)
	  (raise-errno-error 'primitive-mktemp errno template))
	(cstring->string p)))))

(define (primitive-mkstemp template)
  (with-compensations
    (let ((p	(string->cstring/c template)))
      (receive (result errno)
	  (platform-mkstemp p)
	(when (= -1 result)
	  (raise-errno-error 'primitive-mktemp errno template))
	(values result
		(cstring->string p))))))

;;; --------------------------------------------------------------------

(define-primitive-parameter
  primitive-tmpnam-function primitive-tmpnam)

(define-primitive-parameter
  primitive-mktemp-function primitive-mktemp)

(define-primitive-parameter
  primitive-mkstemp-function primitive-mkstemp)

;;; --------------------------------------------------------------------

(define (tmpnam)
  ((primitive-tmpnam-function)))

(define (mktemp template)
  ((primitive-mktemp-function) template))

(define (mkstemp template)
  ((primitive-mkstemp-function) template))




;;;; done

)

;;; end of file
