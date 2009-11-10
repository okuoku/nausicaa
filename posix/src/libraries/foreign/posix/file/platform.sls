;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to platform functions that handle files
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


(library (foreign posix file platform)
  (export
    ;; system inspection
    pathconf		fpathconf

    ;; working directory
    getcwd		chdir		fchdir

    ;; directory access
    opendir		fdopendir	dirfd
    closedir		readdir		readdir_r
    rewinddir		telldir		seekdir
    ftw		nftw

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
    umask		chmod		fchmod

    ;; access test
    access

    ;; file times, notice that LUTIMES and FUTIMES are glibc stuff
    utime		utimes

    ;; file size
    ftruncate		truncate

    ;; temporary files
    mkstemp		mkdtemp

;;;This  does the  same work  of "struct-dirent-d_name-ref",  but  it is
;;;implemented  as Nausicaa functions.   It is  here only  for debugging
;;;purposes.
;;;
;;; struct-dirent-d_name-ptr-ref
    )
  (import (except (rnrs)
		  remove truncate)
    (only (parameters)
	  parametrise)
    (foreign ffi)
    (foreign posix shared-object)
    (foreign posix sizeof))


(define dummy
  (shared-object standard-c-library))

;;; --------------------------------------------------------------------

(define-c-function/with-errno pathconf
  (long pathconf (char* int)))

(define-c-function/with-errno fpathconf
  (long fpathconf (int int)))

;;; --------------------------------------------------------------------
;;; working directory

(define-c-function/with-errno getcwd
  (char* getcwd (char* size_t)))

(define-c-function/with-errno chdir
  (int chdir (char*)))

(define-c-function/with-errno fchdir
  (int fchdir (int)))

;;; --------------------------------------------------------------------
;;; directory access

(define-c-function/with-errno opendir
  (pointer opendir (char*)))

(define-c-function/with-errno fdopendir
  (pointer fdopendir (int)))

(define-c-function/with-errno dirfd
  (int dirfd (pointer)))

(define readdir
  (parametrise ((shared-object libnausicaa-posix))
    (make-c-function/with-errno pointer nausicaa_posix_readdir (pointer))))

(define readdir_r
  (parametrise ((shared-object libnausicaa-posix))
    (make-c-function/with-errno int nausicaa_posix_readdir_r (pointer pointer pointer))))

(define-c-function/with-errno closedir
  (int closedir (pointer)))

(define-c-function rewinddir
  (void rewinddir (pointer)))

(define-c-function telldir
  (long telldir (pointer)))

(define-c-function seekdir
  (void seekdir (pointer long)))

(define-c-function/with-errno ftw
  (int ftw (char* callback int)))

(define-c-function/with-errno nftw
  (int nftw (char* callback int int)))

;;; --------------------------------------------------------------------
;;; links

(define-c-function/with-errno link
  (int link (char* char*)))

(define-c-function/with-errno symlink
  (int symlink (char* char*)))

(define-c-function/with-errno readlink
  (int readlink (char* char* size_t)))

(define-c-function/with-errno realpath
  (char* realpath (char* char*)))

;;; --------------------------------------------------------------------
;;; removing

(define-c-function/with-errno unlink
  (int unlink (char*)))

(define-c-function/with-errno rmdir
  (int rmdir (char*)))

(define-c-function/with-errno remove
  (int remove (char*)))

;;; --------------------------------------------------------------------
;;; renaming

(define-c-function/with-errno rename
  (int rename (char* char*)))

;;; --------------------------------------------------------------------
;;; mkdir

(define-c-function/with-errno mkdir
  (int mkdir (char* mode_t)))

;;; --------------------------------------------------------------------
;;; changing owner

(define-c-function/with-errno chown
  (int chown (char* uid_t gid_t)))

(define-c-function/with-errno fchown
  (int fchown (int int int)))

;;; --------------------------------------------------------------------
;;; changing permissions

(define-c-function/with-errno umask
  (mode_t umask (mode_t)))

(define-c-function/with-errno chmod
  (int chmod (char* mode_t)))

(define-c-function/with-errno fchmod
  (int fchmod (int int)))

;;; --------------------------------------------------------------------
;;; access test

(define-c-function/with-errno access
  (int access (char* int)))

;;; --------------------------------------------------------------------
;;; file times

(define-c-function/with-errno utime
  (int utime (char* pointer)))

(define-c-function/with-errno utimes
  (int utimes (char* pointer)))

;;; --------------------------------------------------------------------
;;; tile size

(define ftruncate
  (parametrise ((shared-object libnausicaa-posix))
    (make-c-function/with-errno int nausicaa_posix_ftruncate (int off_t))))

(define truncate
  (parametrise ((shared-object libnausicaa-posix))
    (make-c-function/with-errno int nausicaa_posix_truncate (char* off_t))))

;;; --------------------------------------------------------------------
;;; struct dirent accessors

;;;This  does the  same work  of "struct-dirent-d_name-ref",  but  it is
;;;implemented  as Nausicaa functions.   It is  here only  for debugging
;;;purposes.
;;;
;;; (define struct-dirent-d_name-ptr-ref
;;;   (parametrise ((shared-object libnausicaa-posix))
;;;     (make-c-function char* nausicaa_posix_dirent_d_name_ptr_ref (void*))))

;;; --------------------------------------------------------------------
;;; temporary files

(define-c-function/with-errno mkstemp
  (int mkstemp (char*)))

(define-c-function/with-errno mkdtemp
  (char* mkdtemp (char*)))


;;;; done

)

;;; end of file
