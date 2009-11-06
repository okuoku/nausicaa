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
    ;; working directory
    getcwd		chdir		fchdir

    ;; directory access
    opendir		fdopendir	dirfd
    closedir		readdir		rewinddir
    telldir		seekdir		scandir
    alphasort		versionsort

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
    umask		chmod		fchmod

    ;; access test
    access

    ;; file times, notice that UTIMES, LUTIMES and FUTIMES are glibc stuff
    utime

    ;; file size
    ftruncate

    struct-dirent-d_name-ptr-ref)
  (import (except (rnrs)
		  remove truncate)
    (foreign ffi)
    (foreign posix sizeof))


(define dummy
  (shared-object self-shared-object))

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

(define-c-function/with-errno readdir
  (pointer readdir (pointer)))

(define-c-function/with-errno closedir
  (int closedir (pointer)))

(define-c-function rewinddir
  (void rewinddir (pointer)))

(define-c-function telldir
  (long telldir (pointer)))

(define-c-function seekdir
  (void seekdir (pointer long)))

(define-c-function/with-errno scandir
  (int scandir (char* pointer callback callback)))

(define-c-function alphasort
  (int alphasort (void* void*)))

(define-c-function versionsort
  (int versionsort (void* void*)))

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
;;; temporary files

(define-c-function tmpnam
  (char* tmpnam (char*)))

(define-c-function/with-errno mktemp
  (char* mktemp (char*)))

(define-c-function/with-errno mkstemp
  (int mkstemp (char*)))

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

;;; --------------------------------------------------------------------
;;; tile size

(define-c-function/with-errno ftruncate
  (int ftruncate (int off_t)))


(define dummy2
  (shared-object (open-shared-object* 'libnausicaa-posix1.so)))

(define-c-function struct-dirent-d_name-ptr-ref
  (char* nausicaa_posix_dirent_d_name_ptr_ref (void*)))


;;;; done

)

;;; end of file
