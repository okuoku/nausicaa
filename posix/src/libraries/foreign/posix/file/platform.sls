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
    ftw			nftw

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
    (foreign ffi)
    (foreign posix shared-object)
    (foreign posix sizeof))


(define-c-functions/with-errno libc-shared-object
  (pathconf		(long pathconf (char* int)))
  (fpathconf		(long fpathconf (int int)))

  (getcwd		(char* getcwd (char* size_t)))
  (chdir		(int chdir (char*)))
  (fchdir		(int fchdir (int)))

  (opendir		(pointer opendir (char*)))
  (fdopendir		(pointer fdopendir (int)))
  (dirfd		(int dirfd (pointer)))

  (closedir		(int closedir (pointer)))

  (link			(int link (char* char*)))
  (symlink		(int symlink (char* char*)))
  (readlink		(int readlink (char* char* size_t)))
  (realpath		(char* realpath (char* char*)))

  (unlink		(int unlink (char*)))
  (rmdir		(int rmdir (char*)))
  (remove		(int remove (char*)))

  (rename		(int rename (char* char*)))

  (mkdir		(int mkdir (char* mode_t)))

  (chown		(int chown (char* uid_t gid_t)))
  (fchown		(int fchown (int int int)))

  (umask		(mode_t umask (mode_t)))
  (chmod		(int chmod (char* mode_t)))
  (fchmod		(int fchmod (int int)))

  (access		(int access (char* int)))

  (utime		(int utime (char* pointer)))
  (utimes		(int utimes (char* pointer)))

  (mkstemp		(int mkstemp (char*)))
  (mkdtemp		(char* mkdtemp (char*))))

(define-c-functions libc-shared-object
  (rewinddir		(void rewinddir (pointer)))
  (telldir		(long telldir (pointer)))
  (seekdir		(void seekdir (pointer long))))

(define-c-functions/with-errno libnausicaa-posix
  (readdir		(pointer nausicaa_posix_readdir (pointer)))
  (readdir_r		(int nausicaa_posix_readdir_r (pointer pointer pointer)))
  (ftw			(int nausicaa_posix_ftw (char* callback int)))
  (nftw			(int nausicaa_posix_nftw (char* callback int int)))

  (ftruncate		(int nausicaa_posix_ftruncate (int off_t)))
  (truncate		(int nausicaa_posix_truncate (char* off_t))))

;;;This  does the  same work  of "struct-dirent-d_name-ref",  but  it is
;;;implemented  as Nausicaa functions.   It is  here only  for debugging
;;;purposes.
;;;
;;;(define-c-functions libnausicaa-posix
;;;   (struct-dirent-d_name-ptr-ref (char* nausicaa_posix_dirent_d_name_ptr_ref (void*))))


;;;; done

)

;;; end of file
