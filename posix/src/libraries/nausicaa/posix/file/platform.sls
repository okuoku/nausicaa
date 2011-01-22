;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to platform functions that handle files
;;;Date: Thu Jan  1, 2009
;;;
;;;Abstract
;;;
;;;	The interface to "stat()",  "fstat()" and "lstat()" makes use of
;;;	the stubs functions in  "libnausicaa-posix.so"; also the size of
;;;	"struct stat"  is not determined  correctly by the  GNU Autoconf
;;;	macros.   This is because  the stat  interface depends  upon the
;;;	platform being 32-bitt or 64-bits (this affects also the size of
;;;	the stat structure).
;;;
;;;Copyright (c) 2009, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (nausicaa posix file platform)
  (export
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

;;; --------------------------------------------------------------------

    stat		lstat		fstat

    S_ISDIR		S_ISCHR		S_ISBLK
    S_ISREG		S_ISFIFO	S_ISLNK
    S_ISSOCK

    S_TYPEISMQ		S_TYPEISSEM	S_TYPEISSHM

    sizeof-stat

    ;; accessors for "struct stat"
    struct-stat-st_mode-ref
    struct-stat-st_ino-ref
    struct-stat-st_dev-ref
    struct-stat-st_nlink-ref
    struct-stat-st_uid-ref
    struct-stat-st_gid-ref
    struct-stat-st_size-ref
    struct-stat-st_atime-ref
    struct-stat-st_atime_usec-ref
    struct-stat-st_mtime-ref
    struct-stat-st_mtime_usec-ref
    struct-stat-st_ctime-ref
    struct-stat-st_ctime_usec-ref
    struct-stat-st_blocks-ref
    struct-stat-st_blksize-ref)
  (import (except (rnrs) remove truncate)
    (nausicaa ffi)
    (nausicaa ffi sizeof)
    (nausicaa posix shared-object)
    (nausicaa posix sizeof))


(define-c-functions/with-errno libc-shared-object
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


(define-c-functions libnausicaa-posix
  (%sizeof-stat		(int nausicaa_posix_sizeof_stat (void))))

(define sizeof-stat
  (%sizeof-stat))

(define-c-functions libnausicaa-posix
  (struct-stat-st_mode-ref		(mode_t nausicaa_posix_stat_st_mode_ref (void*)))
  (struct-stat-st_ino-ref		(ino_t nausicaa_posix_stat_st_ino_ref (void*)))
  (struct-stat-st_dev-ref		(dev_t nausicaa_posix_stat_st_dev_ref (void*)))
  (struct-stat-st_nlink-ref		(nlink_t nausicaa_posix_stat_st_nlink_ref (void*)))
  (struct-stat-st_uid-ref		(uid_t nausicaa_posix_stat_st_uid_ref (void*)))
  (struct-stat-st_gid-ref		(gid_t nausicaa_posix_stat_st_gid_ref (void*)))
  (struct-stat-st_size-ref		(off_t nausicaa_posix_stat_st_size_ref (void*)))
  (struct-stat-st_atime-ref		(time_t nausicaa_posix_stat_st_atime_ref (void*)))
  (struct-stat-st_atime_usec-ref	(unsigned-long nausicaa_posix_stat_st_atime_usec_ref (void*)))
  (struct-stat-st_mtime-ref		(time_t nausicaa_posix_stat_st_mtime_ref (void*)))
  (struct-stat-st_mtime_usec-ref	(unsigned-long nausicaa_posix_stat_st_mtime_usec_ref (void*)))
  (struct-stat-st_ctime-ref		(time_t nausicaa_posix_stat_st_ctime_ref (void*)))
  (struct-stat-st_ctime_usec-ref	(unsigned-long nausicaa_posix_stat_st_ctime_usec_ref (void*)))
  (struct-stat-st_blocks-ref		(blkcnt_t nausicaa_posix_stat_st_blocks_ref (void*)))
  (struct-stat-st_blksize-ref		(unsigned nausicaa_posix_stat_st_blksize_ref (void*))))

(define-c-functions/with-errno libnausicaa-posix
  (stat			(int nausicaa_posix_stat (char* pointer)))
  (fstat		(int nausicaa_posix_fstat (int pointer)))
  (lstat		(int nausicaa_posix_lstat (char* pointer))))

(define-c-functions libnausicaa-posix
  (S_ISDIR		(int nausicaa_posix_stat_is_dir (mode_t)))
  (S_ISCHR		(int nausicaa_posix_stat_is_chr (mode_t)))
  (S_ISBLK		(int nausicaa_posix_stat_is_blk (mode_t)))
  (S_ISREG		(int nausicaa_posix_stat_is_reg (mode_t)))
  (S_ISFIFO		(int nausicaa_posix_stat_is_fifo (mode_t)))
  (S_ISLNK		(int nausicaa_posix_stat_is_lnk (mode_t)))
  (S_ISSOCK		(int nausicaa_posix_stat_is_sock (mode_t)))
  (S_TYPEISMQ		(int nausicaa_posix_stat_typeismq (pointer)))
  (S_TYPEISSEM		(int nausicaa_posix_stat_typeissem(pointer)))
  (S_TYPEISSHM		(int nausicaa_posix_stat_typeisshm (pointer))))


;;;; done

)

;;; end of file
