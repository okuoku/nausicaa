;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: low-level interface to the stat functions
;;;Date: Wed Nov  4, 2009
;;;
;;;Abstract
;;;
;;;	This is an interface  to "stat()", "fstat()" and "lstat()" which
;;;	makes use of the  stubs functions in "libnausicaa-posix.so" from
;;;	the Nausicaa/Stubs project.
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


(library (posix stat platform)
  (export
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
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (posix shared-object)
    (posix sizeof))


;;;FIXME For some reason it looks like the size of "struct stat" is not
;;;determined correctly by the Autoconf macros.  Dunno why.  This error
;;;causes segmentation faults in the %REAL-STAT function.
;;;
;;;The fix below solves the  problem.
;;;
;;;Note that on my i686-pc-linux-gnu  the size reported by the Autoconf
;;;macro is 88, while the value returned by the foreign function is 96.
;;;
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
