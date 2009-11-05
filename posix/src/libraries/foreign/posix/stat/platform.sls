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
;;;	  On systems  using the GNU C  Library it appears  that the stat
;;;	functions are exported with names like:
;;;
;;;		statfs		__statfs
;;;		fstatfs		__fstatfs
;;;		statfs64	__statfs64
;;;		fstatfs64	__fstatfs64
;;;				__statfs_symlinks
;;;
;;;	but I  dunno if they are  portable.  For this reason  we use the
;;;	stub library.  While  we are at it, we  exploit the stub library
;;;	to get further informations and functionalities.
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


(library (foreign posix stat platform)
  (export
    stat		lstat		fstat

    S_ISDIR		S_ISCHR		S_ISBLK
    S_ISREG		S_ISFIFO	S_ISLNK
    S_ISSOCK

    S_TYPEISMQ		S_TYPEISSEM	S_TYPEISSHM

    sizeof-struct-stat)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (except (foreign posix sizeof)
	    sizeof-struct-stat))


(define dummy
  (shared-object (open-shared-object* 'libnausicaa-posix1.so)))

;;;FIXME For some reason it looks  like the size of "struct stat" is not
;;;determined correctly by the  Autoconf macros.  Dunno why.  This error
;;;causes segmentation faults in the %REAL-STAT function.
;;;
;;;The fix below solves the  problem.
;;;
;;;Note that on  my i686-pc-linux-gnu the size reported  by the Autoconf
;;;macro is 88, while the value returned by the foreign function is 96.
(define-c-function %sizeof-stat
  (int nausicaa_posix_sizeof_stat (void)))

(define sizeof-struct-stat
  (%sizeof-stat))

;;; --------------------------------------------------------------------

(define-c-function/with-errno stat
  (int nausicaa_posix_stat (char* pointer)))

(define-c-function/with-errno fstat
  (int nausicaa_posix_fstat (int pointer)))

(define-c-function/with-errno lstat
  (int nausicaa_posix_lstat (char* pointer)))

;;; --------------------------------------------------------------------

(define-c-function S_ISDIR
  (int nausicaa_posix_stat_is_dir (mode_t)))

(define-c-function S_ISCHR
  (int nausicaa_posix_stat_is_chr (mode_t)))

(define-c-function S_ISBLK
  (int nausicaa_posix_stat_is_blk (mode_t)))

(define-c-function S_ISREG
  (int nausicaa_posix_stat_is_reg (mode_t)))

(define-c-function S_ISFIFO
  (int nausicaa_posix_stat_is_fifo (mode_t)))

(define-c-function S_ISLNK
  (int nausicaa_posix_stat_is_lnk (mode_t)))

(define-c-function S_ISSOCK
  (int nausicaa_posix_stat_is_sock (mode_t)))

;;; --------------------------------------------------------------------

(define-c-function S_TYPEISMQ
  (int nausicaa_posix_stat_typeismq (pointer)))

(define-c-function S_TYPEISSEM
  (int nausicaa_posix_stat_typeissem(pointer)))

(define-c-function S_TYPEISSHM
  (int nausicaa_posix_stat_typeisshm (pointer)))


;;;; done

)

;;; end of file
