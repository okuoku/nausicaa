;;;!mosh
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: foreign library inspection generator
;;;Date: Sun Dec  6, 2009
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


(import (nausicaa)
  (foreign ffi inspector-maker))


;;; --------------------------------------------------------------------
;;; Inspection: typedefs.
;;; --------------------------------------------------------------------

(define-c-type blkcnt_t		unsigned-int)
(define-c-type clock_t		signed-int)
(define-c-type dev_t		signed-int)
(define-c-type gid_t		signed-int)
(define-c-type ino_t		unsigned-int)
(define-c-type mode_t		unsigned-int)
(define-c-type nlink_t		unsigned-int)
(define-c-type off_t		unsigned-int)
(define-c-type pid_t		signed-int)
(define-c-type time_t		signed-int)
(define-c-type uid_t		signed-int)

(define-c-type wchar_t		signed-int)


;;; --------------------------------------------------------------------
;;; Struct types inspection.
;;; --------------------------------------------------------------------

(define-c-struct flock
  "struct flock"
  (signed-int		l_type)
  (signed-int		l_whence)
  (signed-int		l_start)
  (signed-int		l_len)
  (signed-int		l_pid))

(define-c-struct timeval
  "struct timeval"
  (signed-int		tv_sec)
  (signed-int		tv_usec))

(define-c-struct timespec
  "struct timespec"
  (signed-int		tv_sec)
  (signed-int		tv_nsec))

(define-c-struct tms
  "struct tms")

(define-c-struct dirent
  "struct dirent"
  (signed-int		d_ino)
  (signed-int		d_off)
  (unsigned-int		d_reclen)
  (unsigned-int		d_type)
  (embedded		d_name))

;;The stat structure  is "special", so the following  tests do not work.
;;We relay  on the stub library  to access it.  Note  that the following
;;fields are not present on all the platforms:
;;
;;     time_usec st_dev st_mtime_usec st_atime_usec st_ctime_usec
;;
;;

;;; NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_STAT],[struct stat])

;;; AC_DEFUN([NAU_POSIX_STRUCT_STAT_FIELD],
;;;   [NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_STAT_$1],[struct stat],[$2],[$3])])

;;; NAU_POSIX_STRUCT_STAT_FIELD([ST_MODE],[st_mode],[unsigned-int])
;;; NAU_POSIX_STRUCT_STAT_FIELD([ST_INO],[st_ino],[unsigned-int])
;;; NAU_POSIX_STRUCT_STAT_FIELD([ST_DEV],[st_dev],[unsigned-int])
;;; NAU_POSIX_STRUCT_STAT_FIELD([ST_NLINK],[st_nlink],[unsigned-int])
;;; NAU_POSIX_STRUCT_STAT_FIELD([ST_UID],[st_uid],[signed-int])
;;; NAU_POSIX_STRUCT_STAT_FIELD([ST_GID],[st_gid],[signed-int])
;;; NAU_POSIX_STRUCT_STAT_FIELD([ST_SIZE],[st_size],[unsigned-int])
;;; NAU_POSIX_STRUCT_STAT_FIELD([ST_ATIME],[st_atime],[signed-int])
;;; NAU_POSIX_STRUCT_STAT_FIELD([ST_ATIME_USEC],[st_atime_usec],[unsigned-int])
;;; NAU_POSIX_STRUCT_STAT_FIELD([ST_MTIME],[st_mtime],[signed-int])
;;; NAU_POSIX_STRUCT_STAT_FIELD([ST_MTIME_USEC],[st_mtime_usec],[unsigned-int])
;;; NAU_POSIX_STRUCT_STAT_FIELD([ST_CTIME],[st_ctime],[signed-int])
;;; NAU_POSIX_STRUCT_STAT_FIELD([ST_CTIME_USEC],[st_ctime_usec],[unsigned-int])
;;; NAU_POSIX_STRUCT_STAT_FIELD([ST_BLOCKS],[st_blocks],[unsigned-int])
;;; NAU_POSIX_STRUCT_STAT_FIELD([ST_BLKSIZE],[st_blksize],[unsigned-int])

;;We can still check for the existence of some fields.
(autoconf-lib "
AC_CHECK_MEMBERS([struct stat.st_atime_usec])
AC_CHECK_MEMBERS([struct stat.st_mtime_usec])
AC_CHECK_MEMBERS([struct stat.st_ctime_usec])
")

(define-c-struct utimbuf
  "struct utimbuf"
  (signed-int		actime)
  (signed-int		modtime))

(define-c-struct timezone
  "struct timezone"
  (signed-int		tz_minuteswest)
  (signed-int		tz_dsttime))

(define-c-struct tm
  "struct tm"
  (signed-int		tm_sec)
  (signed-int		tm_min)
  (signed-int		tm_hour)
  (signed-int		tm_mday)
  (signed-int		tm_mon)
  (signed-int		tm_year)
  (signed-int		tm_wday)
  (signed-int		tm_yday)
  (signed-int		tm_isdst)
  (signed-int		tm_gmtoff)
  (pointer		tm_zone))

(define-c-struct ntptimeval
  "struct ntptimeval"
  (embedded		time)
  (signed-int		maxerror)
  (signed-int		esterror))

(define-c-struct timex
  "struct timex"
  (unsigned-int		modes)
  (signed-int		offset)
  (signed-int		freq)
  (signed-int		maxerror)
  (signed-int		esterror)
  (signed-int		status)
  (signed-int		constant)
  (signed-int		precision)
  (signed-int		tolerance)
  (embedded		time)
  (signed-int		tick)
  (signed-int		ppsfreq)
  (signed-int		jitter)
  (signed-int		shift)
  (signed-int		stabil)
  (signed-int		jitcnt)
  (signed-int		calcnt)
  (signed-int		errcnt)
  (signed-int		stbcnt))

(define-c-struct itimerval
  "struct itimerval"
  (embedded		it_interval)
  (embedded		it_value))

(define-c-struct FTW
  "struct FTW"
  (signed-int		base)
  (signed-int		level))

(define-c-struct iovec
  "struct iovec"
  (pointer		iov_base)
  (unsigned-int		iov_len))

(sizeof-lib
 (define (sizeof-iovec-array number-of-elements)
   (* strideof-iovec number-of-elements))
 (define (array-ref-c-iovec pointer index)
   (pointer-add pointer (* index strideof-iovec)))
 )

(sizeof-lib-exports
 sizeof-iovec-array
 array-ref-c-iovec)

(autoconf-lib "AC_CACHE_SAVE")


;;; --------------------------------------------------------------------
;;; Constants.
;;; --------------------------------------------------------------------

(define-c-defines "seek whence arguments"
  SEEK_SET
  SEEK_CUR
  SEEK_END)

(define-c-defines "file descriptor related flags"
  O_ACCMODE
  O_APPEND
  O_ASYNC
  O_CREAT
  O_EXCL
  O_EXEC
  O_EXLOCK
  O_FSYNC
  O_IGNORE_CTTY
  O_NDELAY
  O_NOCTTY
  O_NOLINK
  O_NONBLOCK
  O_NOTRANS
  O_RDONLY
  O_RDWR
  O_READ
  O_SHLOCK
  O_SYNC
  O_TRUNC
  O_WRITE
  O_WRONLY

;;; This is GNU specific.
  O_NOATIME

  FD_CLOEXEC)

(autoconf-lib "AC_CACHE_SAVE")

(define-c-defines "ioctl action selection"
  F_DUPFD
  F_GETFD
  F_GETFL
  F_GETLK
  F_GETOWN
  F_SETFD
  F_SETFL
  F_SETLKW
  F_SETLK
  F_SETOWN

  F_RDLCK
  F_UNLCK
  F_WRLCK)

(define-c-defines "miscellaneous file-related constants"
  WNOHANG
  WUNTRACED
  WCONTINUED

  R_OK
  W_OK
  X_OK
  F_OK)

(define-c-defines "miscellaneous constants"
  L_ctermid
  L_tmpnam

  CLOCKS_PER_SEC)

(define-c-defines "mode bits"
  S_IRUSR
  S_IWUSR
  S_IXUSR

  S_IRGRP
  S_IWGRP
  S_IXGRP

  S_IROTH
  S_IWOTH
  S_IXOTH

  S_IRWXU
  S_IRWXG
  S_IRWXO

  S_ISUID
  S_ISGID
  S_ISVTX)

;;; "struct dirent" related stuff

(autoconf-lib "
AC_CHECK_DECL([_DIRENT_HAVE_D_NAMELEN],[NAU_DIRENT_HAVE_D_NAMELEN=#t],[NAU_DIRENT_HAVE_D_NAMELEN=#f],
  [NAU_POSIX_INCLUDES])
AC_CHECK_DECL([_DIRENT_HAVE_D_RECLEN],[NAU_DIRENT_HAVE_D_RECLEN=#t],[NAU_DIRENT_HAVE_D_RECLEN=#f],
  [NAU_POSIX_INCLUDES])
AC_CHECK_DECL([_DIRENT_HAVE_D_OFF],[NAU_DIRENT_HAVE_D_OFF=#t],[NAU_DIRENT_HAVE_D_OFF=#f],
  [NAU_POSIX_INCLUDES])
AC_CHECK_DECL([_DIRENT_HAVE_D_TYPE],[NAU_DIRENT_HAVE_D_TYPE=#t],[NAU_DIRENT_HAVE_D_TYPE=#f],
  [NAU_POSIX_INCLUDES])
AC_SUBST([NAU_DIRENT_HAVE_D_NAMELEN])
AC_SUBST([NAU_DIRENT_HAVE_D_RECLEN])
AC_SUBST([NAU_DIRENT_HAVE_D_OFF])
AC_SUBST([NAU_DIRENT_HAVE_D_TYPE])
")

(sizeof-lib
 (define _DIRENT_HAVE_D_NAMELEN	^NAU_DIRENT_HAVE_D_NAMELEN^)
 (define _DIRENT_HAVE_D_RECLEN	^NAU_DIRENT_HAVE_D_RECLEN^)
 (define _DIRENT_HAVE_D_OFF	^NAU_DIRENT_HAVE_D_OFF^)
 (define _DIRENT_HAVE_D_TYPE	^NAU_DIRENT_HAVE_D_TYPE^))

(sizeof-lib-exports
 _DIRENT_HAVE_D_NAMELEN		_DIRENT_HAVE_D_RECLEN
 _DIRENT_HAVE_D_OFF		_DIRENT_HAVE_D_TYPE)

(define-c-defines "dirent stuff"
  DT_BLK
  DT_CHR
  DT_DIR
  DT_FIFO
  DT_LNK
  DT_REG
  DT_SOCK
  DT_UNKNOWN)

(define-c-defines "constants related to pathconf()"
  _PC_LINK_MAX
  _PC_MAX_CANON
  _PC_MAX_INPUT
  _PC_NAME_MAX
  _PC_PATH_MAX
  _PC_PIPE_BUF
  _PC_CHOWN_RESTRICTED
  _PC_NO_TRUNC
  _PC_VDISABLE
  _PC_SYNC_IO
  _PC_ASYNC_IO
  _PC_PRIO_IO
  _PC_FILESIZEBITS
  _PC_REC_INCR_XFER_SIZE
  _PC_REC_MAX_XFER_SIZE
  _PC_REC_MIN_XFER_SIZE
  _PC_XFER_ALIGN)

(define-c-defines "constants related to ftw() and nftw()"
  FTW_F
  FTW_D
  FTW_NS
  FTW_DNR
  FTW_SL
  FTW_DP
  FTW_SLN
  FTW_PHYS
  FTW_MOUNT
  FTW_CHDIR
  FTW_DEPTH
  FTW_ACTIONRETVAL
  FTW_CONTINUE
  FTW_STOP
  FTW_SKIP_SUBTREE
  FTW_SKIP_SIBLINGS)

(define-c-defines "setitimer() stuff"
  ITIMER_REAL
  ITIMER_VIRTUAL
  ITIMER_PROF)

(define-c-defines "miscellaneous"
  EOF
  MAXSYMLINKS
  NAME_MAX
  PATH_MAX
  MAXPATHLEN)

(autoconf-lib "AC_CACHE_SAVE")


;;;; done

(define posix-library-spec
  '(foreign posix sizeof))

(autoconf-lib-write "configuration/posix-inspector.m4" posix-library-spec)
(sizeof-lib-write   "src/libraries/foreign/posix/sizeof.sls.in" posix-library-spec)

;;; end of file
