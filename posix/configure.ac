dnl
dnl Part of: Nausicaa/POSIX
dnl Contents: configuration file template
dnl Date: Fri Dec  5, 2008
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
dnl
dnl This  program  is free  software:  you  can redistribute  it
dnl and/or modify it under the terms of the GNU General Public
dnl License as published by the Free Software Foundation, either
dnl version  3 of  the License,  or (at  your option)  any later
dnl version.
dnl
dnl This  program is  distributed in  the hope  that it  will be
dnl useful, but  WITHOUT ANY WARRANTY; without  even the implied
dnl warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
dnl PURPOSE.   See  the  GNU  General Public  License  for  more
dnl details.
dnl
dnl You should  have received a  copy of the GNU  General Public
dnl License   along   with    this   program.    If   not,   see
dnl <http://www.gnu.org/licenses/>.
dnl

dnl page
dnl --------------------------------------------------------------------
dnl Setup.
dnl --------------------------------------------------------------------

AC_INIT([nausicaa-posix],[0.1d0],[marco.maggi-ipsu@poste.it])
DS_BEGIN([./src/libraries/foreign/posix/fd.sls])
dnl AC_CANONICAL_TARGET
dnl AC_USE_SYSTEM_EXTENSIONS
AC_SUBST([NAUSICAA_OS],[$target_os])
NAUSICAA_BEGIN
DS_EXCLUDE_SECTION([CONFIG_INSPECTION_SCRIPT])
DS_EXCLUDE_SECTION([AUTOCONF])
DS_EXCLUDE_SECTION([PKGCONFIG])

NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_FOREIGN],[(foreign ffi)])

dnl NAUSICAA_C_LANGUAGE
DS_C99_LANGUAGE_COMMON_SETUP
NAUSICAA_SIZEOF
NAUSICAA_C_TYPES_LIMITS

DS_C_LANGUAGE_LIBRARY([posix],1,0,[nausicaa-posix])

dnl These work!!!
AC_CHECK_FUNC([stat])
AC_CHECK_FUNC([lstat])
AC_CHECK_FUNC([fstat])

AC_CHECK_HEADERS([dirent.h])
AC_CHECK_HEADERS([fcntl.h])
AC_CHECK_HEADERS([ftw.h])
AC_CHECK_HEADERS([sys/stat.h])
AC_CHECK_HEADERS([sys/time.h])
AC_CHECK_HEADERS([sys/times.h])
AC_CHECK_HEADERS([sys/timex.h])
AC_CHECK_HEADERS([sys/wait.h])
AC_CHECK_HEADERS([time.h])
AC_CHECK_HEADERS([unistd.h])
AC_CHECK_HEADERS([utime.h])

dnl page
dnl --------------------------------------------------------------------
dnl Helpers.
dnl --------------------------------------------------------------------

m4_define([NAU_POSIX_INCLUDES],[
#ifdef HAVE_DIRENT_H
#  include <dirent.h>
#endif
#ifdef HAVE_FCNTL_H
#  include <fcntl.h>
#endif
#ifdef HAVE_FTW_H
#  include <ftw.h>
#endif
#ifdef HAVE_TIME_H
#  include <time.h>
#endif
#ifdef HAVE_UTIME_H
#  include <utime.h>
#endif
#ifdef HAVE_SYS_STAT_H
#  include <sys/stat.h>
#endif
#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_TIMES_H
#  include <sys/times.h>
#endif
#ifdef HAVE_SYS_TIMEX_H
#  include <sys/timex.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#endif
])

dnl --------------------------------------------------------------------

AC_DEFUN([NAU_POSIX_INSPECT_TYPE],
  [NAUSICAA_INSPECT_TYPE([$1],[$2],[$3],[#f],[NAU_POSIX_INCLUDES])])

AC_DEFUN([NAU_POSIX_INSPECT_STRUCT_TYPE],
  [NAUSICAA_INSPECT_STRUCT_TYPE([$1],[$2],[#f],[NAU_POSIX_INCLUDES])])

AC_DEFUN([NAU_POSIX_INSPECT_FIELD_TYPE],
  [NAUSICAA_INSPECT_FIELD_TYPE([$1],[$2],[$3],[$4],[#f],[NAU_POSIX_INCLUDES])])

AC_DEFUN([NAU_POSIX_INSPECT_FIELD_TYPE_POINTER],
  [NAUSICAA_INSPECT_FIELD_TYPE_POINTER([$1],[$2],[$3],[NAU_POSIX_INCLUDES])])

AC_DEFUN([NAU_POSIX_ENUM_VALUE],
  [NAU_POSIX_VALUEOF_TEST([$1],[$1])])

AC_DEFUN([NAU_POSIX_DEFINE_VALUE],
  [NAUSICAA_VALUEOF_TEST([$1],[$1],[0],[NAU_POSIX_INCLUDES])])

AC_DEFUN([NAU_POSIX_VALUEOF_TEST],
   [NAUSICAA_VALUEOF_TEST([$1],[$1],[#f],[NAU_POSIX_INCLUDES])])

AC_DEFUN([NAU_POSIX_SIZEOF_TEST],
   [NAUSICAA_SIZEOF_TEST([$1],[$2],[#f],[NAU_POSIX_INCLUDES])])

dnl page
dnl --------------------------------------------------------------------
dnl Inspection: typedefs.
dnl --------------------------------------------------------------------

NAU_POSIX_INSPECT_TYPE([BLKCNT_T],[blkcnt_t],[unsigned-int])
NAU_POSIX_INSPECT_TYPE([CLOCK_T],[clock_t],[signed-int])
NAU_POSIX_INSPECT_TYPE([DEV_T],[dev_t],[signed-int])
NAU_POSIX_INSPECT_TYPE([GID_T],[gid_t],[signed-int])
NAU_POSIX_INSPECT_TYPE([INO_T],[ino_t],[unsigned-int])
NAU_POSIX_INSPECT_TYPE([MODE_T],[mode_t],[unsigned-int])
NAU_POSIX_INSPECT_TYPE([NLINK_T],[nlink_t],[unsigned-int])
NAU_POSIX_INSPECT_TYPE([OFF_T],[off_t],[unsigned-int])
NAU_POSIX_INSPECT_TYPE([PID_T],[pid_t],[signed-int])
NAU_POSIX_INSPECT_TYPE([TIME_T],[time_t],[signed-int])
NAU_POSIX_INSPECT_TYPE([UID_T],[uid_t],[signed-int])

dnl page
dnl --------------------------------------------------------------------
dnl Struct types inspection.
dnl --------------------------------------------------------------------

AC_DEFUN([NAU_POSIX_STRUCT_FLOCK_FIELD],
  [NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_FLOCK_$1],[struct flock],[$2],[$3])])

NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_FLOCK],[struct flock])
NAU_POSIX_STRUCT_FLOCK_FIELD([L_TYPE],[l_type],[signed-int])
NAU_POSIX_STRUCT_FLOCK_FIELD([L_WHENCE],[l_whence],[signed-int])
NAU_POSIX_STRUCT_FLOCK_FIELD([L_START],[l_start],[signed-int])
NAU_POSIX_STRUCT_FLOCK_FIELD([L_LEN],[l_len],[signed-int])
NAU_POSIX_STRUCT_FLOCK_FIELD([L_PID],[l_pid],[signed-int])

dnl --------------------------------------------------------------------

AC_DEFUN([NAU_POSIX_STRUCT_TIMEVAL_FIELD],
  [NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_TIMEVAL_$1],[struct timeval],[$2],[$3])])

NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_TIMEVAL],[struct timeval])
NAU_POSIX_STRUCT_TIMEVAL_FIELD([TV_SEC],[tv_sec],[signed-int])
NAU_POSIX_STRUCT_TIMEVAL_FIELD([TV_USEC],[tv_usec],[signed-int])

dnl --------------------------------------------------------------------

AC_DEFUN([NAU_POSIX_STRUCT_TIMESPEC_FIELD],
  [NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_TIMESPEC_$1],[struct timespec],[$2],[$3])])

NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_TIMESPEC],[struct timespec])
NAU_POSIX_STRUCT_TIMESPEC_FIELD([TV_SEC],[tv_sec],[signed-int])
NAU_POSIX_STRUCT_TIMESPEC_FIELD([TV_NSEC],[tv_nsec],[signed-int])

dnl --------------------------------------------------------------------

NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_TMS],[struct tms])

dnl --------------------------------------------------------------------

AC_DEFUN([NAU_POSIX_STRUCT_DIRENT_FIELD],
  [NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_DIRENT_$1],[struct dirent],[$2],[$3])])

NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_DIRENT],[struct dirent])
NAU_POSIX_STRUCT_DIRENT_FIELD([D_INO],[d_ino],[signed-int])
NAU_POSIX_STRUCT_DIRENT_FIELD([D_OFF],[d_off],[signed-int])
NAU_POSIX_STRUCT_DIRENT_FIELD([D_RECLEN],[d_reclen],[unsigned-int])
NAU_POSIX_STRUCT_DIRENT_FIELD([D_TYPE],[d_type],[unsigned-int])

NAU_POSIX_INSPECT_FIELD_TYPE_POINTER([STRUCT_DIRENT_D_NAME],[struct dirent],[d_name])

dnl --------------------------------------------------------------------
dnl The stat structure is "special", so the following tests do not work.
dnl We  relay  on  the  stub  library  to  access  it.   Note  that  the
dnl following fields are not present on all the platforms:
dnl
dnl     time_usec st_dev st_mtime_usec st_atime_usec st_ctime_usec
dnl

AC_CHECK_MEMBERS([struct stat.st_atime_usec])
AC_CHECK_MEMBERS([struct stat.st_mtime_usec])
AC_CHECK_MEMBERS([struct stat.st_ctime_usec])

dnl NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_STAT],[struct stat])

dnl AC_DEFUN([NAU_POSIX_STRUCT_STAT_FIELD],
dnl   [NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_STAT_$1],[struct stat],[$2],[$3])])

dnl NAU_POSIX_STRUCT_STAT_FIELD([ST_MODE],[st_mode],[unsigned-int])
dnl NAU_POSIX_STRUCT_STAT_FIELD([ST_INO],[st_ino],[unsigned-int])
dnl NAU_POSIX_STRUCT_STAT_FIELD([ST_DEV],[st_dev],[unsigned-int])
dnl NAU_POSIX_STRUCT_STAT_FIELD([ST_NLINK],[st_nlink],[unsigned-int])
dnl NAU_POSIX_STRUCT_STAT_FIELD([ST_UID],[st_uid],[signed-int])
dnl NAU_POSIX_STRUCT_STAT_FIELD([ST_GID],[st_gid],[signed-int])
dnl NAU_POSIX_STRUCT_STAT_FIELD([ST_SIZE],[st_size],[unsigned-int])
dnl NAU_POSIX_STRUCT_STAT_FIELD([ST_ATIME],[st_atime],[signed-int])
dnl NAU_POSIX_STRUCT_STAT_FIELD([ST_ATIME_USEC],[st_atime_usec],[unsigned-int])
dnl NAU_POSIX_STRUCT_STAT_FIELD([ST_MTIME],[st_mtime],[signed-int])
dnl NAU_POSIX_STRUCT_STAT_FIELD([ST_MTIME_USEC],[st_mtime_usec],[unsigned-int])
dnl NAU_POSIX_STRUCT_STAT_FIELD([ST_CTIME],[st_ctime],[signed-int])
dnl NAU_POSIX_STRUCT_STAT_FIELD([ST_CTIME_USEC],[st_ctime_usec],[unsigned-int])
dnl NAU_POSIX_STRUCT_STAT_FIELD([ST_BLOCKS],[st_blocks],[unsigned-int])
dnl NAU_POSIX_STRUCT_STAT_FIELD([ST_BLKSIZE],[st_blksize],[unsigned-int])

dnl --------------------------------------------------------------------

AC_DEFUN([NAU_POSIX_STRUCT_UTIMBUF_FIELD],
  [NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_UTIMBUF_$1],[struct utimbuf],[$2],[$3])])

NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_UTIMBUF],[struct utimbuf])
NAU_POSIX_STRUCT_UTIMBUF_FIELD([ACTIME],[actime],[signed-int])
NAU_POSIX_STRUCT_UTIMBUF_FIELD([MODTIME],[modtime],[signed-int])

dnl --------------------------------------------------------------------

AC_DEFUN([NAU_POSIX_STRUCT_TIMEZONE_FIELD],
  [NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_TIMEZONE_$1],[struct timezone],[$2],[$3])])

NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_TIMEZONE],[struct timezone])
NAU_POSIX_STRUCT_TIMEZONE_FIELD([TZ_MINUTESWEST],[tz_minuteswest],[signed-int])
NAU_POSIX_STRUCT_TIMEZONE_FIELD([TZ_DSTTIME],[tz_dsttime],[signed-int])

dnl --------------------------------------------------------------------

AC_DEFUN([NAU_POSIX_STRUCT_TM_FIELD],
  [NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_TM_$1],[struct tm],[$2],[$3])])

NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_TM],[struct tm])
NAU_POSIX_STRUCT_TM_FIELD([TM_SEC],[tm_sec],[signed-int])
NAU_POSIX_STRUCT_TM_FIELD([TM_MIN],[tm_min],[signed-int])
NAU_POSIX_STRUCT_TM_FIELD([TM_HOUR],[tm_hour],[signed-int])
NAU_POSIX_STRUCT_TM_FIELD([TM_MDAY],[tm_mday],[signed-int])
NAU_POSIX_STRUCT_TM_FIELD([TM_MON],[tm_mon],[signed-int])
NAU_POSIX_STRUCT_TM_FIELD([TM_YEAR],[tm_year],[signed-int])
NAU_POSIX_STRUCT_TM_FIELD([TM_WDAY],[tm_wday],[signed-int])
NAU_POSIX_STRUCT_TM_FIELD([TM_YDAY],[tm_yday],[signed-int])
NAU_POSIX_STRUCT_TM_FIELD([TM_ISDST],[tm_isdst],[signed-int])
NAU_POSIX_STRUCT_TM_FIELD([TM_GMTOFF],[tm_gmtoff],[signed-int])
NAU_POSIX_STRUCT_TM_FIELD([TM_ZONE],[tm_zone],[pointer])

dnl --------------------------------------------------------------------

AC_DEFUN([NAU_POSIX_STRUCT_NTPTIMEVAL_FIELD],
  [NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_NTPTIMEVAL_$1],[struct ntptimeval],[$2],[$3])])

AC_DEFUN([NAU_POSIX_STRUCT_NTPTIMEVAL_POINTER_FIELD],
  [NAU_POSIX_INSPECT_FIELD_TYPE_POINTER([STRUCT_NTPTIMEVAL_$1],[struct ntptimeval],[$2])])

NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_NTPTIMEVAL],[struct ntptimeval])
NAU_POSIX_STRUCT_NTPTIMEVAL_POINTER_FIELD([TIME],[time])
NAU_POSIX_STRUCT_NTPTIMEVAL_FIELD([MAXERROR],[maxerror],[signed-int])
NAU_POSIX_STRUCT_NTPTIMEVAL_FIELD([ESTERROR],[esterror],[signed-int])

dnl --------------------------------------------------------------------

AC_DEFUN([NAU_POSIX_STRUCT_FTW_FIELD],
  [NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_FTW_$1],[struct FTW],[$2],[$3])])

NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_FTW],[struct FTW])
NAU_POSIX_STRUCT_FTW_FIELD([BASE],[base],[signed-int])
NAU_POSIX_STRUCT_FTW_FIELD([LEVEL],[level],[signed-int])

dnl --------------------------------------------------------------------

AC_CACHE_SAVE

dnl page
dnl --------------------------------------------------------------------
dnl Constants.
dnl --------------------------------------------------------------------

NAU_POSIX_DEFINE_VALUE([SEEK_SET])
NAU_POSIX_DEFINE_VALUE([SEEK_CUR])
NAU_POSIX_DEFINE_VALUE([SEEK_END])

dnl In alphabetical order.
NAU_POSIX_DEFINE_VALUE([O_ACCMODE])
NAU_POSIX_DEFINE_VALUE([O_APPEND])
NAU_POSIX_DEFINE_VALUE([O_ASYNC])
NAU_POSIX_DEFINE_VALUE([O_CREAT])
NAU_POSIX_DEFINE_VALUE([O_EXCL])
NAU_POSIX_DEFINE_VALUE([O_EXEC])
NAU_POSIX_DEFINE_VALUE([O_EXLOCK])
NAU_POSIX_DEFINE_VALUE([O_FSYNC])
NAU_POSIX_DEFINE_VALUE([O_IGNORE_CTTY])
NAU_POSIX_DEFINE_VALUE([O_NDELAY])
NAU_POSIX_DEFINE_VALUE([O_NOCTTY])
NAU_POSIX_DEFINE_VALUE([O_NOLINK])
NAU_POSIX_DEFINE_VALUE([O_NONBLOCK])
NAU_POSIX_DEFINE_VALUE([O_NOTRANS])
NAU_POSIX_DEFINE_VALUE([O_RDONLY])
NAU_POSIX_DEFINE_VALUE([O_RDWR])
NAU_POSIX_DEFINE_VALUE([O_READ])
NAU_POSIX_DEFINE_VALUE([O_SHLOCK])
NAU_POSIX_DEFINE_VALUE([O_SYNC])
NAU_POSIX_DEFINE_VALUE([O_TRUNC])
NAU_POSIX_DEFINE_VALUE([O_WRITE])
NAU_POSIX_DEFINE_VALUE([O_WRONLY])

dnl This is GNU specific.
NAU_POSIX_DEFINE_VALUE([O_NOATIME])

NAU_POSIX_DEFINE_VALUE([FD_CLOEXEC])

AC_CACHE_SAVE

dnl In alphabetical order.
NAU_POSIX_DEFINE_VALUE([F_DUPFD])
NAU_POSIX_DEFINE_VALUE([F_GETFD])
NAU_POSIX_DEFINE_VALUE([F_GETFL])
NAU_POSIX_DEFINE_VALUE([F_GETLK])
NAU_POSIX_DEFINE_VALUE([F_GETOWN])
NAU_POSIX_DEFINE_VALUE([F_SETFD])
NAU_POSIX_DEFINE_VALUE([F_SETFL])
NAU_POSIX_DEFINE_VALUE([F_SETLKW])
NAU_POSIX_DEFINE_VALUE([F_SETLK])
NAU_POSIX_DEFINE_VALUE([F_SETOWN])

dnl In alphabetical order.
NAU_POSIX_DEFINE_VALUE([F_RDLCK])
NAU_POSIX_DEFINE_VALUE([F_UNLCK])
NAU_POSIX_DEFINE_VALUE([F_WRLCK])

NAU_POSIX_DEFINE_VALUE([WNOHANG])
NAU_POSIX_DEFINE_VALUE([WUNTRACED])
NAU_POSIX_DEFINE_VALUE([WCONTINUED])

NAU_POSIX_DEFINE_VALUE([R_OK])
NAU_POSIX_DEFINE_VALUE([W_OK])
NAU_POSIX_DEFINE_VALUE([X_OK])
NAU_POSIX_DEFINE_VALUE([F_OK])

NAU_POSIX_DEFINE_VALUE([L_ctermid])
NAU_POSIX_DEFINE_VALUE([L_tmpnam])

NAU_POSIX_DEFINE_VALUE([CLOCKS_PER_SEC])

dnl Mode bits.
NAU_POSIX_DEFINE_VALUE([S_IRUSR])
NAU_POSIX_DEFINE_VALUE([S_IWUSR])
NAU_POSIX_DEFINE_VALUE([S_IXUSR])

NAU_POSIX_DEFINE_VALUE([S_IRGRP])
NAU_POSIX_DEFINE_VALUE([S_IWGRP])
NAU_POSIX_DEFINE_VALUE([S_IXGRP])

NAU_POSIX_DEFINE_VALUE([S_IROTH])
NAU_POSIX_DEFINE_VALUE([S_IWOTH])
NAU_POSIX_DEFINE_VALUE([S_IXOTH])

NAU_POSIX_DEFINE_VALUE([S_IRWXU])
NAU_POSIX_DEFINE_VALUE([S_IRWXG])
NAU_POSIX_DEFINE_VALUE([S_IRWXO])

NAU_POSIX_DEFINE_VALUE([S_ISUID])
NAU_POSIX_DEFINE_VALUE([S_ISGID])
NAU_POSIX_DEFINE_VALUE([S_ISVTX])

dnl "struct dirent" related stuff

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

NAU_POSIX_DEFINE_VALUE([DT_BLK])
NAU_POSIX_DEFINE_VALUE([DT_CHR])
NAU_POSIX_DEFINE_VALUE([DT_DIR])
NAU_POSIX_DEFINE_VALUE([DT_FIFO])
NAU_POSIX_DEFINE_VALUE([DT_LNK])
NAU_POSIX_DEFINE_VALUE([DT_REG])
NAU_POSIX_DEFINE_VALUE([DT_SOCK])
NAU_POSIX_DEFINE_VALUE([DT_UNKNOWN])

dnl constants related to "pathconf()"
NAU_POSIX_DEFINE_VALUE([_PC_LINK_MAX])
NAU_POSIX_DEFINE_VALUE([_PC_MAX_CANON])
NAU_POSIX_DEFINE_VALUE([_PC_MAX_INPUT])
NAU_POSIX_DEFINE_VALUE([_PC_NAME_MAX])
NAU_POSIX_DEFINE_VALUE([_PC_PATH_MAX])
NAU_POSIX_DEFINE_VALUE([_PC_PIPE_BUF])
NAU_POSIX_DEFINE_VALUE([_PC_CHOWN_RESTRICTED])
NAU_POSIX_DEFINE_VALUE([_PC_NO_TRUNC])
NAU_POSIX_DEFINE_VALUE([_PC_VDISABLE])
NAU_POSIX_DEFINE_VALUE([_PC_SYNC_IO])
NAU_POSIX_DEFINE_VALUE([_PC_ASYNC_IO])
NAU_POSIX_DEFINE_VALUE([_PC_PRIO_IO])
NAU_POSIX_DEFINE_VALUE([_PC_FILESIZEBITS])
NAU_POSIX_DEFINE_VALUE([_PC_REC_INCR_XFER_SIZE])
NAU_POSIX_DEFINE_VALUE([_PC_REC_MAX_XFER_SIZE])
NAU_POSIX_DEFINE_VALUE([_PC_REC_MIN_XFER_SIZE])
NAU_POSIX_DEFINE_VALUE([_PC_XFER_ALIGN])

dnl constants related to "ftw()" and "nftw()"
NAU_POSIX_DEFINE_VALUE([FTW_F])
NAU_POSIX_DEFINE_VALUE([FTW_D])
NAU_POSIX_DEFINE_VALUE([FTW_NS])
NAU_POSIX_DEFINE_VALUE([FTW_DNR])
NAU_POSIX_DEFINE_VALUE([FTW_SL])
NAU_POSIX_DEFINE_VALUE([FTW_DP])
NAU_POSIX_DEFINE_VALUE([FTW_SLN])
NAU_POSIX_DEFINE_VALUE([FTW_PHYS])
NAU_POSIX_DEFINE_VALUE([FTW_MOUNT])
NAU_POSIX_DEFINE_VALUE([FTW_CHDIR])
NAU_POSIX_DEFINE_VALUE([FTW_DEPTH])
NAU_POSIX_DEFINE_VALUE([FTW_ACTIONRETVAL])
NAU_POSIX_DEFINE_VALUE([FTW_CONTINUE])
NAU_POSIX_DEFINE_VALUE([FTW_STOP])
NAU_POSIX_DEFINE_VALUE([FTW_SKIP_SUBTREE])
NAU_POSIX_DEFINE_VALUE([FTW_SKIP_SIBLINGS])

dnl Misc
NAU_POSIX_DEFINE_VALUE([EOF])
NAU_POSIX_DEFINE_VALUE([MAXSYMLINKS])
NAU_POSIX_DEFINE_VALUE([NAME_MAX])
NAU_POSIX_DEFINE_VALUE([PATH_MAX])
NAU_POSIX_DEFINE_VALUE([MAXPATHLEN])

AC_CACHE_SAVE

dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../src/libraries/foreign/posix/sizeof.sls:../src/libraries/foreign/posix/sizeof.sls.in])

DS_CONFIG_FILE_SLACKWARE_BIN_DESC
DS_CONFIG_FILE_SLACKWARE_DOC_DESC
NAUSICAA_END
DS_END

### end of file
