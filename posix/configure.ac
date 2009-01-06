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

AC_INIT([nausicaa-posix],[0.1d0])
NAUSICAA_BEGIN
NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_R6RS],[(r6rs)])
NAUSICAA_SCHEME_CHECK_LIBRARY([URIEL_FFI],[(uriel ffi)])
NAUSICAA_SCHEME_CHECK_LIBRARY([URIEL_LANG],[(uriel lang)])
NAUSICAA_SYSTEM_SETUP
NAUSICAA_C_LANGUAGE
NAUSICAA_SIZEOF

dnl These work!!!
AC_CHECK_FUNC([stat])
AC_CHECK_FUNC([lstat])
AC_CHECK_FUNC([fstat])

dnl page
dnl --------------------------------------------------------------------
dnl Helpers.
dnl --------------------------------------------------------------------

m4_define([NAU_POSIX_INCLUDES],[
#include <fcntl.h>
#include <time.h>
#include <sys/times.h>
#include <sys/stat.h>
#include <dirent.h>
#include <utime.h>
])

AC_DEFUN([NAU_POSIX_VALUEOF_TEST],
   [NAUSICAA_VALUEOF_TEST([$1],[$1],[#f],[NAU_POSIX_INCLUDES])])

AC_DEFUN([NAU_POSIX_SIZEOF_TEST],
   [NAUSICAA_SIZEOF_TEST([$1],[$2],[#f],[NAU_POSIX_INCLUDES])])

AC_DEFUN([NAU_POSIX_INSPECT_TYPE],
  [NAUSICAA_INSPECT_TYPE([$1],[$2],[$3],[#f],[NAU_POSIX_INCLUDES])])

AC_DEFUN([NAU_POSIX_INSPECT_FIELD_TYPE],
  [NAUSICAA_INSPECT_FIELD_TYPE([$1],[$2],[$3],[$4],[#f],[NAU_POSIX_INCLUDES])])

AC_DEFUN([NAU_POSIX_INSPECT_FIELD_TYPE_POINTER],
  [NAUSICAA_INSPECT_FIELD_TYPE_POINTER([$1],[$2],[$3],[NAU_POSIX_INCLUDES])])

AC_DEFUN([NAU_POSIX_INSPECT_STRUCT_TYPE],
   [NAUSICAA_INSPECT_STRUCT_TYPE([$1],[$2],[#f],[NAU_POSIX_INCLUDES])])


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

NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_FLOCK],[struct flock])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_FLOCK_L_TYPE],[struct flock],[l_type],[signed-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_FLOCK_L_WHENCE],[struct flock],[l_whence],[signed-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_FLOCK_L_START],[struct flock],[l_start],[signed-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_FLOCK_L_LEN],[struct flock],[l_len],[signed-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_FLOCK_L_PID],[struct flock],[l_pid],[signed-int])

NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_TIMEVAL],[struct timeval])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_TIMEVAL_TV_SEC],[struct timeval],[tv_sec],[signed-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_TIMEVAL_TV_USEC],[struct timeval],[tv_usec],[signed-int])

NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_TIMESPEC],[struct timespec])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_TIMESPEC_TV_SEC],[struct timespec],[tv_sec],[signed-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_TIMESPEC_TV_NSEC],[struct timespec],[tv_nsec],[signed-int])

NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_DIRENT],[struct dirent])
NAU_POSIX_INSPECT_FIELD_TYPE_POINTER([STRUCT_DIRENT_D_NAME],[struct dirent],[d_name])

dnl Note that the "time_usec" and "st_dev" fields are not detected on my
dnl platform.
NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_STAT],[struct stat])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_STAT_ST_MODE],[struct stat],[st_mode],[unsigned-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_STAT_ST_INO],[struct stat],[st_ino],[unsigned-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_STAT_ST_DEV],[struct stat],[st_dev],[unsigned-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_STAT_ST_NLINK],[struct stat],[st_nlink],[unsigned-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_STAT_ST_UID],[struct stat],[st_uid],[signed-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_STAT_ST_GID],[struct stat],[st_gid],[signed-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_STAT_ST_SIZE],[struct stat],[st_size],[unsigned-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_STAT_ST_ATIME],     [struct stat],[st_atime],[signed-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_STAT_ST_ATIME_USEC],[struct stat],[st_atime_usec],[unsigned-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_STAT_ST_MTIME],     [struct stat],[st_mtime],[signed-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_STAT_ST_MTIME_USEC],[struct stat],[st_mtime_usec],[unsigned-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_STAT_ST_CTIME],     [struct stat],[st_ctime],[signed-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_STAT_ST_CTIME_USEC],[struct stat],[st_ctime_usec],[unsigned-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_STAT_ST_BLOCKS],[struct stat],[st_blocks],[unsigned-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_STAT_ST_BLKSIZE],[struct stat],[st_blksize],[unsigned-int])

NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_UTIMBUF],[struct utimbuf])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_UTIMBUF_ACTIME], [struct utimbuf],[actime],[signed-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_UTIMBUF_MODTIME],[struct utimbuf],[modtime],[signed-int])

AC_CACHE_SAVE

dnl page
dnl --------------------------------------------------------------------
dnl Inspection: file descriptors related constants.
dnl --------------------------------------------------------------------

NAU_POSIX_VALUEOF_TEST([SEEK_SET])
NAU_POSIX_VALUEOF_TEST([SEEK_CUR])
NAU_POSIX_VALUEOF_TEST([SEEK_END])

dnl In alphabetical order.
NAU_POSIX_VALUEOF_TEST([O_ACCMODE])
NAU_POSIX_VALUEOF_TEST([O_APPEND])
NAU_POSIX_VALUEOF_TEST([O_ASYNC])
NAU_POSIX_VALUEOF_TEST([O_CREAT])
NAU_POSIX_VALUEOF_TEST([O_EXCL])
NAU_POSIX_VALUEOF_TEST([O_EXEC])
NAU_POSIX_VALUEOF_TEST([O_EXLOCK])
NAU_POSIX_VALUEOF_TEST([O_FSYNC])
NAU_POSIX_VALUEOF_TEST([O_IGNORE_CTTY])
NAU_POSIX_VALUEOF_TEST([O_NDELAY])
NAU_POSIX_VALUEOF_TEST([O_NOCTTY])
NAU_POSIX_VALUEOF_TEST([O_NOLINK])
NAU_POSIX_VALUEOF_TEST([O_NONBLOCK])
NAU_POSIX_VALUEOF_TEST([O_NOTRANS])
NAU_POSIX_VALUEOF_TEST([O_RDONLY])
NAU_POSIX_VALUEOF_TEST([O_RDWR])
NAU_POSIX_VALUEOF_TEST([O_READ])
NAU_POSIX_VALUEOF_TEST([O_SHLOCK])
NAU_POSIX_VALUEOF_TEST([O_SYNC])
NAU_POSIX_VALUEOF_TEST([O_TRUNC])
NAU_POSIX_VALUEOF_TEST([O_WRITE])
NAU_POSIX_VALUEOF_TEST([O_WRONLY])

dnl This is GNU specific.
NAU_POSIX_VALUEOF_TEST([O_NOATIME])

NAU_POSIX_VALUEOF_TEST([FD_CLOEXEC])

AC_CACHE_SAVE

dnl In alphabetical order.
NAU_POSIX_VALUEOF_TEST([F_DUPFD])
NAU_POSIX_VALUEOF_TEST([F_GETFD])
NAU_POSIX_VALUEOF_TEST([F_GETFL])
NAU_POSIX_VALUEOF_TEST([F_GETLK])
NAU_POSIX_VALUEOF_TEST([F_GETOWN])
NAU_POSIX_VALUEOF_TEST([F_SETFD])
NAU_POSIX_VALUEOF_TEST([F_SETFL])
NAU_POSIX_VALUEOF_TEST([F_SETLKW])
NAU_POSIX_VALUEOF_TEST([F_SETLK])
NAU_POSIX_VALUEOF_TEST([F_SETOWN])

dnl In alphabetical order.
NAU_POSIX_VALUEOF_TEST([F_RDLCK])
NAU_POSIX_VALUEOF_TEST([F_UNLCK])
NAU_POSIX_VALUEOF_TEST([F_WRLCK])

NAU_POSIX_VALUEOF_TEST([WNOHANG])
NAU_POSIX_VALUEOF_TEST([WUNTRACED])
NAU_POSIX_VALUEOF_TEST([WCONTINUED])

NAU_POSIX_VALUEOF_TEST([R_OK])
NAU_POSIX_VALUEOF_TEST([W_OK])
NAU_POSIX_VALUEOF_TEST([X_OK])
NAU_POSIX_VALUEOF_TEST([F_OK])

NAU_POSIX_VALUEOF_TEST([L_ctermid])
NAU_POSIX_VALUEOF_TEST([L_tmpnam])

NAU_POSIX_VALUEOF_TEST([CLOCKS_PER_SEC])

dnl Mode bits.
NAU_POSIX_VALUEOF_TEST([S_IRUSR])
NAU_POSIX_VALUEOF_TEST([S_IWUSR])
NAU_POSIX_VALUEOF_TEST([S_IXUSR])

NAU_POSIX_VALUEOF_TEST([S_IRGRP])
NAU_POSIX_VALUEOF_TEST([S_IWGRP])
NAU_POSIX_VALUEOF_TEST([S_IXGRP])

NAU_POSIX_VALUEOF_TEST([S_IROTH])
NAU_POSIX_VALUEOF_TEST([S_IWOTH])
NAU_POSIX_VALUEOF_TEST([S_IXOTH])

NAU_POSIX_VALUEOF_TEST([S_IRWXU])
NAU_POSIX_VALUEOF_TEST([S_IRWXG])
NAU_POSIX_VALUEOF_TEST([S_IRWXO])

NAU_POSIX_VALUEOF_TEST([S_ISUID])
NAU_POSIX_VALUEOF_TEST([S_ISGID])
NAU_POSIX_VALUEOF_TEST([S_ISVTX])

AC_CACHE_SAVE

dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../libraries/posix/sizeof.sls:libraries/posix/sizeof.sls.in])
NAUSICAA_END


### end of file
