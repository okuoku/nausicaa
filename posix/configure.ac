dnl
dnl Part of: Nausicaa/POSIX
dnl Contents: configuration file template
dnl Date: Fri Dec  5, 2008
dnl Time-stamp: <2008-12-23 08:19:23 marco>
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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


dnl page
dnl --------------------------------------------------------------------
dnl Helpers.
dnl --------------------------------------------------------------------

m4_define([NAU_POSIX_INCLUDES],[
#include <fcntl.h>
#include <time.h>
#include <sys/times.h>
])

AC_DEFUN([NAU_POSIX_VALUEOF_TEST],
   [NAUSICAA_VALUEOF_TEST([$1],[$1],[#f],[NAU_POSIX_INCLUDES])])

AC_DEFUN([NAU_POSIX_SIZEOF_TEST],
   [NAUSICAA_SIZEOF_TEST([$1],[$2],[#f],[NAU_POSIX_INCLUDES])])

AC_DEFUN([NAU_POSIX_INSPECT_TYPE],
  [NAUSICAA_INSPECT_TYPE([$1],[$2],[$3],[#f],[NAU_POSIX_INCLUDES])])

AC_DEFUN([NAU_POSIX_INSPECT_FIELD_TYPE],
  [NAUSICAA_INSPECT_FIELD_TYPE([$1],[$2],[$3],[$4],[#f],[NAU_POSIX_INCLUDES])])

AC_DEFUN([NAU_POSIX_INSPECT_STRUCT_TYPE],
   [NAUSICAA_INSPECT_STRUCT_TYPE([$1],[$2],[#f],[NAU_POSIX_INCLUDES])])


dnl page
dnl --------------------------------------------------------------------
dnl Inspection: typedefs.
dnl --------------------------------------------------------------------

NAU_POSIX_INSPECT_TYPE([MODE_T],[mode_t],[signed-int])
NAU_POSIX_INSPECT_TYPE([PID_T],[pid_t],[signed-int])
NAU_POSIX_INSPECT_TYPE([OFF_T],[off_t],[unsigned-int])
NAU_POSIX_INSPECT_TYPE([CLOCK_T],[clock_t],[signed-int])

NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_FLOCK],[struct flock])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_FLOCK_L_TYPE],[struct flock],[l_type],[signed-int],,)
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_FLOCK_L_WHENCE],[struct flock],[l_whence],[signed-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_FLOCK_L_START],[struct flock],[l_start],[signed-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_FLOCK_L_LEN],[struct flock],[l_len],[signed-int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_FLOCK_L_PID],[struct flock],[l_pid],[signed-int])

NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_TIMEVAL],[struct timeval])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_TIMEVAL_TV_SEC],[struct timeval],[tv_sec],[signed-int],,)
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_TIMEVAL_TV_USEC],[struct timeval],[tv_usec],[signed-int],,)

NAU_POSIX_INSPECT_STRUCT_TYPE([STRUCT_TIMESPEC],[struct timespec])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_TIMESPEC_TV_SEC],[struct timespec],[tv_sec],[signed-int],,)
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_TIMESPEC_TV_NSEC],[struct timespec],[tv_nsec],[signed-int],,)


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
NAU_POSIX_VALUEOF_TEST([O_NOATIME])
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

NAU_POSIX_VALUEOF_TEST([L_ctermid])

NAU_POSIX_VALUEOF_TEST([CLOCKS_PER_SEC])

AC_CACHE_SAVE

dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../libraries/posix/sizeof.sls:libraries/posix/sizeof.sls.in])
NAUSICAA_END


### end of file
