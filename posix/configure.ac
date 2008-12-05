dnl
dnl Part of: Nausicaa/POSIX
dnl Contents: configuration file template
dnl Date: Fri Dec  5, 2008
dnl Time-stamp: <2008-12-05 17:52:57 marco>
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
AC_PREREQ(2.60)
AC_CONFIG_SRCDIR([libraries/compile-all.sps])
NAUSICAA_BEGIN()
DS_IKARUS_CHECK_LIBRARY([URIEL_FFI],[(uriel ffi)])
DS_IKARUS_CHECK_LIBRARY([URIEL_LANG],[(uriel lang)])
NAUSICAA_SYSTEM_SETUP()
NAUSICAA_C_LANGUAGE()
NAUSICAA_SIZEOF()


dnl page
dnl --------------------------------------------------------------------
dnl Helpers.
dnl --------------------------------------------------------------------

AC_DEFUN([NAU_POSIX_INSPECT_TYPE],[
NAUSICAA_INSPECT_TYPE([$1],[$2],[$3],[
#include <fcntl.h>
#include <unistd.h>
])
])

AC_DEFUN([NAU_POSIX_INSPECT_FIELD_TYPE],[
NAUSICAA_INSPECT_FIELD_TYPE([$1],[$2],[$3],[$4],[
#include <fcntl.h>
#include <unistd.h>
])
])

AC_DEFUN([NAU_POSIX_OFFSETOF],[
NAUSICAA_OFFSETOF_TEST([$1],[$2],[$3],[
#include <fcntl.h>
#include <unistd.h>
])
])

AC_DEFUN([NAU_POSIX_SIZEOF],[
NAUSICAA_SIZEOF_TEST([$1],[$2],[#f],[
#include <fcntl.h>
#include <unistd.h>
])
])

AC_DEFUN([NAU_POSIX_VALUEOF],[
NAUSICAA_VALUEOF_TEST([$1],[$1],[#f],[
#include <fcntl.h>
#include <unistd.h>
])
])


dnl page
dnl --------------------------------------------------------------------
dnl Inspection: typedefs.
dnl --------------------------------------------------------------------

NAU_POSIX_INSPECT_TYPE([MODE_T],[mode_t],[int])
NAU_POSIX_INSPECT_TYPE([PID_T],[pid_t],[int])
NAU_POSIX_INSPECT_TYPE([OFF_T],[off_t],[uint])

NAU_POSIX_SIZEOF([STRUCT_FLOCK],[struct flock])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_FLOCK_L_TYPE],[struct flock],[l_type],[int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_FLOCK_L_WHENCE],[struct flock],[l_whence],[int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_FLOCK_L_START],[struct flock],[l_start],[int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_FLOCK_L_LEN],[struct flock],[l_len],[int])
NAU_POSIX_INSPECT_FIELD_TYPE([STRUCT_FLOCK_L_PID],[struct flock],[l_pid],[int])



dnl page
dnl --------------------------------------------------------------------
dnl Inspection: file descriptors related constants.
dnl --------------------------------------------------------------------

dnl In alphabetical order.
NAU_POSIX_VALUEOF([O_ACCMODE])
NAU_POSIX_VALUEOF([O_APPEND])
NAU_POSIX_VALUEOF([O_ASYNC])
NAU_POSIX_VALUEOF([O_CREAT])
NAU_POSIX_VALUEOF([O_EXCL])
NAU_POSIX_VALUEOF([O_EXEC])
NAU_POSIX_VALUEOF([O_EXLOCK])
NAU_POSIX_VALUEOF([O_FSYNC])
NAU_POSIX_VALUEOF([O_IGNORE_CTTY])
NAU_POSIX_VALUEOF([O_NDELAY])
NAU_POSIX_VALUEOF([O_NOATIME])
NAU_POSIX_VALUEOF([O_NOCTTY])
NAU_POSIX_VALUEOF([O_NOLINK])
NAU_POSIX_VALUEOF([O_NONBLOCK])
NAU_POSIX_VALUEOF([O_NOTRANS])
NAU_POSIX_VALUEOF([O_RDONLY])
NAU_POSIX_VALUEOF([O_RDWR])
NAU_POSIX_VALUEOF([O_READ])
NAU_POSIX_VALUEOF([O_SHLOCK])
NAU_POSIX_VALUEOF([O_SYNC])
NAU_POSIX_VALUEOF([O_TRUNC])
NAU_POSIX_VALUEOF([O_WRITE])
NAU_POSIX_VALUEOF([O_WRONLY])

NAU_POSIX_VALUEOF([FD_CLOEXEC])

dnl In alphabetical order.
NAU_POSIX_VALUEOF([F_DUPFD])
NAU_POSIX_VALUEOF([F_GETFD])
NAU_POSIX_VALUEOF([F_GETFL])
NAU_POSIX_VALUEOF([F_GETLK])
NAU_POSIX_VALUEOF([F_GETOWN])
NAU_POSIX_VALUEOF([F_SETFD])
NAU_POSIX_VALUEOF([F_SETFL])
NAU_POSIX_VALUEOF([F_SETLKW])
NAU_POSIX_VALUEOF([F_SETLK])
NAU_POSIX_VALUEOF([F_SETOWN])

dnl In alphabetical order.
NAU_POSIX_VALUEOF([F_RDLCK])
NAU_POSIX_VALUEOF([F_UNLCK])
NAU_POSIX_VALUEOF([F_WRLCK])


dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../libraries/posix/sizeof.sls:libraries/posix/sizeof.sls.in])
AC_CONFIG_FILES([../libraries/posix/fields.sls:libraries/posix/fields.sls.in])
NAUSICAA_END()


### end of file
