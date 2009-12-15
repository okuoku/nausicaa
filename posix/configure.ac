dnl
dnl Part of: Nausicaa/POSIX
dnl Contents: configuration file template
dnl Date: Fri Dec  5, 2008
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2008, 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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

AC_INIT([nausicaa-posix],[0.1a3],[marco.maggi-ipsu@poste.it])
DS_BEGIN([./src/libraries/foreign/posix/fd.sls])
NAUSICAA_BEGIN
NAUSICAA_SYSTEM_SETUP
DS_EXCLUDE_SECTION([CONFIG_INSPECTION_SCRIPT])
DS_EXCLUDE_SECTION([AUTOCONF])
DS_EXCLUDE_SECTION([PKGCONFIG])

NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_FOREIGN],[(foreign ffi)])

DS_C99_LANGUAGE_COMMON_SETUP
DS_EXCLUDE_SECTION([DEV_RULES])

AC_CHECK_FUNC([stat])
AC_CHECK_FUNC([lstat])
AC_CHECK_FUNC([fstat])

AC_CHECK_HEADERS([errno.h])
AC_CHECK_HEADERS([fcntl.h])
AC_CHECK_HEADERS([inttypes.h])
AC_CHECK_HEADERS([limits.h])
AC_CHECK_HEADERS([stdint.h])
AC_CHECK_HEADERS([unistd.h])

AC_CHECK_HEADERS([dirent.h])
AC_CHECK_HEADERS([fcntl.h])
AC_CHECK_HEADERS([ftw.h])
AC_CHECK_HEADERS([pwd.h])
AC_CHECK_HEADERS([grp.h])
AC_CHECK_HEADERS([sys/mman.h])
AC_CHECK_HEADERS([sys/select.h])
AC_CHECK_HEADERS([sys/stat.h])
AC_CHECK_HEADERS([sys/time.h])
AC_CHECK_HEADERS([sys/times.h])
AC_CHECK_HEADERS([sys/timex.h])
AC_CHECK_HEADERS([sys/types.h])
AC_CHECK_HEADERS([sys/wait.h])
AC_CHECK_HEADERS([time.h])
AC_CHECK_HEADERS([unistd.h])
AC_CHECK_HEADERS([utime.h])

NAUSICAA_SIZEOF
NAUSICAA_C_TYPES_LIMITS

DS_C_LANGUAGE_LIBRARY([posix_stub],1,0,[nausicaa-posix])

dnl page
dnl --------------------------------------------------------------------
dnl Inspection.
dnl --------------------------------------------------------------------

m4_define([NAUSICAA_CUSTOM_INCLUDES],[
#ifdef HAVE_DIRENT_H
#  include <dirent.h>
#endif
#ifdef HAVE_FCNTL_H
#  include <fcntl.h>
#endif
#ifdef HAVE_FTW_H
#  include <ftw.h>
#endif
#ifdef HAVE_GRP_H
#  include <grp.h>
#endif
#ifdef HAVE_INTTYPES_H
#  include <inttypes.h>
#endif
#ifdef HAVE_LIMITS_H
#  include <limits.h>
#endif
#ifdef HAVE_PWD_H
#  include <pwd.h>
#endif
#ifdef HAVE_TIME_H
#  include <time.h>
#endif
#ifdef HAVE_UTIME_H
#  include <utime.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#  include <sys/mman.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#  include <sys/select.h>
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
#ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#endif
#ifdef HAVE_STDINT_H
#  include <stdint.h>
#endif
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
])

m4_include([configuration/posix-inspector.m4])

dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../src/libraries/foreign/posix/sizeof.sls:../src/libraries/foreign/posix/sizeof.sls.in])
AC_CONFIG_FILES([meta.d/makefiles/library.make:../configuration/library.make])

DS_CONFIG_FILE_SLACKWARE_BIN_DESC
DS_CONFIG_FILE_SLACKWARE_DOC_DESC
DS_CONFIG_FILE_SLACKWARE_DEV_DESC
NAUSICAA_END
DS_END

### end of file
