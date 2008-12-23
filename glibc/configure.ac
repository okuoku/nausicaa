dnl
dnl Part of: Nausicaa/Glibc
dnl Contents: configuration file template
dnl Date: Mon Dec 22, 2008
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
dnl
dnl This  program  is free  software:  you  can redistribute  it
dnl and/or modify it  under the terms of the  GNU General Public
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

AC_INIT([nausicaa-glibc],[0.1d0])
NAUSICAA_BEGIN
NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_R6RS],[(r6rs)])
NAUSICAA_SCHEME_CHECK_LIBRARY([URIEL_LANG],[(uriel lang)])
NAUSICAA_SCHEME_CHECK_LIBRARY([URIEL_FOREIGN],[(uriel foreign)])
NAUSICAA_SCHEME_CHECK_LIBRARY([POSIX_SIZEOF],[(posix sizeof)])
NAUSICAA_SYSTEM_SETUP
NAUSICAA_C_LANGUAGE
AC_CHECK_HEADERS([time.h])
AC_CHECK_HEADERS([sys/time.h])
AC_CHECK_HEADERS([sys/times.h])
AC_CHECK_HEADERS([sys/timex.h])
NAUSICAA_SIZEOF


dnl page
dnl --------------------------------------------------------------------
dnl Customised tests.
dnl --------------------------------------------------------------------

m4_define([NAU_GLIBC_INCLUDES],[
#include <stdio.h>
#include <time.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/timex.h>
])

AC_DEFUN([NAU_GLIBC_VALUEOF_TEST],
   [NAUSICAA_VALUEOF_TEST([$1],[$1],[#f],[NAU_GLIBC_INCLUDES])])

AC_DEFUN([NAU_GLIBC_SIZEOF_TEST],
   [NAUSICAA_SIZEOF_TEST([$1],[$2],[#f],[NAU_GLIBC_INCLUDES])])

AC_DEFUN([NAU_GLIBC_INSPECT_TYPE],
  [NAUSICAA_INSPECT_TYPE([$1],[$2],[$3],[#f],[NAU_GLIBC_INCLUDES])])

AC_DEFUN([NAU_GLIBC_INSPECT_FIELD_TYPE],
  [NAUSICAA_INSPECT_FIELD_TYPE([$1],[$2],[$3],[$4],[#f],[NAU_GLIBC_INCLUDES])])

AC_DEFUN([NAU_GLIBC_INSPECT_STRUCT_TYPE],
   [NAUSICAA_INSPECT_STRUCT_TYPE([$1],[$2],[#f],[NAU_GLIBC_INCLUDES])])

AC_DEFUN([NAU_GLIBC_INSPECT_FIELD_STRUCT_TYPE],
   [NAUSICAA_INSPECT_FIELD_STRUCT_TYPE([$1],[$2],[$3],[NAU_GLIBC_INCLUDES])])


dnl page
dnl --------------------------------------------------------------------
dnl Typedefs.
dnl --------------------------------------------------------------------

NAU_GLIBC_INSPECT_TYPE([TIME_T],[time_t],[signed-int])

NAU_GLIBC_INSPECT_STRUCT_TYPE([STRUCT_TIMEZONE],[struct timezone])
NAU_GLIBC_INSPECT_FIELD_TYPE([STRUCT_TIMEZONE_TZ_MINUTESWEST],
        [struct timezone],[tz_minuteswest],[signed-int])
NAU_GLIBC_INSPECT_FIELD_TYPE([STRUCT_TIMEZONE_TZ_DSTTIME],
        [struct timezone],[tz_dsttime],[signed-int])

NAU_GLIBC_INSPECT_STRUCT_TYPE([STRUCT_TM],[struct tm])
NAU_GLIBC_INSPECT_FIELD_TYPE([STRUCT_TM_TM_SEC],[struct tm],[tm_sec],[signed-int])
NAU_GLIBC_INSPECT_FIELD_TYPE([STRUCT_TM_TM_MIN],[struct tm],[tm_min],[signed-int])
NAU_GLIBC_INSPECT_FIELD_TYPE([STRUCT_TM_TM_HOUR],[struct tm],[tm_hour],[signed-int])
NAU_GLIBC_INSPECT_FIELD_TYPE([STRUCT_TM_TM_MDAY],[struct tm],[tm_mday],[signed-int])
NAU_GLIBC_INSPECT_FIELD_TYPE([STRUCT_TM_TM_MON],[struct tm],[tm_mon],[signed-int])
NAU_GLIBC_INSPECT_FIELD_TYPE([STRUCT_TM_TM_YEAR],[struct tm],[tm_year],[signed-int])
NAU_GLIBC_INSPECT_FIELD_TYPE([STRUCT_TM_TM_WDAY],[struct tm],[tm_wday],[signed-int])
NAU_GLIBC_INSPECT_FIELD_TYPE([STRUCT_TM_TM_YDAY],[struct tm],[tm_yday],[signed-int])
NAU_GLIBC_INSPECT_FIELD_TYPE([STRUCT_TM_TM_ISDST],[struct tm],[tm_isdst],[signed-int])
NAU_GLIBC_INSPECT_FIELD_TYPE([STRUCT_TM_TM_GMTOFF],[struct tm],[tm_gmtoff],[signed-int])
NAU_GLIBC_INSPECT_FIELD_TYPE([STRUCT_TM_TM_ZONE],[struct tm],[tm_zone],[pointer])

NAU_GLIBC_INSPECT_STRUCT_TYPE([STRUCT_NTPTIMEVAL],[struct ntptimeval])
NAU_GLIBC_INSPECT_FIELD_STRUCT_TYPE([STRUCT_NTPTIMEVAL_TIME],
        [struct ntptimeval],[time])
NAU_GLIBC_INSPECT_FIELD_TYPE([STRUCT_NTPTIMEVAL_MAXERROR],
        [struct ntptimeval],[maxerror],[signed-int])
NAU_GLIBC_INSPECT_FIELD_TYPE([STRUCT_NTPTIMEVAL_ESTERROR],
        [struct ntptimeval],[esterror],[signed-int])


dnl page
dnl --------------------------------------------------------------------
dnl Constants.
dnl --------------------------------------------------------------------

NAU_GLIBC_VALUEOF_TEST([EOF])


dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../libraries/glibc/sizeof.sls:libraries/glibc/sizeof.sls.in])
NAUSICAA_END


### end of file
