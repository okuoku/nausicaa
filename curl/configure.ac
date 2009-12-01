dnl
dnl Part of: Nausicaa/cURL
dnl Contents: configuration script
dnl Date: Mon Nov 16, 2009
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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

AC_INIT([nausicaa-curl],[0.1a1],[marco.maggi-ipsu@poste.it])
DS_BEGIN([./src/libraries/foreign/net/curl.sls])
AC_CANONICAL_TARGET
NAUSICAA_BEGIN
DS_EXCLUDE_SECTION([CONFIG_INSPECTION_SCRIPT])
DS_EXCLUDE_SECTION([AUTOCONF])
DS_EXCLUDE_SECTION([PKGCONFIG])
NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_FOREIGN],[(foreign ffi)])
NAUSICAA_C_LANGUAGE
AC_CHECK_HEADERS([curl/curl.h],,[AC_MSG_FAILURE([cannot find cURL headers],[2])])
AC_CHECK_LIB([curl],[curl_global_init],,[AC_MSG_FAILURE([cannot find cURL library],[2])])
NAUSICAA_SIZEOF

AC_CHECK_HEADER([ioctl.h])
AC_CHECK_HEADER([time.h])

dnl page
dnl --------------------------------------------------------------------
dnl Inspection.
dnl --------------------------------------------------------------------

m4_define([NAUSICAA_CUSTOM_INCLUDES],[
#include <curl/curl.h>
#ifdef HAVE_FCNTL_H
#  include <fcntl.h>
#endif
#ifdef HAVE_TIME_H
#  include <time.h>
#endif
])

m4_include(configuration/curl-inspector.m4)

dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../src/libraries/foreign/net/curl/sizeof.sls:src/libraries/foreign/net/curl/sizeof.sls.in])
DS_CONFIG_FILE_SLACKWARE_BIN_DESC
DS_CONFIG_FILE_SLACKWARE_DOC_DESC
NAUSICAA_END
DS_END

### end of file
