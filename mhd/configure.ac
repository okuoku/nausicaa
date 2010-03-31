dnl
dnl Part of: Nausicaa/MHD
dnl Contents: configuration template
dnl Date: Wed Dec  2, 2009
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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

AC_INIT([nausicaa-mhd],[0.1d0],[marco.maggi-ipsu@poste.it])
DS_BEGIN([./src/libraries/net/mhd.sls])
AC_CANONICAL_TARGET
NAUSICAA_BEGIN
NAUSICAA_SYSTEM_SETUP
DS_EXCLUDE_SECTION([CONFIG_INSPECTION_SCRIPT])
DS_EXCLUDE_SECTION([AUTOCONF])
DS_EXCLUDE_SECTION([PKGCONFIG])
NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_FOREIGN],[(foreign ffi)])
NAUSICAA_C_LANGUAGE
AC_CHECK_HEADERS([microhttpd.h],,[AC_MSG_FAILURE([cannot find MHD header],[2])])
AC_CHECK_LIB([microhttpd],[MHD_start_daemon],,[AC_MSG_FAILURE([cannot find MHD library],[2])])
NAUSICAA_SIZEOF

AC_CHECK_HEADER([unistd.h])

dnl page
dnl --------------------------------------------------------------------
dnl Inspection.
dnl --------------------------------------------------------------------

m4_define([NAUSICAA_CUSTOM_INCLUDES],[
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#else
#define socklen_t       size_t
#endif
#include <microhttpd.h>
])

NAUSICAA_MHD

dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../src/libraries/net/mhd/sizeof.sls:src/libraries/net/mhd/sizeof.sls.in])
DS_CONFIG_FILE_SLACKWARE_BIN_DESC
DS_CONFIG_FILE_SLACKWARE_DOC_DESC
NAUSICAA_END
DS_END

### end of file
