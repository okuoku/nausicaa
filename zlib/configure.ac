dnl
dnl Part of: Nausicaa/Zlib
dnl Contents: configuration template
dnl Date: Sun Dec  7, 2008
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2008-2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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

AC_INIT([nausicaa-zlib],[0.2a0],[marco.maggi-ipsu@poste.it])
DS_BEGIN([./src/libraries/compression/zlib.sls])
AC_CANONICAL_TARGET
NAUSICAA_BEGIN
NAUSICAA_SYSTEM_SETUP
DS_EXCLUDE_SECTION([CONFIG_INSPECTION_SCRIPT])
DS_EXCLUDE_SECTION([AUTOCONF])
DS_EXCLUDE_SECTION([PKGCONFIG])
NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_FOREIGN],[(foreign ffi)])
NAUSICAA_C_LANGUAGE
AC_CHECK_HEADERS([zlib.h],,[AC_MSG_FAILURE([cannot find libz header],[2])])
AC_CHECK_LIB([z],[deflate],,[AC_MSG_FAILURE([cannot find libz library],[2])])
NAUSICAA_SIZEOF

dnl page
dnl --------------------------------------------------------------------
dnl Zlib inspection.
dnl --------------------------------------------------------------------

m4_define([NAUSICAA_CUSTOM_INCLUDES],[
#include <zlib.h>
])

NAUSICAA_ZLIB

dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../src/libraries/compression/zlib/sizeof.sls:src/libraries/compression/zlib/sizeof.sls.in])
DS_CONFIG_FILE_SLACKWARE_BIN_DESC
DS_CONFIG_FILE_SLACKWARE_DOC_DESC
NAUSICAA_END
DS_END

### end of file
