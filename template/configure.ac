dnl
dnl Part of: Nausicaa/Template
dnl Contents: configuration template
dnl Date:
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

AC_INIT([nausicaa-template],[0.1d0],[marco.maggi-ipsu@poste.it])
DS_BEGIN([./src/libraries/foreign/category/template.sls])
AC_CANONICAL_TARGET
NAUSICAA_BEGIN
NAUSICAA_SYSTEM_SETUP
DS_EXCLUDE_SECTION([CONFIG_INSPECTION_SCRIPT])
DS_EXCLUDE_SECTION([AUTOCONF])
DS_EXCLUDE_SECTION([PKGCONFIG])
NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_FOREIGN],[(foreign ffi)])
NAUSICAA_C_LANGUAGE
AC_CHECK_HEADERS([template.h],,[AC_MSG_FAILURE([cannot find Template header],[2])])
AC_CHECK_LIB([template],[template_init],,[AC_MSG_FAILURE([cannot find Template library],[2])])
NAUSICAA_SIZEOF

dnl page
dnl --------------------------------------------------------------------
dnl Helper functions.
dnl --------------------------------------------------------------------

m4_define([NAUSICAA_CUSTOM_INCLUDES],[
#include <template/template.h>
])

m4_include([configuration/template-inspector.m4])

dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../src/libraries/foreign/category/template/sizeof.sls:src/libraries/foreign/category/template/sizeof.sls.in])
DS_CONFIG_FILE_SLACKWARE_BIN_DESC
DS_CONFIG_FILE_SLACKWARE_DOC_DESC
NAUSICAA_END
DS_END

### end of file
