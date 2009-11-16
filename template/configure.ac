dnl
dnl Part of: Nausicaa/Template
dnl Contents: configuration template
dnl Date: Thu Oct 22, 2009
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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

AC_INIT([nausicaa-template],[0.1d0],[marcomaggi@gna.org])
DS_BEGIN([./src/libraries/foreign/template.sls])
AC_CANONICAL_TARGET
NAUSICAA_BEGIN
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

m4_define([NAU_TEMPLATE_INCLUDES],[
#include <template/template.h>
])

AC_DEFUN([NAU_TEMPLATE_INSPECT_TYPE],
  [NAUSICAA_INSPECT_TYPE([$1],[$2],[$3],[#f],[NAU_TEMPLATE_INCLUDES])])

AC_DEFUN([NAU_TEMPLATE_INSPECT_STRUCT_TYPE],
  [NAUSICAA_INSPECT_STRUCT_TYPE([$1],[$2],[#f],[NAU_TEMPLATE_INCLUDES])])

AC_DEFUN([NAU_TEMPLATE_INSPECT_FIELD_TYPE],
  [NAUSICAA_INSPECT_FIELD_TYPE([$1],[$2],[$3],[$4],[#f],[NAU_TEMPLATE_INCLUDES])])

AC_DEFUN([NAU_TEMPLATE_ENUM_VALUE],
  [NAU_TEMPLATE_VALUEOF_TEST([$1],[$1])])

AC_DEFUN([NAU_TEMPLATE_DEFINE_VALUE],
  [NAU_TEMPLATE_VALUEOF_TEST([$1],[$1])])

dnl --------------------------------------------------------------------

AC_DEFUN([NAU_TEMPLATE_SIZEOF_TEST],
  [NAUSICAA_SIZEOF_TEST([$1],[$2],[#f],[NAU_TEMPLATE_INCLUDES])])

AC_DEFUN([NAU_TEMPLATE_VALUEOF_TEST],
  [NAUSICAA_VALUEOF_TEST([$1],[$2],[#f],[NAU_TEMPLATE_INCLUDES])])

AC_DEFUN([NAU_TEMPLATE_OFFSETOF_FIELD_TEST],
  [NAUSICAA_OFFSETOF_FIELD_TEST([$1],[$2],[$3],[NAU_TEMPLATE_INCLUDES])])

AC_DEFUN([NAU_TEMPLATE_STRUCT_FIELD],
  [NAUSICAA_INSPECT_FIELD_TYPE([$1],[$2],[$3],[$4],,[NAU_TEMPLATE_INCLUDES])])

dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../src/libraries/foreign/template/sizeof.sls:src/libraries/foreign/template/sizeof.sls.in])
DS_CONFIG_FILE_SLACKWARE_BIN_DESC
DS_CONFIG_FILE_SLACKWARE_DOC_DESC
NAUSICAA_END
DS_END

### end of file
