dnl
dnl Part of: Nausicaa/Pango
dnl Contents: configuration pango
dnl Date: Sat Oct 31, 2009
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

AC_INIT([nausicaa-pango],[0.1d0],[marcomaggi@gna.org])
DS_BEGIN([./src/libraries/foreign/graphics/pango.sls])
AC_CANONICAL_TARGET
NAUSICAA_BEGIN
DS_EXCLUDE_SECTION([CONFIG_INSPECTION_SCRIPT])
DS_EXCLUDE_SECTION([AUTOCONF])
DS_EXCLUDE_SECTION([PKGCONFIG])
dnl NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_FOREIGN],[(foreign ffi)])
NAUSICAA_C_LANGUAGE

NAU_DS_WITH_OPTION([GLIB_INCLUDEDIR],[glib-includedir],
  [/usr/local/include/glib-2.0],[Glib include directory],
  [select include directory for Glib headers])

NAU_DS_WITH_OPTION([GLIB_LIBINCLUDEDIR],[glib-libincludedir],
  [/usr/local/lib/glib-2.0/include],[Glib lib include directory],
  [select lib include directory for Glib headers])

GLIB_CPPFLAGS="-I$GLIB_INCLUDEDIR -I$GLIB_LIBINCLUDEDIR"
CPPFLAGS="$GLIB_CPPFLAGS $CPPFLAGS"

NAU_DS_WITH_OPTION([PANGO_INCLUDEDIR],[pango-includedir],
  [/usr/local/include/pango-1.0],[Pango include directory],
  [select include directory for Pango headers])

PANGO_CPPFLAGS="-I$PANGO_INCLUDEDIR"
CPPFLAGS="$PANGO_CPPFLAGS $CPPFLAGS"

NAU_DS_WITH_OPTION([PANGO_LIBDIR],[pango-libdir],
  [/usr/local/lib],[Pango library directory],
  [select library directory for Pango shared object])

PANGO_LDFLAGS=-L$PANGO_LIBDIR
LDFLAGS="$PANGO_LDFLAGS $LDFLAGS"

AC_CHECK_HEADERS([glib.h],,[AC_MSG_FAILURE([cannot find Glib header],[2])])
AC_CHECK_HEADERS([pango/pango.h],,[AC_MSG_FAILURE([cannot find Pango header],[2])])
AC_CHECK_LIB([pango-1.0],[pango_engine_shape_get_type],,[AC_MSG_FAILURE([cannot find Pango library],[2])])

NAUSICAA_SIZEOF

dnl page
dnl --------------------------------------------------------------------
dnl Helper functions.
dnl --------------------------------------------------------------------

m4_define([NAU_PANGO_INCLUDES],[
#include <glib/glib.h>
#include <pango/pango.h>
])

AC_DEFUN([NAU_PANGO_INSPECT_TYPE],
  [NAUSICAA_INSPECT_TYPE([$1],[$2],[$3],[#f],[NAU_PANGO_INCLUDES])])

AC_DEFUN([NAU_PANGO_INSPECT_STRUCT_TYPE],
  [NAUSICAA_INSPECT_STRUCT_TYPE([$1],[$2],[#f],[NAU_PANGO_INCLUDES])])

AC_DEFUN([NAU_PANGO_INSPECT_FIELD_TYPE],
  [NAUSICAA_INSPECT_FIELD_TYPE([$1],[$2],[$3],[$4],[#f],[NAU_PANGO_INCLUDES])])

AC_DEFUN([NAU_PANGO_ENUM_VALUE],
  [NAU_PANGO_VALUEOF_TEST([$1],[$1])])

AC_DEFUN([NAU_PANGO_DEFINE_VALUE],
  [NAU_PANGO_VALUEOF_TEST([$1],[$1])])

dnl --------------------------------------------------------------------

AC_DEFUN([NAU_PANGO_SIZEOF_TEST],
  [NAUSICAA_SIZEOF_TEST([$1],[$2],[#f],[NAU_PANGO_INCLUDES])])

AC_DEFUN([NAU_PANGO_VALUEOF_TEST],
  [NAUSICAA_VALUEOF_TEST([$1],[$2],[#f],[NAU_PANGO_INCLUDES])])

AC_DEFUN([NAU_PANGO_OFFSETOF_FIELD_TEST],
  [NAUSICAA_OFFSETOF_FIELD_TEST([$1],[$2],[$3],[NAU_PANGO_INCLUDES])])

AC_DEFUN([NAU_PANGO_STRUCT_FIELD],
  [NAUSICAA_INSPECT_FIELD_TYPE([$1],[$2],[$3],[$4],,[NAU_PANGO_INCLUDES])])

dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../src/libraries/foreign/graphics/pango/sizeof.sls:src/libraries/foreign/graphics/pango/sizeof.sls.in])
DS_CONFIG_FILE_SLACKWARE_BIN_DESC
DS_CONFIG_FILE_SLACKWARE_DOC_DESC
NAUSICAA_END
DS_END

### end of file
