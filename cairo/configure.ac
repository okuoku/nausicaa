dnl
dnl Part of: Nausicaa/Cairo
dnl Contents: configuration template
dnl Date: Thu Oct 22, 2009
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

AC_INIT([nausicaa-cairo],[0.1b3],[marco.maggi-ipsu@poste.it])
DS_BEGIN([./src/libraries/foreign/graphics/cairo.sls])
AC_CANONICAL_TARGET
NAUSICAA_BEGIN
NAUSICAA_SYSTEM_SETUP
DS_EXCLUDE_SECTION([CONFIG_INSPECTION_SCRIPT])
DS_EXCLUDE_SECTION([AUTOCONF])
DS_EXCLUDE_SECTION([PKGCONFIG])
NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_FOREIGN],[(foreign ffi)])
NAUSICAA_C_LANGUAGE

AC_CHECK_HEADERS([cairo/cairo.h],,
  [AC_MSG_FAILURE([cannot find Cairo's cairo.h header],[2])])
AC_CHECK_HEADERS([cairo/cairo-pdf.h],,
  [AC_MSG_WARN([cannot find Cairo's cairo-pdf.h header])])
AC_CHECK_HEADERS([cairo/cairo-svg.h],,
  [AC_MSG_WARN([cannot find Cairo's cairo-svg.h header])])
AC_CHECK_HEADERS([cairo/cairo-ps.h],,
  [AC_MSG_WARN([cannot find Cairo's cairo-ps.h header])])
AC_CHECK_HEADERS([cairo/cairo-xlib.h],,
  [AC_MSG_WARN([cannot find Cairo's cairo-xlib.h header])])
AC_CHECK_HEADERS([cairo/cairo-xlib-xrender.h],,
  [AC_MSG_WARN([cannot find Cairo's cairo-xlib-xrender.h header])])
AC_CHECK_HEADERS([cairo/cairo-ft.h],,
  [AC_MSG_WARN([cannot find Cairo' cairo-ft.h header])])

AC_CHECK_LIB([cairo],[cairo_create],,[AC_MSG_FAILURE([cannot find Cairo library],[2])])

NAUSICAA_SIZEOF

dnl page
dnl --------------------------------------------------------------------
dnl Inspection.
dnl --------------------------------------------------------------------

m4_define([NAUSICAA_CUSTOM_INCLUDES],[
#include <cairo/cairo.h>
#if HAVE_CAIRO_CAIRO_H
#  include <cairo/cairo-pdf.h>
#endif
#if HAVE_CAIRO_CAIRO_SVG_H
#  include <cairo/cairo-svg.h>
#endif
#if HAVE_CAIRO_CAIRO_PS_H
#  include <cairo/cairo-ps.h>
#endif
#if HAVE_CAIRO_CAIRO_XLIB_H
#  include <cairo/cairo-xlib.h>
#endif
#if HAVE_CAIRO_CAIRO_XLIB_XRENDER_H
#  include <cairo/cairo-xlib-xrender.h>
#endif
#if HAVE_CAIRO_CAIRO_FT_H
#  include <cairo/cairo-ft.h>
#endif
])

NAUSICAA_CAIRO


dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../src/libraries/foreign/graphics/cairo/sizeof.sls:src/libraries/foreign/graphics/cairo/sizeof.sls.in])
DS_CONFIG_FILE_SLACKWARE_BIN_DESC
DS_CONFIG_FILE_SLACKWARE_DOC_DESC
NAUSICAA_END
DS_END

### end of file
