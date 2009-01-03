dnl
dnl Part of: Nausicaa/Expat
dnl Contents: configuration template
dnl Date: Sat Jan  3, 2009
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

AC_INIT([nausicaa-expat],[0.1d0])
NAUSICAA_BEGIN
NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_R6RS],[(r6rs)])
NAUSICAA_SCHEME_CHECK_LIBRARY([URIEL_LANG],[(uriel lang)])
NAUSICAA_SCHEME_CHECK_LIBRARY([URIEL_FOREIGN],[(uriel foreign)])
NAUSICAA_SYSTEM_SETUP
NAUSICAA_C_LANGUAGE
NAUSICAA_SIZEOF

AC_CHECK_LIB([expat],[XML_ParserCreate],,
  [AC_MSG_FAILURE([missing Expat library],[2])])
AC_CHECK_HEADERS([expat.h],,
  [AC_MSG_FAILURE([missing expat.h header],[2])])


dnl page
dnl --------------------------------------------------------------------
dnl Helper functions.
dnl --------------------------------------------------------------------

m4_define([NAU_EXPAT_INCLUDES],[
#include <expat.h>
])

AC_DEFUN([NAU_EXPAT_VALUEOF_TEST],
   [NAUSICAA_VALUEOF_TEST([$1],[$1],[#f],[NAU_EXPAT_INCLUDES])])

AC_DEFUN([NAU_EXPAT_SIZEOF_TEST],
   [NAUSICAA_SIZEOF_TEST([$1],[$2],[#f],[NAU_EXPAT_INCLUDES])])

AC_DEFUN([NAU_EXPAT_INSPECT_TYPE],
  [NAUSICAA_INSPECT_TYPE([$1],[$2],[$3],[#f],[NAU_EXPAT_INCLUDES])])

AC_DEFUN([NAU_EXPAT_INSPECT_FIELD_TYPE],
  [NAUSICAA_INSPECT_FIELD_TYPE([$1],[$2],[$3],[$4],[#f],[NAU_EXPAT_INCLUDES])])

AC_DEFUN([NAU_EXPAT_INSPECT_FIELD_TYPE_POINTER],
  [NAUSICAA_INSPECT_FIELD_TYPE_POINTER([$1],[$2],[$3],[NAU_EXPAT_INCLUDES])])

AC_DEFUN([NAU_EXPAT_INSPECT_STRUCT_TYPE],
   [NAUSICAA_INSPECT_STRUCT_TYPE([$1],[$2],[#f],[NAU_EXPAT_INCLUDES])])


dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../libraries/expat/sizeof.sls:libraries/expat/sizeof.sls.in])
NAUSICAA_END


dnl end of file
