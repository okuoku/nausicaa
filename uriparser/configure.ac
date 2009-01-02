dnl
dnl Part of: Nausicaa/Uriparser
dnl Contents: configuration template
dnl Date: Tue Dec 23, 2008
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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

AC_INIT([nausicaa-uriparser],[0.1d0])
NAUSICAA_BEGIN
NAUSICAA_SYSTEM_SETUP
NAUSICAA_C_LANGUAGE

dnl --------------------------------------------------------------------

URIPARSER_MISSING="Please install uriparser 0.6.4 or later.
   On a Debian-based system enter 'sudo apt-get install liburiparser-dev'."
AC_CHECK_LIB(uriparser, uriParseUriA,, AC_MSG_ERROR(${URIPARSER_MISSING}))
AC_CHECK_HEADER(uriparser/Uri.h,, AC_MSG_ERROR(${URIPARSER_MISSING}))

URIPARSER_TOO_OLD="uriparser 0.6.4 or later is required, your copy is too old."
AC_COMPILE_IFELSE([
#include <uriparser/Uri.h>
#if (defined(URI_VER_MAJOR) && defined(URI_VER_MINOR) && defined(URI_VER_RELEASE) \
&& ((URI_VER_MAJOR > 0) \
|| ((URI_VER_MAJOR == 0) && (URI_VER_MINOR > 6)) \
|| ((URI_VER_MAJOR == 0) && (URI_VER_MINOR == 6) && (URI_VER_RELEASE >= 4)) \
))
/* FINE */
#else
# error uriparser not recent enough
#endif
],,AC_MSG_ERROR(${URIPARSER_TOO_OLD}))

dnl --------------------------------------------------------------------

NAUSICAA_SIZEOF

dnl page
dnl --------------------------------------------------------------------
dnl Customised tests.
dnl --------------------------------------------------------------------

m4_define([NAU_URIPARSER_INCLUDES],[
#include <uriparser/Uri.h>
])

AC_DEFUN([NAU_URIPARSER_VALUEOF_TEST],
   [NAUSICAA_VALUEOF_TEST([$1],[$1],[#f],[NAU_URIPARSER_INCLUDES])])

AC_DEFUN([NAU_URIPARSER_SIZEOF_TEST],
   [NAUSICAA_SIZEOF_TEST([$1],[$2],[#f],[NAU_URIPARSER_INCLUDES])])

AC_DEFUN([NAU_URIPARSER_INSPECT_TYPE],
  [NAUSICAA_INSPECT_TYPE([$1],[$2],[$3],[#f],[NAU_URIPARSER_INCLUDES])])

AC_DEFUN([NAU_URIPARSER_INSPECT_FIELD_TYPE],
  [NAUSICAA_INSPECT_FIELD_TYPE([$1],[$2],[$3],[$4],[#f],[NAU_URIPARSER_INCLUDES])])

AC_DEFUN([NAU_URIPARSER_INSPECT_STRUCT_TYPE],
   [NAUSICAA_INSPECT_STRUCT_TYPE([$1],[$2],[#f],[NAU_URIPARSER_INCLUDES])])

AC_DEFUN([NAU_URIPARSER_INSPECT_FIELD_TYPE_POINTER],
   [NAUSICAA_INSPECT_FIELD_STRUCT_TYPE([$1],[$2],[$3],[NAU_URIPARSER_INCLUDES])])


dnl page
dnl --------------------------------------------------------------------
dnl Typedefs.
dnl --------------------------------------------------------------------

NAU_URIPARSER_INSPECT_TYPE([URI_BOOL],[UriBool],[signed-int])
NAU_URIPARSER_INSPECT_TYPE([URI_BREAK_CONVERSION],[UriBreakConversion],[signed-int])

NAU_URIPARSER_INSPECT_STRUCT_TYPE([URI_TEXT_RANGE_A],[UriTextRangeA])
NAU_URIPARSER_INSPECT_STRUCT_TYPE([URI_PATH_SEGMENT_A],[UriPathSegmentA])
NAU_URIPARSER_INSPECT_STRUCT_TYPE([URI_HOST_DATA_A],[UriHostDataA])
NAU_URIPARSER_INSPECT_STRUCT_TYPE([URI_URI_A],[UriUriA])
NAU_URIPARSER_INSPECT_STRUCT_TYPE([URI_PARSER_STATE_A],[UriParserStateA])
NAU_URIPARSER_INSPECT_STRUCT_TYPE([URI_QUERY_LIST_A],[UriQueryListA])

dnl UriParserStateA
NAU_URIPARSER_INSPECT_FIELD_TYPE([URI_PARSER_STATE_A_URI],
  [UriParserStateA],[uri],[pointer])

dnl UriTextRangeA
NAU_URIPARSER_INSPECT_FIELD_TYPE([URI_TEXT_RANGE_A_FIRST],[UriTextRangeA],[first],[pointer])
NAU_URIPARSER_INSPECT_FIELD_TYPE([URI_TEXT_RANGE_A_AFTER_LAST],[UriTextRangeA],[afterLast],[pointer])

dnl UriUriA
NAU_URIPARSER_INSPECT_FIELD_TYPE_POINTER([URI_URI_A_SCHEME],[UriUriA],[scheme])
NAU_URIPARSER_INSPECT_FIELD_TYPE_POINTER([URI_URI_A_USER_INFO],[UriUriA],[userInfo])
NAU_URIPARSER_INSPECT_FIELD_TYPE_POINTER([URI_URI_A_HOST_TEXT],[UriUriA],[hostText])
NAU_URIPARSER_INSPECT_FIELD_TYPE_POINTER([URI_URI_A_HOST_DATA],[UriUriA],[hostData])
NAU_URIPARSER_INSPECT_FIELD_TYPE_POINTER([URI_URI_A_PORT_TEXT],[UriUriA],[portText])
NAU_URIPARSER_INSPECT_FIELD_TYPE([URI_URI_A_PATH_HEAD],[UriUriA],[pathHead],[pointer])
NAU_URIPARSER_INSPECT_FIELD_TYPE([URI_URI_A_PATH_TAIL],[UriUriA],[pathTail],[pointer])
NAU_URIPARSER_INSPECT_FIELD_TYPE_POINTER([URI_URI_A_QUERY],[UriUriA],[query])
NAU_URIPARSER_INSPECT_FIELD_TYPE_POINTER([URI_URI_A_FRAGMENT],[UriUriA],[fragment])
NAU_URIPARSER_INSPECT_FIELD_TYPE([URI_URI_A_ABSOLUTE_PATH],[UriUriA],[absolutePath],[signed-int])
NAU_URIPARSER_INSPECT_FIELD_TYPE([URI_URI_A_OWNER],[UriUriA],[owner],[signed-int])


dnl page
dnl --------------------------------------------------------------------
dnl Constants.
dnl --------------------------------------------------------------------

NAU_URIPARSER_VALUEOF_TEST([URI_SUCCESS])
NAU_URIPARSER_VALUEOF_TEST([URI_ERROR_SYNTAX])
NAU_URIPARSER_VALUEOF_TEST([URI_ERROR_NULL])
NAU_URIPARSER_VALUEOF_TEST([URI_ERROR_MALLOC])
NAU_URIPARSER_VALUEOF_TEST([URI_ERROR_OUTPUT_TOO_LARGE])
NAU_URIPARSER_VALUEOF_TEST([URI_ERROR_NOT_IMPLEMENTED])
NAU_URIPARSER_VALUEOF_TEST([URI_ERROR_RANGE_INVALID])
NAU_URIPARSER_VALUEOF_TEST([URI_ERROR_TOSTRING_TOO_LONG])
NAU_URIPARSER_VALUEOF_TEST([URI_ERROR_ADDBASE_REL_BASE])
NAU_URIPARSER_VALUEOF_TEST([URI_ERROR_REMOVEBASE_REL_BASE])
NAU_URIPARSER_VALUEOF_TEST([URI_ERROR_REMOVEBASE_REL_SOURCE])

NAU_URIPARSER_VALUEOF_TEST([URI_BR_TO_LF])
NAU_URIPARSER_VALUEOF_TEST([URI_BR_TO_CRLF])
NAU_URIPARSER_VALUEOF_TEST([URI_BR_TO_CR])
NAU_URIPARSER_VALUEOF_TEST([URI_BR_TO_UNIX])
NAU_URIPARSER_VALUEOF_TEST([URI_BR_TO_WINDOWS])
NAU_URIPARSER_VALUEOF_TEST([URI_BR_TO_MAC])
NAU_URIPARSER_VALUEOF_TEST([URI_BR_DONT_TOUCH])

NAU_URIPARSER_VALUEOF_TEST([URI_NORMALIZED])
NAU_URIPARSER_VALUEOF_TEST([URI_NORMALIZE_SCHEME])
NAU_URIPARSER_VALUEOF_TEST([URI_NORMALIZE_USER_INFO])
NAU_URIPARSER_VALUEOF_TEST([URI_NORMALIZE_HOST])
NAU_URIPARSER_VALUEOF_TEST([URI_NORMALIZE_PATH])
NAU_URIPARSER_VALUEOF_TEST([URI_NORMALIZE_QUERY])
NAU_URIPARSER_VALUEOF_TEST([URI_NORMALIZE_FRAGMENT])


dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../libraries/uriparser/sizeof.sls:libraries/uriparser/sizeof.sls.in])
NAUSICAA_END


### end of file
