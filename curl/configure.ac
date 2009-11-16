dnl
dnl Part of: Nausicaa/Curl
dnl Contents: configuration curl
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

AC_INIT([nausicaa-curl],[0.1d0],[marco.maggi-ipsu@poste.it])
DS_BEGIN([./src/libraries/foreign/net/curl.sls])
AC_CANONICAL_TARGET
NAUSICAA_BEGIN
DS_EXCLUDE_SECTION([CONFIG_INSPECTION_SCRIPT])
DS_EXCLUDE_SECTION([AUTOCONF])
DS_EXCLUDE_SECTION([PKGCONFIG])
NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_FOREIGN],[(foreign ffi)])
NAUSICAA_C_LANGUAGE
AC_CHECK_HEADERS([curl/curl.h],,[AC_MSG_FAILURE([cannot find Curl headers],[2])])
AC_CHECK_LIB([curl],[curl_global_init],,[AC_MSG_FAILURE([cannot find Curl library],[2])])
NAUSICAA_SIZEOF

dnl page
dnl --------------------------------------------------------------------
dnl Helper functions.
dnl --------------------------------------------------------------------

m4_define([NAU_CURL_INCLUDES],[
#include <curl/curl.h>
])

AC_DEFUN([NAU_CURL_INSPECT_TYPE],
  [NAUSICAA_INSPECT_TYPE([$1],[$2],[$3],[#f],[NAU_CURL_INCLUDES])])

AC_DEFUN([NAU_CURL_INSPECT_STRUCT_TYPE],
  [NAUSICAA_INSPECT_STRUCT_TYPE([$1],[$2],[#f],[NAU_CURL_INCLUDES])])

AC_DEFUN([NAU_CURL_INSPECT_FIELD_TYPE],
  [NAUSICAA_INSPECT_FIELD_TYPE([$1],[$2],[$3],[$4],[#f],[NAU_CURL_INCLUDES])])

AC_DEFUN([NAU_CURL_ENUM_VALUE],
  [NAU_CURL_VALUEOF_TEST([$1],[$1])])

AC_DEFUN([NAU_CURL_DEFINE_VALUE],
  [NAU_CURL_VALUEOF_TEST([$1],[$1])])

AC_DEFUN([NAU_CURL_DEFINE_STRING],
  [NAUSICAA_STRING_TEST([$1],[$1],[#f],[NAU_CURL_INCLUDES])])

dnl --------------------------------------------------------------------

AC_DEFUN([NAU_CURL_SIZEOF_TEST],
  [NAUSICAA_SIZEOF_TEST([$1],[$2],[#f],[NAU_CURL_INCLUDES])])

AC_DEFUN([NAU_CURL_VALUEOF_TEST],
  [NAUSICAA_VALUEOF_TEST([$1],[$2],[#f],[NAU_CURL_INCLUDES])])

AC_DEFUN([NAU_CURL_OFFSETOF_FIELD_TEST],
  [NAUSICAA_OFFSETOF_FIELD_TEST([$1],[$2],[$3],[NAU_CURL_INCLUDES])])

AC_DEFUN([NAU_CURL_STRUCT_FIELD],
  [NAUSICAA_INSPECT_FIELD_TYPE([$1],[$2],[$3],[$4],,[NAU_CURL_INCLUDES])])

dnl page
dnl --------------------------------------------------------------------
dnl Data types inspection.
dnl --------------------------------------------------------------------

NAU_CURL_INSPECT_TYPE([CURL_SOCKET],[curl_socket_t],[signed-int])
NAU_CURL_INSPECT_TYPE([CURL_OFF],[curl_off_t],[signed-int])


dnl page
dnl --------------------------------------------------------------------
dnl Data structures type inspection.
dnl --------------------------------------------------------------------

AC_DEFUN([NAU_CURL_HTTPPOST_FIELD],
  [NAU_CURL_INSPECT_FIELD_TYPE([CURL_HTTPPOST_$1],[struct curl_httppost],[$2],[$3])])

NAU_CURL_INSPECT_STRUCT_TYPE([CURL_HTTPPOST],[struct curl_httppost])
NAU_CURL_HTTPPOST_FIELD([NEXT],[next],[pointer])
NAU_CURL_HTTPPOST_FIELD([NAME],[name],[pointer])
NAU_CURL_HTTPPOST_FIELD([NAMELENGTH],[namelength],[signed-int])
NAU_CURL_HTTPPOST_FIELD([CONTENTS],[contents],[pointer])
NAU_CURL_HTTPPOST_FIELD([CONTENTSLENGTH],[contentslength],[signed-int])
NAU_CURL_HTTPPOST_FIELD([BUFFER],[buffer],[pointer])
NAU_CURL_HTTPPOST_FIELD([BUFFERLENGTH],[bufferlength],[signed-int])
NAU_CURL_HTTPPOST_FIELD([CONTENTTYPE],[contenttype],[pointer])
NAU_CURL_HTTPPOST_FIELD([CONTENTTYPE],[contenttype],[pointer])
NAU_CURL_HTTPPOST_FIELD([CONTENTHEADER],[contentheader],[pointer])
NAU_CURL_HTTPPOST_FIELD([MORE],[more],[pointer])
NAU_CURL_HTTPPOST_FIELD([FLAGS],[flags],[signed-int])
NAU_CURL_HTTPPOST_FIELD([SHOWFILENAME],[showfilename],[pointer])
NAU_CURL_HTTPPOST_FIELD([USERP],[userp],[pointer])

dnl page
dnl --------------------------------------------------------------------
dnl Constants.
dnl --------------------------------------------------------------------

dnl version informations
NAU_CURL_DEFINE_STRING([LIBCURL_COPYRIGHT])
NAU_CURL_DEFINE_STRING([LIBCURL_TIMESTAMP])

NAU_CURL_DEFINE_VALUE([LIBCURL_VERSION_MAJOR])
NAU_CURL_DEFINE_VALUE([LIBCURL_VERSION_MINOR])
NAU_CURL_DEFINE_VALUE([LIBCURL_VERSION_PATCH])
NAU_CURL_DEFINE_VALUE([LIBCURL_VERSION_NUM])

dnl flag values for the "flags" field of structure "curl_httppost"
NAU_CURL_DEFINE_VALUE([HTTPPOST_FILENAME])
NAU_CURL_DEFINE_VALUE([HTTPPOST_READFILE])
NAU_CURL_DEFINE_VALUE([HTTPPOST_PTRNAME])
NAU_CURL_DEFINE_VALUE([HTTPPOST_PTRCONTENTS])
NAU_CURL_DEFINE_VALUE([HTTPPOST_BUFFER])
NAU_CURL_DEFINE_VALUE([HTTPPOST_PTRBUFFER])
NAU_CURL_DEFINE_VALUE([HTTPPOST_CALLBACK])

dnl miscellaneous constants
NAU_CURL_DEFINE_VALUE([CURL_SOCKET_BAD])


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
