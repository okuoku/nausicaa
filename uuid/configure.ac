dnl
dnl Part of: Nausicaa/UUID
dnl Contents: configuration template
dnl Date: Tue Oct 27, 2009
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

AC_INIT([nausicaa-uuid],[0.1d0],[marcomaggi@gna.org])
DS_BEGIN([./src/libraries/foreign/uuid.sls])
AC_CANONICAL_TARGET
NAUSICAA_BEGIN
DS_EXCLUDE_SECTION([CONFIG_INSPECTION_SCRIPT])
DS_EXCLUDE_SECTION([AUTOCONF])
DS_EXCLUDE_SECTION([PKGCONFIG])
dnl NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_FOREIGN],[(foreign ffi)])
NAUSICAA_C_LANGUAGE

AC_CHECK_HEADER([uuid.h],,[AC_MSG_ERROR([cannot find uuid.h],1)])
AC_CHECK_LIB([uuid],[uuid_compare])

NAUSICAA_SIZEOF

dnl page
dnl --------------------------------------------------------------------
dnl Helper functions.
dnl --------------------------------------------------------------------

AC_DEFUN([NAU_UUID_INSPECT_TYPE],
  [NAUSICAA_INSPECT_TYPE([$1],[$2],[$3],[#f],[#include <uuid.h>])])

AC_DEFUN([NAU_UUID_INSPECT_STRUCT_TYPE],
  [NAUSICAA_INSPECT_STRUCT_TYPE([$1],[$2],[#f],[#include <uuid.h>])])

AC_DEFUN([NAU_UUID_INSPECT_FIELD_TYPE],
  [NAUSICAA_INSPECT_FIELD_TYPE([$1],[$2],[$3],[$4],[#f],[#include <uuid.h>])])

AC_DEFUN([NAU_UUID_ENUM_VALUE],
  [NAU_UUID_VALUEOF_TEST([$1],[$1])])

AC_DEFUN([NAU_UUID_DEFINE_VALUE],
  [NAU_UUID_VALUEOF_TEST([$1],[$1])])

dnl --------------------------------------------------------------------

AC_DEFUN([NAU_UUID_SIZEOF_TEST],
  [NAUSICAA_SIZEOF_TEST([$1],[$2],[#f],[#include <uuid.h>])])

AC_DEFUN([NAU_UUID_VALUEOF_TEST],
  [NAUSICAA_VALUEOF_TEST([$1],[$2],[#f],[#include <uuid.h>])])

AC_DEFUN([NAU_UUID_OFFSETOF_FIELD_TEST],
  [NAUSICAA_OFFSETOF_FIELD_TEST([$1],[$2],[$3],[#include <uuid.h>])])

AC_DEFUN([NAU_UUID_STRUCT_FIELD],
  [NAUSICAA_INSPECT_FIELD_TYPE([$1],[$2],[$3],[$4],,[#include <uuid.h>])])

dnl page
dnl --------------------------------------------------------------------
dnl Data types inspection.
dnl --------------------------------------------------------------------

NAU_UUID_INSPECT_TYPE([UUID_RC],[uuid_rc_t],[unsigned-int])
NAU_UUID_INSPECT_TYPE([UUID_FMT],[uuid_fmt_t],[unsigned-int])




dnl page
dnl --------------------------------------------------------------------
dnl Constants.
dnl --------------------------------------------------------------------

NAU_UUID_DEFINE_VALUE([UUID_VERSION])

dnl encoding octet stream lengths
NAU_UUID_DEFINE_VALUE([UUID_LEN_BIN])
NAU_UUID_DEFINE_VALUE([UUID_LEN_STR])
NAU_UUID_DEFINE_VALUE([UUID_LEN_SIV])

dnl enum uuid_rc_t
NAU_UUID_ENUM_VALUE([UUID_RC_OK])
NAU_UUID_ENUM_VALUE([UUID_RC_ARG])
NAU_UUID_ENUM_VALUE([UUID_RC_MEM])
NAU_UUID_ENUM_VALUE([UUID_RC_SYS])
NAU_UUID_ENUM_VALUE([UUID_RC_INT])
NAU_UUID_ENUM_VALUE([UUID_RC_IMP])

dnl UUID make modes
NAU_UUID_ENUM_VALUE([UUID_MAKE_V1])
NAU_UUID_ENUM_VALUE([UUID_MAKE_V3])
NAU_UUID_ENUM_VALUE([UUID_MAKE_V4])
NAU_UUID_ENUM_VALUE([UUID_MAKE_V5])
NAU_UUID_ENUM_VALUE([UUID_MAKE_MC])

dnl enum uuid_fmt_t
NAU_UUID_ENUM_VALUE([UUID_FMT_BIN])
NAU_UUID_ENUM_VALUE([UUID_FMT_STR])
NAU_UUID_ENUM_VALUE([UUID_FMT_SIV])
NAU_UUID_ENUM_VALUE([UUID_FMT_TXT])

dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../src/libraries/foreign/uuid/sizeof.sls:src/libraries/foreign/uuid/sizeof.sls.in])
DS_CONFIG_FILE_SLACKWARE_BIN_DESC
DS_CONFIG_FILE_SLACKWARE_DOC_DESC
NAUSICAA_END
DS_END

### end of file
