dnl (foreign uuid sizeof) --
dnl
dnl Part of: Nausicaa
dnl Contents: foreign library inspection generation
dnl Date: Mon Mar 29, 2010
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
dnl
dnl This program is free software:  you can redistribute it and/or modify
dnl it under the terms of the  GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or (at
dnl your option) any later version.
dnl
dnl This program is  distributed in the hope that it  will be useful, but
dnl WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
dnl MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
dnl General Public License for more details.
dnl
dnl You should  have received  a copy of  the GNU General  Public License
dnl along with this program.  If not, see <http://www.gnu.org/licenses/>.
dnl

AC_DEFUN([NAUSICAA_UUID],[


dnl Preprocessor symbols: version
NAUSICAA_DEFINE_VALUE([UUID_VERSION])

dnl Preprocessor symbols: encoding octet stream lengths
NAUSICAA_DEFINE_VALUE([UUID_LEN_BIN])
NAUSICAA_DEFINE_VALUE([UUID_LEN_STR])
NAUSICAA_DEFINE_VALUE([UUID_LEN_SIV])
NAUSICAA_INSPECT_TYPE([UUID_RC_T],[uuid_rc_t],[signed-int],[#f])

dnl enum uuid_rc_t
NAUSICAA_ENUM_VALUE([UUID_RC_OK])
NAUSICAA_ENUM_VALUE([UUID_RC_ARG])
NAUSICAA_ENUM_VALUE([UUID_RC_MEM])
NAUSICAA_ENUM_VALUE([UUID_RC_SYS])
NAUSICAA_ENUM_VALUE([UUID_RC_INT])
NAUSICAA_ENUM_VALUE([UUID_RC_IMP])

dnl Preprocessor symbols: UUID make modes
NAUSICAA_DEFINE_VALUE([UUID_MAKE_V1])
NAUSICAA_DEFINE_VALUE([UUID_MAKE_V3])
NAUSICAA_DEFINE_VALUE([UUID_MAKE_V4])
NAUSICAA_DEFINE_VALUE([UUID_MAKE_V5])
NAUSICAA_DEFINE_VALUE([UUID_MAKE_MC])
NAUSICAA_INSPECT_TYPE([UUID_FMT_T],[uuid_fmt_t],[signed-int],[#f])

dnl enum uuid_fmt_t
NAUSICAA_ENUM_VALUE([UUID_FMT_BIN])
NAUSICAA_ENUM_VALUE([UUID_FMT_STR])
NAUSICAA_ENUM_VALUE([UUID_FMT_SIV])
NAUSICAA_ENUM_VALUE([UUID_FMT_TXT])
NAU_DS_WITH_OPTION([UUID_SHARED_OBJECT],[uuid-shared-object],[libuuid.so],
  [Uuid shared library file],[select Uuid shared library file])


])


dnl end of file
