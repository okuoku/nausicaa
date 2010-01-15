dnl
dnl Part of: Nausicaa/MP
dnl Contents: configuration template file
dnl Date: Sun Dec  7, 2008
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2008, 2009, 2010  Marco Maggi <marco.maggi-ipsu@poste.it>
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

AC_INIT([nausicaa-mp],[0.1b5],[marco.maggi-ipsu@poste.it])
DS_BEGIN([./src/libraries/foreign/math/mp/sizeof.sls.in])
AC_CANONICAL_TARGET
NAUSICAA_BEGIN
NAUSICAA_SYSTEM_SETUP
DS_EXCLUDE_SECTION([CONFIG_INSPECTION_SCRIPT])
DS_EXCLUDE_SECTION([AUTOCONF])
DS_EXCLUDE_SECTION([PKGCONFIG])
NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_FOREIGN],[(foreign ffi)])
NAUSICAA_C_LANGUAGE
NAUSICAA_SIZEOF

dnl page
dnl --------------------------------------------------------------------
dnl Checking libraries.
dnl --------------------------------------------------------------------

dnl The use of GMP is mandatory, we cannot disable it.

AC_CHECK_LIB([gmp],[__gmpz_init],,
  [AC_MSG_FAILURE([missing GMP library],[2])])
AC_CHECK_HEADERS([gmp.h],,
  [AC_MSG_FAILURE([missing gmp.h header],[2])])

dnl --------------------------------------------------------------------

NAU_DS_ENABLE_OPTION([nau_ENABLE_MPFR],[mpfr],[yes],
        [whether the MPFR interface will be used],
        [disable MPFR interface])

if test "${nau_ENABLE_MPFR}" = yes ; then

AC_CHECK_LIB([mpfr],[mpfr_init],,[AC_MSG_FAILURE([missing or unusable MPFR library],[2])])
AC_CHECK_HEADERS([mpfr.h],,[AC_MSG_FAILURE([missing mpfr.h header],[2])])

fi

dnl --------------------------------------------------------------------

NAU_DS_ENABLE_OPTION([nau_ENABLE_MPFI],[mpfi],[yes],
        [whether the MPFI interface will be used],
        [disable MPFI interface])

if test "${nau_ENABLE_MPFI}" = yes ; then
  if test "${nau_ENABLE_MPFR}" = yes ; then

AC_CHECK_HEADERS([mpfi.h],,[AC_MSG_FAILURE([missing mpfi.h header],[2])])
AC_CHECK_LIB([mpfi],[mpfi_init],,[AC_MSG_FAILURE([missing or unusable MPFI library],[2])])

  else
    AC_MSG_FAILURE([to enable MPFI, MPFR support must be enabled, too],[2])
  fi
fi

dnl --------------------------------------------------------------------

NAU_DS_ENABLE_OPTION([nau_ENABLE_MPC],[mpc],[yes],
        [whether the MPC interface will be used],
        [disable MPC interface])

if test "${nau_ENABLE_MPC}" = yes ; then
  if test "${nau_ENABLE_MPFR}" = yes ; then

# In version 0.7 of MPC the function "mpc_init()" does not exist.
#
AC_CHECK_HEADERS([mpc.h],,[AC_MSG_FAILURE([missing mpc.h header],[2])])
AC_CHECK_LIB([mpc],[mpc_clear],,[AC_MSG_FAILURE([missing or unusable MPC library],[2])])

  else
    AC_MSG_FAILURE([to enable MPC, MPFR support must be enabled, too],[2])
  fi
fi

dnl --------------------------------------------------------------------

NAU_DS_ENABLE_OPTION([nau_ENABLE_MPFRCX],[mpfrcx],[no],
  [whether the MPFRCX interface will be used],
  [disable MPFRCX interface])

if test "${nau_ENABLE_MPFRCX}" = yes ; then
  if test "${nau_ENABLE_MPFR}" = yes ; then
    if test "${nau_ENABLE_MPC}" = yes ; then

AC_CHECK_HEADERS([mpfrcx.h],,[AC_MSG_FAILURE([missing mpfrcx.h header],[2])])
AC_CHECK_LIB([mpfrcx],[mpfrx_mul],,[AC_MSG_FAILURE([missing or unusable MPFRCX library],[2])])


    else
      AC_MSG_FAILURE([to enable MPFRCX, MPC support must be enabled, too],[2])
    fi
  else
    AC_MSG_FAILURE([to enable MPFRCX, MPFR support must be enabled, too],[2])
  fi
fi

dnl page
dnl --------------------------------------------------------------------
dnl Inspection.
dnl --------------------------------------------------------------------

m4_define([NAUSICAA_CUSTOM_INCLUDES],[
#ifdef HAVE_GMP_H
#  include <gmp.h>
#endif
#ifdef HAVE_MFPR_H
#  include <mpfr.h>
#endif
#ifdef HAVE_MPFI_H
#  include <mpfi.h>
#endif
#ifdef HAVE_MPC_H
#  include <mpc.h>
#endif
#ifdef HAVE_MPFRCX_H
#  include <mpfrcx.h>
#endif
])

m4_include([configuration/mp-inspector.m4])

dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../src/libraries/foreign/math/mp/sizeof.sls:src/libraries/foreign/math/mp/sizeof.sls.in])
DS_CONFIG_FILE_SLACKWARE_BIN_DESC
DS_CONFIG_FILE_SLACKWARE_DOC_DESC
NAUSICAA_END
DS_END

### end of file
