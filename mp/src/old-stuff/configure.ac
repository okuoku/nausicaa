dnl
dnl Part of: Nausicaa/MP
dnl Contents: configuration template file
dnl Date: Sun Dec  7, 2008
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2008, 2009  Marco Maggi <marco.maggi-ipsu@poste.it>
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

AC_INIT([nausicaa-mp],[0.1b3],[marco.maggi-ipsu@poste.it])
DS_BEGIN([./src/libraries/foreign/math/mp/sizeof.sls.in])
AC_CANONICAL_TARGET
NAUSICAA_BEGIN
DS_EXCLUDE_SECTION([CONFIG_INSPECTION_SCRIPT])
DS_EXCLUDE_SECTION([AUTOCONF])
DS_EXCLUDE_SECTION([PKGCONFIG])
NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_FOREIGN],[(foreign ffi)])
NAUSICAA_C_LANGUAGE
NAUSICAA_SIZEOF

dnl page
dnl --------------------------------------------------------------------
dnl Helper functions.
dnl --------------------------------------------------------------------

m4_define([NAU_GMP_INCLUDES],[
#include <gmp.h>
])

m4_define([NAU_MPFR_INCLUDES],[
#include <gmp.h>
#include <mpfr.h>
#include <stdint.h>
])

m4_define([NAU_MPFI_INCLUDES],[
#include <gmp.h>
#include <mpfr.h>
#include <mpfi.h>
])

m4_define([NAU_MPC_INCLUDES],[
#include <gmp.h>
#include <mpfr.h>
#include <mpc.h>
])

m4_define([NAUSICAA_CUSTOM_INCLUDES],[
#ifdef HAVE_GMP_
#  include <gmp.h>
#endif
#ifdef HAVE_MFPR_H
#  include <mpfr.h>
#endif
#ifdef HAVE_MPC_H
#  include <mpc.h>
#endif
#ifdef HAVE_MPFRCX_H
#  include <mpfrcx.h>
#endif
])

dnl --------------------------------------------------------------------

AC_DEFUN([NAU_GMP_SIZEOF_TEST],
   [NAUSICAA_SIZEOF_TEST([$1],[$2],[#f],[NAU_GMP_INCLUDES])])

AC_DEFUN([NAU_GMP_ALIGNOF_TEST],
   [NAUSICAA_ALIGNOF_TEST([$1],[$2],[#f],[NAU_GMP_INCLUDES])])

AC_DEFUN([NAU_GMP_STRIDEOF_TEST],
   [NAUSICAA_STRIDEOF_TEST([$1],[$2],[#f],[NAU_GMP_INCLUDES])])

AC_DEFUN([NAU_GMP_VALUEOF_TEST],
   [NAUSICAA_VALUEOF_TEST([$1],[$2],[#f],[NAU_GMP_INCLUDES])])

AC_DEFUN([NAU_GMP_INSPECT_TYPE],
   [NAUSICAA_INSPECT_TYPE([$1],[$2],[$3],[#f],[NAU_GMP_INCLUDES])])

AC_DEFUN([NAU_GMP_OFFSETOF_FIELD_TEST],
   [NAUSICAA_OFFSETOF_FIELD_TEST([$1],[$2],[$3],[NAU_GMP_INCLUDES])])

dnl --------------------------------------------------------------------

AC_DEFUN([NAU_MPFR_SIZEOF_TEST],
   [NAUSICAA_SIZEOF_TEST([$1],[$2],[#f],[NAU_MPFR_INCLUDES])])

AC_DEFUN([NAU_MPFR_ALIGNOF_TEST],
   [NAUSICAA_ALIGNOF_TEST([$1],[$2],[#f],[NAU_MPFR_INCLUDES])])

AC_DEFUN([NAU_MPFR_STRIDEOF_TEST],
   [NAUSICAA_STRIDEOF_TEST([$1],[$2],[#f],[NAU_MPFR_INCLUDES])])

AC_DEFUN([NAU_MPFR_VALUEOF_TEST],
   [NAUSICAA_VALUEOF_TEST([$1],[$2],[#f],[NAU_MPFR_INCLUDES])])

AC_DEFUN([NAU_MPFR_INSPECT_TYPE],
   [NAUSICAA_INSPECT_TYPE([$1],[$2],[$3],[#f],[NAU_MPFR_INCLUDES])])

AC_DEFUN([NAU_MPFR_INSPECT_FIELD_TYPE],
   [NAUSICAA_INSPECT_FIELD_TYPE([$1],[$2],[$3],[$4],[#f],[NAU_MPFR_INCLUDES])])

dnl --------------------------------------------------------------------

AC_DEFUN([NAU_MPFI_SIZEOF_TEST],
   [NAUSICAA_SIZEOF_TEST([$1],[$2],[#f],[NAU_MPFI_INCLUDES])])

AC_DEFUN([NAU_MPFI_ALIGNOF_TEST],
   [NAUSICAA_ALIGNOF_TEST([$1],[$2],[#f],[NAU_MPFI_INCLUDES])])

AC_DEFUN([NAU_MPFI_STRIDEOF_TEST],
   [NAUSICAA_STRIDEOF_TEST([$1],[$2],[#f],[NAU_MPFI_INCLUDES])])

AC_DEFUN([NAU_MPFI_VALUEOF_TEST],
   [NAUSICAA_VALUEOF_TEST([$1],[$2],[#f],[NAU_MPFI_INCLUDES])])

AC_DEFUN([NAU_MPFI_OFFSETOF_FIELD_TEST],
   [NAUSICAA_OFFSETOF_FIELD_TEST([$1],[$2],[$3],[NAU_MPFI_INCLUDES])])

dnl --------------------------------------------------------------------

AC_DEFUN([NAU_MPC_SIZEOF_TEST],
   [NAUSICAA_SIZEOF_TEST([$1],[$2],[#f],[NAU_MPC_INCLUDES])])

AC_DEFUN([NAU_MPC_ALIGNOF_TEST],
   [NAUSICAA_ALIGNOF_TEST([$1],[$2],[#f],[NAU_MPC_INCLUDES])])

AC_DEFUN([NAU_MPC_STRIDEOF_TEST],
   [NAUSICAA_STRIDEOF_TEST([$1],[$2],[#f],[NAU_MPC_INCLUDES])])

AC_DEFUN([NAU_MPC_VALUEOF_TEST],
   [NAUSICAA_VALUEOF_TEST([$1],[$2],[#f],[NAU_MPC_INCLUDES])])

AC_DEFUN([NAU_MPC_INSPECT_TYPE],
   [NAUSICAA_INSPECT_TYPE([$1],[$2],[$3],[#f],[NAU_MPC_INCLUDES])])

AC_DEFUN([NAU_MPC_OFFSETOF_FIELD_TEST],
   [NAUSICAA_OFFSETOF_FIELD_TEST([$1],[$2],[$3],[NAU_MPC_INCLUDES])])

dnl --------------------------------------------------------------------

AC_DEFUN([NAU_MPFRCX_SIZEOF_TEST],
   [NAUSICAA_SIZEOF_TEST([$1],[$2],[#f],[NAU_MPFRCX_INCLUDES])])

AC_DEFUN([NAU_MPFRCX_VALUEOF_TEST],
   [NAUSICAA_VALUEOF_TEST([$1],[$2],[#f],[NAU_MPFRCX_INCLUDES])])

AC_DEFUN([NAU_MPFRCX_ALIGNOF_TEST],
   [NAUSICAA_ALIGNOF_TEST([$1],[$2],[#f],[NAU_MPFRCX_INCLUDES])])

AC_DEFUN([NAU_MPFRCX_STRIDEOF_TEST],
   [NAUSICAA_STRIDEOF_TEST([$1],[$2],[#f],[NAU_MPFRCX_INCLUDES])])

AC_DEFUN([NAU_MPFRCX_INSPECT_TYPE],
   [NAUSICAA_INSPECT_TYPE([$1],[$2],[$3],[#f],[NAU_MPFRCX_INCLUDES])])

AC_DEFUN([NAU_MPFRCX_INSPECT_FIELD_TYPE],
   [NAUSICAA_INSPECT_FIELD_TYPE([STRUCT_$1],[$2],[$3],[$4],[#f],[NAU_MPFRCX_INCLUDES])])

dnl page
dnl --------------------------------------------------------------------
dnl Checking GMP.
dnl --------------------------------------------------------------------

dnl The use of GMP is mandatory, we cannot disable it.

AC_CHECK_LIB([gmp],[__gmpz_init],,
  [AC_MSG_FAILURE([missing GMP library],[2])])
AC_CHECK_HEADERS([gmp.h],,
  [AC_MSG_FAILURE([missing gmp.h header],[2])])

NAU_GMP_INSPECT_TYPE([GMP_RANDALG_T],[gmp_randalg_t],[signed-int])
NAU_GMP_INSPECT_TYPE([MP_EXP_T],[mp_exp_t],[signed-int])
NAU_GMP_INSPECT_TYPE([MP_LIMB_T],[mp_limb_t],[signed-int])
NAU_GMP_INSPECT_TYPE([MP_SIZE_T],[mp_size_t],[signed-int])

NAU_GMP_SIZEOF_TEST([GMP_RANDSTATE_T],[gmp_randstate_t])
NAU_GMP_SIZEOF_TEST([MPF_T],[mpf_t])
NAU_GMP_SIZEOF_TEST([MPQ_T],[mpq_t])
NAU_GMP_SIZEOF_TEST([MPZ_T],[mpz_t])

NAU_GMP_ALIGNOF_TEST([GMP_RANDSTATE_T],[gmp_randstate_t])
NAU_GMP_ALIGNOF_TEST([MPF_T],[mpf_t])
NAU_GMP_ALIGNOF_TEST([MPQ_T],[mpq_t])
NAU_GMP_ALIGNOF_TEST([MPZ_T],[mpz_t])

NAU_GMP_STRIDEOF_TEST([GMP_RANDSTATE_T],[gmp_randstate_t])
NAU_GMP_STRIDEOF_TEST([MPF_T],[mpf_t])
NAU_GMP_STRIDEOF_TEST([MPQ_T],[mpq_t])
NAU_GMP_STRIDEOF_TEST([MPZ_T],[mpz_t])

NAU_GMP_OFFSETOF_FIELD_TEST([MPQ_STRUCT_NUM],[__mpq_struct],[_mp_num])
NAU_GMP_OFFSETOF_FIELD_TEST([MPQ_STRUCT_DEN],[__mpq_struct],[_mp_den])

NAU_GMP_VALUEOF_TEST([GMP_RAND_ALG_DEFAULT],[GMP_RAND_ALG_DEFAULT])
NAU_GMP_VALUEOF_TEST([GMP_RAND_ALG_LC],[GMP_RAND_ALG_LC])

dnl page
dnl --------------------------------------------------------------------
dnl Checking MPFR.
dnl --------------------------------------------------------------------

NAU_DS_ENABLE_OPTION([nau_ENABLE_MPFR],[mpfr],[yes],
        [whether the MPFR interface will be used],
        [disable MPFR interface])

if test "${nau_ENABLE_MPFR}" = yes ; then

AC_CHECK_LIB([mpfr],[mpfr_init],,[AC_MSG_FAILURE([missing or unusable MPFR library],[2])])
AC_CHECK_HEADERS([mpfr.h],,[AC_MSG_FAILURE([missing mpfr.h header],[2])])

else
NAUSICAA_DISABLE_TESTS
fi

NAU_MPFR_SIZEOF_TEST([MPFR_T],[mpfr_t])
NAU_MPFR_ALIGNOF_TEST([MPFR_T],[mpfr_t])
NAU_MPFR_STRIDEOF_TEST([MPFR_T],[mpfr_t])

NAU_MPFR_INSPECT_TYPE([MP_RND_T],[mp_rnd_t],[signed-int])
NAU_MPFR_INSPECT_TYPE([MP_PREC_T],[mp_prec_t],[unsigned-int])
NAU_MPFR_INSPECT_TYPE([MPFR_PREC_T],[mpfr_prec_t],[unsigned-int])
NAU_MPFR_INSPECT_TYPE([MPFR_SIGN_T],[mpfr_sign_t],[signed-int])
NAU_MPFR_INSPECT_TYPE([MPFR_RND_T],[mpfr_rnd_t],[signed-int])

NAU_MPFR_INSPECT_TYPE([INTMAX_T],[intmax_t],[signed-int])
NAU_MPFR_INSPECT_TYPE([UINTMAX_T],[uintmax_t],[unsigned-int])

NAU_MPFR_INSPECT_FIELD_TYPE([MPFR_STRUCT_PREC],[__mpfr_struct],[_mpfr_prec],[unsigned-int])
NAU_MPFR_INSPECT_FIELD_TYPE([MPFR_STRUCT_SIGN],[__mpfr_struct],[_mpfr_sign],[signed-int])
NAU_MPFR_INSPECT_FIELD_TYPE([MPFR_STRUCT_EXP],[__mpfr_struct],[_mpfr_exp],[signed-int])
NAU_MPFR_INSPECT_FIELD_TYPE([MPFR_STRUCT_D],[__mpfr_struct],[_mpfr_d],[pointer])

NAU_MPFR_VALUEOF_TEST([GMP_RNDD],[GMP_RNDD])
NAU_MPFR_VALUEOF_TEST([GMP_RNDN],[GMP_RNDN])
NAU_MPFR_VALUEOF_TEST([GMP_RNDU],[GMP_RNDU])
NAU_MPFR_VALUEOF_TEST([GMP_RNDZ],[GMP_RNDZ])
NAU_MPFR_VALUEOF_TEST([MPFR_PREC_MAX],[MPFR_PREC_MAX])
NAU_MPFR_VALUEOF_TEST([MPFR_PREC_MIN],[MPFR_PREC_MIN])
NAU_MPFR_VALUEOF_TEST([MPFR_VERSION],[MPFR_VERSION])
NAU_MPFR_VALUEOF_TEST([MPFR_EMAX_DEFAULT],[MPFR_EMAX_DEFAULT])
NAU_MPFR_VALUEOF_TEST([MPFR_EMIN_DEFAULT],[MPFR_EMIN_DEFAULT])

NAUSICAA_ENABLE_TESTS

dnl page
dnl --------------------------------------------------------------------
dnl Checking MPFI.
dnl --------------------------------------------------------------------

NAU_DS_ENABLE_OPTION([nau_ENABLE_MPFI],[mpfi],[yes],
        [whether the MPFI interface will be used],
        [disable MPFI interface])

if test "${nau_ENABLE_MPFI}" = yes ; then
  if test "${nau_ENABLE_MPFR}" = yes ; then

AC_CHECK_HEADERS([mpfi.h],,[AC_MSG_FAILURE([missing mpfr.h header],[2])])
AC_CHECK_LIB([mpfi],[mpfi_init],,[AC_MSG_FAILURE([missing or unusable MPFI library],[2])])

  else
    AC_MSG_FAILURE([to enable MPFI, MPFR support must be enabled, too],[2])
  fi
else
NAUSICAA_DISABLE_TESTS
fi

NAU_MPFI_SIZEOF_TEST([MPFI_T],[mpfi_t])
NAU_MPFI_ALIGNOF_TEST([MPFI_T],[mpfi_t])
NAU_MPFI_STRIDEOF_TEST([MPFI_T],[mpfi_t])

NAU_MPFI_OFFSETOF_FIELD_TEST([MPFI_STRUCT_LEFT],[__mpfi_struct],[left])
NAU_MPFI_OFFSETOF_FIELD_TEST([MPFI_STRUCT_RIGHT],[__mpfi_struct],[right])

NAU_MPFI_VALUEOF_TEST([MPFI_FLAGS_BOTH_ENDPOINTS_EXACT],[MPFI_FLAGS_BOTH_ENDPOINTS_EXACT])
NAU_MPFI_VALUEOF_TEST([MPFI_FLAGS_LEFT_ENDPOINT_INEXACT],[MPFI_FLAGS_LEFT_ENDPOINT_INEXACT])
NAU_MPFI_VALUEOF_TEST([MPFI_FLAGS_RIGHT_ENDPOINT_INEXACT],[MPFI_FLAGS_RIGHT_ENDPOINT_INEXACT])
NAU_MPFI_VALUEOF_TEST([MPFI_FLAGS_BOTH_ENDPOINTS_INEXACT],[MPFI_FLAGS_BOTH_ENDPOINTS_INEXACT])

NAUSICAA_ENABLE_TESTS

dnl page
dnl --------------------------------------------------------------------
dnl Checking MPC.
dnl --------------------------------------------------------------------

NAU_DS_ENABLE_OPTION([nau_ENABLE_MPC],[mpc],[yes],
        [whether the MPC interface will be used],
        [disable MPC interface])

if test "${nau_ENABLE_MPC}" = yes ; then
  if test "${nau_ENABLE_MPFR}" = yes ; then

# In version 0.7 of MPC the function "mpc_init()" does not exist.
#
AC_CHECK_LIB([mpc],[mpc_clear],,[AC_MSG_FAILURE([missing or unusable MPC library],[2])])
AC_CHECK_HEADERS([mpc.h],,[AC_MSG_FAILURE([missing mpc.h header],[2])])

  else
    AC_MSG_FAILURE([to enable MPC, MPFR support must be enabled, too],[2])
  fi
else
NAUSICAA_DISABLE_TESTS
fi

NAU_MPC_SIZEOF_TEST([MPC_T],[mpc_t])
NAU_MPC_ALIGNOF_TEST([MPC_T],[mpc_t])
NAU_MPC_STRIDEOF_TEST([MPC_T],[mpc_t])

NAU_MPC_OFFSETOF_FIELD_TEST([MPC_STRUCT_RE],[__mpc_struct],[re])
NAU_MPC_OFFSETOF_FIELD_TEST([MPC_STRUCT_IM],[__mpc_struct],[im])

NAU_MPC_VALUEOF_TEST([MPC_VERSION],[MPC_VERSION])

NAU_MPC_INSPECT_TYPE([MPC_RND_T],[mpc_rnd_t],[signed-int])

NAUSICAA_ENABLE_TESTS

dnl page
dnl --------------------------------------------------------------------
dnl Checking MPFRCX.
dnl --------------------------------------------------------------------

NAU_DS_ENABLE_OPTION([nau_ENABLE_MPFRCX],[mpfrcx],[no],
  [whether the MPFRCX interface will be used],
  [disable MPFRCX interface])

if test "${nau_ENABLE_MPFRCX}" = yes ; then
  if test "${nau_ENABLE_MPFR}" = yes ; then
    if test "${nau_ENABLE_MPC}" = yes ; then

AC_CHECK_LIB([mpfrcx],[mpfrx_mul],,[AC_MSG_FAILURE([missing or unusable MPFRCX library],[2])])
AC_CHECK_HEADERS([mpfrcx.h],,[AC_MSG_FAILURE([missing mpfrcx.h header],[2])])

    else
      AC_MSG_FAILURE([to enable MPFRCX, MPC support must be enabled, too],[2])
    fi
  else
    AC_MSG_FAILURE([to enable MPFRCX, MPFR support must be enabled, too],[2])
  fi
else
NAUSICAA_DISABLE_TESTS
fi

NAU_MPFRCX_VALUEOF_TEST([MPFRCX_VERSION],[MPFRCX_VERSION])

NAU_MPFRCX_SIZEOF_TEST([MPFRX_T],[mpfrx_t])
NAU_MPFRCX_ALIGNOF_TEST([MPFRX_T],[mpfrx_t])
NAU_MPFRCX_STRIDEOF_TEST([MPCX_T],[mpcx_t])

NAU_MPFRCX_SIZEOF_TEST([MPCX_T],[mpcx_t])
NAU_MPFRCX_ALIGNOF_TEST([MPCX_T],[mpcx_t])
NAU_MPFRCX_STRIDEOF_TEST([MPFRX_T],[mpfrx_t])

NAU_MPFRCX_INSPECT_FIELD_TYPE([MPFRX_T_SIZE],[__mpfrx_struct],[size],[signed-int])
NAU_MPFRCX_INSPECT_FIELD_TYPE([MPFRX_T_DEG],[__mpfrx_struct],[deg],[signed-int])
NAU_MPFRCX_INSPECT_FIELD_TYPE([MPFRX_T_PREC],[__mpfrx_struct],[prec],[unsigned-int])
NAU_MPFRCX_INSPECT_FIELD_TYPE([MPFRX_T_COEFF],[__mpfrx_struct],[coeff],[pointer])

NAU_MPFRCX_INSPECT_FIELD_TYPE([MPCX_T_SIZE],[__mpcx_struct],[size],[signed-int])
NAU_MPFRCX_INSPECT_FIELD_TYPE([MPCX_T_DEG],[__mpcx_struct],[deg],[signed-int])
NAU_MPFRCX_INSPECT_FIELD_TYPE([MPCX_T_PREC],[__mpcx_struct],[prec],[unsigned-int])
NAU_MPFRCX_INSPECT_FIELD_TYPE([MPCX_T_COEFF],[__mpcx_struct],[coeff],[pointer])

NAUSICAA_ENABLE_TESTS

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
