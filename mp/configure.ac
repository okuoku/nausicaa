dnl
dnl Part of: Nausicaa/IrRegex
dnl Contents: configuration template file
dnl Date: Sun Dec  7, 2008
dnl Time-stamp: <2008-12-07 18:24:10 marco>
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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

AC_INIT([nausicaa-mp],[0.1d0])
AC_PREREQ(2.60)
AC_CONFIG_SRCDIR([libraries/compile-all.sps])
NAUSICAA_BEGIN
NAUSICAA_IKARUS_CHECK_LIBRARY([URIEL_FFI],[(uriel ffi)])
NAUSICAA_YPSILON_CHECK_LIBRARY([URIEL_FFI],[(uriel ffi)])
NAUSICAA_SYSTEM_SETUP
NAUSICAA_C_LANGUAGE
NAUSICAA_SIZEOF


dnl page
dnl --------------------------------------------------------------------
dnl Helper functions.
dnl --------------------------------------------------------------------

AC_DEFUN([NAU_GMP_SIZEOF_TEST],
   [NAUSICAA_SIZEOF_TEST([$1],[$2],,[#include <gmp.h>])])

AC_DEFUN([NAU_GMP_VALUEOF_TEST],
   [NAUSICAA_VALUEOF_TEST([$1],[$2],,[#include <gmp.h>])])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Checking GMP.
dnl --------------------------------------------------------------------

AC_CHECK_LIB([gmp],[__gmpz_init],,
  [AC_MSG_FAILURE([missing GMP library],[2])])
AC_CHECK_HEADER([gmp.h],,
  [AC_MSG_FAILURE([missing gmp.h header],[2])])

NAU_GMP_SIZEOF_TEST([MP_EXP_T],[mp_exp_t])
NAU_GMP_SIZEOF_TEST([MP_SIZE_T],[mp_size_t])
NAU_GMP_SIZEOF_TEST([MP_LIMB_T],[mp_limb_t])
NAU_GMP_SIZEOF_TEST([MPQ_T],[mpq_t])
NAU_GMP_SIZEOF_TEST([MPZ_T],[mpz_t])
NAU_GMP_SIZEOF_TEST([MPF_T],[mpf_t])
NAU_GMP_SIZEOF_TEST([GMP_RANDALG_T],[gmp_randalg_t])
NAU_GMP_SIZEOF_TEST([GMP_RANDSTATE_T],[gmp_randstate_t])

NAUSICAA_INTTYPE_TEST([MP_EXP_T],[mp_exp_t])
NAUSICAA_INTTYPE_TEST([MP_SIZE_T],[mp_size_t])
NAUSICAA_INTTYPE_TEST([MP_LIMB_T],[mp_limb_t])
NAUSICAA_INTTYPE_TEST([GMP_RANDALG_T],[gmp_randalg_t])

NAU_GMP_VALUEOF_TEST([GMP_RAND_ALG_DEFAULT],[GMP_RAND_ALG_DEFAULT])
NAU_GMP_VALUEOF_TEST([GMP_RAND_ALG_LC],[GMP_RAND_ALG_LC])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Checking MPFR.
dnl --------------------------------------------------------------------

NAUSICAA_ENABLE_OPTION([nau_ENABLE_MPFR],[mpfr],[yes],
        [whether the MPFR interface will be used],
        [disable MPFR interface])

if test "${nau_ENABLE_MPFR}" = yes ; then

AC_CHECK_LIB([mpfr],[mpfr_init],,[AC_MSG_FAILURE([missing or unusable MPFR library],[2])])
AC_CHECK_HEADER([mpfr.h],,[AC_MSG_FAILURE([missing mpfr.h header],[2])])

AC_COMPUTE_INT([SIZEOF_MPFR_T],[sizeof(mpfr_t)],[#include <mpfr.h>])
AC_SUBST(SIZEOF_MPFR_T)

fi

dnl --------------------------------------------------------------------

NAUSICAA_ENABLE_OPTION([nau_ENABLE_MPFI],[mpfi],[yes],
        [whether the MPFI interface will be used],
        [disable MPFI interface])

if test "${nau_ENABLE_MPFI}" = yes -a "${nau_ENABLE_MPFR}" = yes ; then

AC_CHECK_HEADER([mpfi.h],,[AC_MSG_FAILURE([missing mpfr.h header],[2])])
AC_CHECK_LIB([mpfi],[mpfi_init],,[AC_MSG_FAILURE([missing or unusable MPFI library],[2])])

AC_COMPUTE_INT([SIZEOF_MPFI_T],[sizeof(mpfi_t)],[#include <mpfi.h>])
AC_SUBST(SIZEOF_MPFI_T)

fi

dnl --------------------------------------------------------------------

NAUSICAA_ENABLE_OPTION([nau_ENABLE_MPC],[mpc],[yes],
        [whether the MPC interface will be used],
        [disable MPC interface])

if test "${nau_ENABLE_MPC}" = yes -a "${nau_ENABLE_MPFR}" = yes ; then

AC_CHECK_LIB([mpc],[mpc_init],,[AC_MSG_FAILURE([missing or unusable MPC library],[2])])
AC_CHECK_HEADER([mpc.h],,[AC_MSG_FAILURE([missing mpc.h header],[2])])

AC_COMPUTE_INT([SIZEOF_MPC_T],[sizeof(mpc_t)],[#include <mpc.h>])
AC_SUBST(SIZEOF_MPC_T)

fi

dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../libraries/mp/sizeof.sls:libraries/mp/sizeof.sls.in])
NAUSICAA_END


### end of file
