# configure.ac --
#

AC_INIT([nausicaa-mp],[0.1d0])
AC_PREREQ(2.60)
AC_CONFIG_SRCDIR([libraries/compile-all.sps])
NAUSICAA_BEGIN()
DS_IKARUS_CHECK_LIBRARY([URIEL_FFI],[(uriel ffi)])
NAUSICAA_SYSTEM_SETUP()
NAUSICAA_C_LANGUAGE()
NAUSICAA_SIZEOF()

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Helper functions.
dnl --------------------------------------------------------------------

dnl Synopsis:
dnl
dnl   NAUGMP_SIZEOF_TEST(<SUFFIX>,<TYPEDEF>)
dnl
dnl Description:
dnl
dnl   A wrapper for "AC_COMPUTE_INT" that acquires the size of a C
dnl   language type.  The output variable "SIZEOF_<SUFFIX>" will be
dnl   set to the result.  If the test fails the value of the output
dnl   variable will be "#f".
dnl
AC_DEFUN([NAUGMP_SIZEOF_TEST],[
AC_MSG_CHECKING([the sizeof of '$2'])
AC_COMPUTE_INT([SIZEOF_$1],[sizeof($2)],[
#include <gmp.h>
],[SIZEOF_$1="#f"])
AC_SUBST([SIZEOF_$1])
AC_MSG_RESULT([${SIZEOF_$1}])
])

dnl Synopsis:
dnl
dnl   NAUGMP_VALUEOF_TEST(<SUFFIX>,<EXPR>)
dnl
dnl Description:
dnl
dnl   A wrapper for "AC_COMPUTE_INT" that acquires the value of a C
dnl   language expression, which must be an integer.  It is typically
dnl   used to acquire the value of a constant like "INT_MAX".
dnl
dnl   The output variable "VALUEOF_<SUFFIX>" is set to the result.
dnl   If the test fails: the value of the output variable will be
dnl   "#f".
dnl
AC_DEFUN([NAUGMP_VALUEOF_TEST],[
AC_MSG_CHECKING([the value of '$2'])
AC_COMPUTE_INT([VALUEOF_$1],[$2],[AC_INCLUDES_DEFAULT
#include <gmp.h>
],[VALUEOF_$1=#f])
AC_SUBST([VALUEOF_$1])
AC_MSG_RESULT([${VALUEOF_$1}])
])


dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Checking GMP.
dnl --------------------------------------------------------------------

AC_CHECK_LIB([gmp],[__gmpz_init],,
  [AC_MSG_FAILURE([missing GMP library],[2])])
AC_CHECK_HEADER([gmp.h],,
  [AC_MSG_FAILURE([missing gmp.h header],[2])])

NAUGMP_SIZEOF_TEST([MP_EXP_T],[mp_exp_t])
NAUGMP_SIZEOF_TEST([MP_SIZE_T],[mp_size_t])
NAUGMP_SIZEOF_TEST([MP_LIMB_T],[mp_limb_t])
NAUGMP_SIZEOF_TEST([MPQ_T],[mpq_t])
NAUGMP_SIZEOF_TEST([MPZ_T],[mpz_t])
NAUGMP_SIZEOF_TEST([MPF_T],[mpf_t])
NAUGMP_SIZEOF_TEST([GMP_RANDALG_T],[gmp_randalg_t])
NAUGMP_SIZEOF_TEST([GMP_RANDSTATE_T],[gmp_randstate_t])

NAUSICAA_INTTYPE_TEST([MP_EXP_T],[mp_exp_t])
NAUSICAA_INTTYPE_TEST([MP_SIZE_T],[mp_size_t])
NAUSICAA_INTTYPE_TEST([MP_LIMB_T],[mp_limb_t])
NAUSICAA_INTTYPE_TEST([GMP_RANDALG_T],[gmp_randalg_t])

NAUGMP_VALUEOF_TEST([GMP_RAND_ALG_DEFAULT],[GMP_RAND_ALG_DEFAULT])
NAUGMP_VALUEOF_TEST([GMP_RAND_ALG_LC],[GMP_RAND_ALG_LC])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Checking MPFR.
dnl --------------------------------------------------------------------


DS_ENABLE_OPTION([nau_ENABLE_MPFR],[mpfr],[yes],
        [whether the MPFR interface will be used],
        [disable MPFR interface])

if test "${nau_ENABLE_MPFR}" = yes ; then

AC_CHECK_LIB([mpfr],[mpfr_init],,[AC_MSG_FAILURE([missing or unusable MPFR library],[2])])
AC_CHECK_HEADER([mpfr.h],,[AC_MSG_FAILURE([missing mpfr.h header],[2])])

AC_COMPUTE_INT([SIZEOF_MPFR_T],[sizeof(mpfr_t)],[#include <mpfr.h>])
AC_SUBST(SIZEOF_MPFR_T)

fi

dnl --------------------------------------------------------------------

DS_ENABLE_OPTION([nau_ENABLE_MPFI],[mpfi],[yes],
        [whether the MPFI interface will be used],
        [disable MPFI interface])

if test "${nau_ENABLE_MPFI}" = yes -a "${nau_ENABLE_MPFR}" = yes ; then

AC_CHECK_HEADER([mpfi.h],,[AC_MSG_FAILURE([missing mpfr.h header],[2])])
AC_CHECK_LIB([mpfi],[mpfi_init],,[AC_MSG_FAILURE([missing or unusable MPFI library],[2])])

AC_COMPUTE_INT([SIZEOF_MPFI_T],[sizeof(mpfi_t)],[#include <mpfi.h>])
AC_SUBST(SIZEOF_MPFI_T)

fi

dnl --------------------------------------------------------------------

DS_ENABLE_OPTION([nau_ENABLE_MPC],[mpc],[yes],
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
NAUSICAA_END()


### end of file
