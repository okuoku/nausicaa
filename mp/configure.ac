# configure.ac --
#

AC_INIT([nausicaa-mp],[0.1d0])
AC_PREREQ(2.60)
AC_CONFIG_SRCDIR([libraries/compile-all.sps])
NAUSICAA_BEGIN()
DS_IKARUS_CHECK_LIBRARY([URIEL_FFI],[(uriel ffi)])
NAUSICAA_SYSTEM_SETUP()
NAUSICAA_C_LANGUAGE()

dnl --------------------------------------------------------------------

AC_CHECK_LIB([gmp],[__gmpz_init],,[AC_MSG_FAILURE([missing GMP library],[2])])
AC_CHECK_HEADER([gmp.h],,[AC_MSG_FAILURE([missing gmp.h header],[2])])

AC_COMPUTE_INT([SIZEOF_MPZ_T],[sizeof(mpz_t)],[#include <gmp.h>])
AC_SUBST(SIZEOF_MPZ_T)

AC_COMPUTE_INT([SIZEOF_MPF_T],[sizeof(mpf_t)],[#include <gmp.h>])
AC_SUBST(SIZEOF_MPF_T)

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
