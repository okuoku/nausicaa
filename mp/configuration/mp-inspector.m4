dnl (foreign math mp sizeof) --
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

AC_DEFUN([NAUSICAA_MP],[

NAUSICAA_INSPECT_TYPE([GMP_RANDALG_T],[gmp_randalg_t],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([MP_EXP_T],[mp_exp_t],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([MP_LIMB_T],[mp_limb_t],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([MP_SIZE_T],[mp_size_t],[signed-int],[#f])

dnl Struct inspection: mpf_t
NAUSICAA_INSPECT_STRUCT_TYPE([MPF_T],[mpf_t],[#f])

dnl Struct inspection: mpq_t
NAUSICAA_INSPECT_STRUCT_TYPE([MPQ_T],[mpq_t],[#f])

dnl Struct inspection: mpz_t
NAUSICAA_INSPECT_STRUCT_TYPE([MPZ_T],[mpz_t],[#f])

dnl Struct inspection: gmp_randstate_t
NAUSICAA_INSPECT_STRUCT_TYPE([GMP_RANDSTATE_T],[gmp_randstate_t],[#f])

NAUSICAA_OFFSETOF_FIELD_TEST([MPQ_STRUCT_NUM],[__mpq_struct],[_mp_num])
NAUSICAA_OFFSETOF_FIELD_TEST([MPQ_STRUCT_DEN],[__mpq_struct],[_mp_den])


dnl Preprocessor symbols: GMP constants
NAUSICAA_DEFINE_VALUE([GMP_RAND_ALG_DEFAULT])
NAUSICAA_DEFINE_VALUE([GMP_RAND_ALG_LC])

if test "${nau_ENABLE_MPFR}" = no
then
  NAUSICAA_DISABLE_TESTS
fi

NAUSICAA_INSPECT_TYPE([MP_RND_T],[mp_rnd_t],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([MP_PREC_T],[mp_prec_t],[unsigned-int],[#f])
NAUSICAA_INSPECT_TYPE([MPFR_PREC_T],[mpfr_prec_t],[unsigned-int],[#f])
NAUSICAA_INSPECT_TYPE([MPFR_SIGN_T],[mpfr_sign_t],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([MPFR_RND_T],[mpfr_rnd_t],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([INTMAX_T],[intmax_t],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([UINTMAX_T],[uintmax_t],[unsigned-int],[#f])

dnl Struct inspection: mpfr_t
NAUSICAA_INSPECT_STRUCT_TYPE([MPFR_T],[__mpfr_struct],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([MPFR_T__MPFR_PREC],[__mpfr_struct],[_mpfr_prec],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([MPFR_T__MPFR_SIGN],[__mpfr_struct],[_mpfr_sign],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([MPFR_T__MPFR_EXP],[__mpfr_struct],[_mpfr_exp],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([MPFR_T__MPFR_D],[__mpfr_struct],[_mpfr_d],[pointer])

dnl Preprocessor symbols: MPFR constants
NAUSICAA_DEFINE_VALUE([GMP_RNDD])
NAUSICAA_DEFINE_VALUE([GMP_RNDN])
NAUSICAA_DEFINE_VALUE([GMP_RNDU])
NAUSICAA_DEFINE_VALUE([GMP_RNDZ])
NAUSICAA_DEFINE_VALUE([MPFR_PREC_MAX])
NAUSICAA_DEFINE_VALUE([MPFR_PREC_MIN])
NAUSICAA_DEFINE_VALUE([MPFR_VERSION])
NAUSICAA_DEFINE_VALUE([MPFR_EMAX_DEFAULT])
NAUSICAA_DEFINE_VALUE([MPFR_EMIN_DEFAULT])
NAUSICAA_ENABLE_TESTS

if test "${nau_ENABLE_MPFI}" = no
then
  NAUSICAA_DISABLE_TESTS
fi


dnl Struct inspection: mpfi_t
NAUSICAA_INSPECT_STRUCT_TYPE([MPFI_T],[mpfi_t],[#f])

NAUSICAA_OFFSETOF_FIELD_TEST([MPFI_STRUCT_LEFT],[__mpfi_struct],[left])
NAUSICAA_OFFSETOF_FIELD_TEST([MPFI_STRUCT_RIGHT],[__mpfi_struct],[right])


dnl Preprocessor symbols: MPFI constants
NAUSICAA_DEFINE_VALUE([MPFI_FLAGS_BOTH_ENDPOINTS_EXACT])
NAUSICAA_DEFINE_VALUE([MPFI_FLAGS_LEFT_ENDPOINT_INEXACT])
NAUSICAA_DEFINE_VALUE([MPFI_FLAGS_RIGHT_ENDPOINT_INEXACT])
NAUSICAA_DEFINE_VALUE([MPFI_FLAGS_BOTH_ENDPOINTS_INEXACT])
NAUSICAA_ENABLE_TESTS

if test "${nau_ENABLE_MPC}" = no
then
  NAUSICAA_DISABLE_TESTS
fi


dnl Struct inspection: mpc_t
NAUSICAA_INSPECT_STRUCT_TYPE([MPC_T],[mpc_t],[#f])

NAUSICAA_OFFSETOF_FIELD_TEST([MPC_STRUCT_RE],[__mpc_struct],[re])
NAUSICAA_OFFSETOF_FIELD_TEST([MPC_STRUCT_IM],[__mpc_struct],[im])

NAUSICAA_INSPECT_TYPE([MPC_RND_T],[mpc_rnd_t],[signed-int],[#f])

dnl Preprocessor symbols: MPC constants
NAUSICAA_DEFINE_VALUE([MPC_VERSION])
NAUSICAA_ENABLE_TESTS

if test "${nau_ENABLE_MPFRCX}" = no
then
  NAUSICAA_DISABLE_TESTS
fi


dnl Preprocessor symbols: MPFRCX constants
NAUSICAA_DEFINE_VALUE([MPFRCX_VERSION])

dnl Struct inspection: mpfrx_t
NAUSICAA_INSPECT_STRUCT_TYPE([MPFRX_T],[__mpfrx_struct],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([MPFRX_T_SIZE],[__mpfrx_struct],[size],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([MPFRX_T_DEG],[__mpfrx_struct],[deg],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([MPFRX_T_PREC],[__mpfrx_struct],[prec],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([MPFRX_T_COEFF],[__mpfrx_struct],[coeff],[pointer])

dnl Struct inspection: mpcx_t
NAUSICAA_INSPECT_STRUCT_TYPE([MPCX_T],[__mpcx_struct],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([MPCX_T_SIZE],[__mpcx_struct],[size],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([MPCX_T_DEG],[__mpcx_struct],[deg],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([MPCX_T_PREC],[__mpcx_struct],[prec],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([MPCX_T_COEFF],[__mpcx_struct],[coeff],[pointer])
NAUSICAA_ENABLE_TESTS
NAU_DS_WITH_OPTION([GMP_SHARED_OBJECT],[gmp-shared-object],[libgmp.so],
  [Gmp shared library file],[select Gmp shared library file])
NAU_DS_WITH_OPTION([MPFR_SHARED_OBJECT],[mpfr-shared-object],[libmpfr.so],
  [Mpfr shared library file],[select Mpfr shared library file])
NAU_DS_WITH_OPTION([MPFI_SHARED_OBJECT],[mpfi-shared-object],[libmpfi.so],
  [Mpfi shared library file],[select Mpfi shared library file])
NAU_DS_WITH_OPTION([MPC_SHARED_OBJECT],[mpc-shared-object],[libmpc.so],
  [Mpc shared library file],[select Mpc shared library file])
NAU_DS_WITH_OPTION([MPFRCX_SHARED_OBJECT],[mpfrcx-shared-object],[libmpfrcx.so],
  [Mpfrcx shared library file],[select Mpfrcx shared library file])


])


dnl end of file
