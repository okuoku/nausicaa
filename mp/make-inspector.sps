;;;!mosh
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/MP
;;;Contents: foreign library inspection generator
;;;Date: Tue Dec  1, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


(import (nausicaa)
  (foreign ffi inspector-maker))


;;;; GMP

(define-c-type gmp_randalg_t		signed-int)
(define-c-type mp_exp_t			signed-int)
(define-c-type mp_limb_t		signed-int)
(define-c-type mp_size_t		signed-int)

(define-c-type-alias mpz_ptr		pointer)
(define-c-type-alias mpz_srcptr		pointer)
(define-c-type-alias mpq_ptr		pointer)
(define-c-type-alias mpq_srcptr		pointer)
(define-c-type-alias mpf_ptr		pointer)
(define-c-type-alias mpf_srcptr		pointer)
(define-c-type-alias gmp_randstate_t	pointer)
(define-c-type-alias mp_exp_t*		pointer)
(define-c-type-alias mp_prec_t*		pointer)

(define-c-struct mpf_t
  "mpf_t")

(define-c-struct mpq_t
  "mpq_t")

(define-c-struct mpz_t
  "mpz_t")

(define-c-struct gmp_randstate_t
  "gmp_randstate_t")

(autoconf-lib "
NAUSICAA_OFFSETOF_FIELD_TEST([MPQ_STRUCT_NUM],[__mpq_struct],[_mp_num])
NAUSICAA_OFFSETOF_FIELD_TEST([MPQ_STRUCT_DEN],[__mpq_struct],[_mp_den])
")

(sizeof-lib
 (define-syntax struct-mpq-num-ref
   (syntax-rules ()
     ((_ ?struct-pointer)
      (pointer-add ?struct-pointer ^OFFSETOF_MPQ_STRUCT_NUM^))))

 (define-syntax struct-mpq-den-ref
   (syntax-rules ()
     ((_ ?struct-pointer)
      (pointer-add ?struct-pointer ^OFFSETOF_MPQ_STRUCT_DEN^)))))

(sizeof-lib-exports
 struct-mpq-num-ref struct-mpq-den-ref)

(define-c-defines "GMP constants"
  GMP_RAND_ALG_DEFAULT
  GMP_RAND_ALG_LC)


;;;; MPFR

(autoconf-lib "
if test \"${nau_ENABLE_MPFR}\" = no
then
  NAUSICAA_DISABLE_TESTS
fi
")

(define-c-type-alias mpfr_ptr		pointer)
(define-c-type-alias mpfr_srcptr	pointer)

(define-c-type mp_rnd_t			signed-int)
(define-c-type mp_prec_t		unsigned-int)
(define-c-type mpfr_prec_t		unsigned-int)
(define-c-type mpfr_sign_t		signed-int)
(define-c-type mpfr_rnd_t		signed-int)

(define-c-type intmax_t			signed-int)
(define-c-type uintmax_t		unsigned-int)

(define-c-struct mpfr_t
  "__mpfr_struct"
  (unsigned-int		_mpfr_prec)
  (signed-int		_mpfr_sign)
  (signed-int		_mpfr_exp)
  (pointer		_mpfr_d))

(define-c-defines "MPFR constants"
  GMP_RNDD
  GMP_RNDN
  GMP_RNDU
  GMP_RNDZ
  MPFR_PREC_MAX
  MPFR_PREC_MIN
  MPFR_VERSION
  MPFR_EMAX_DEFAULT
  MPFR_EMIN_DEFAULT)

(autoconf-lib "NAUSICAA_ENABLE_TESTS")


;;;; MPFI

(autoconf-lib "
if test \"${nau_ENABLE_MPFI}\" = no
then
  NAUSICAA_DISABLE_TESTS
fi
")

(define-c-type-alias mpfi_ptr		pointer)
(define-c-type-alias mpfi_srcptr	pointer)

(define-c-struct mpfi_t
  "mpfi_t")

(autoconf-lib "
NAUSICAA_OFFSETOF_FIELD_TEST([MPFI_STRUCT_LEFT],[__mpfi_struct],[left])
NAUSICAA_OFFSETOF_FIELD_TEST([MPFI_STRUCT_RIGHT],[__mpfi_struct],[right])
")

(sizeof-lib
 (define-syntax struct-mpfi-left-ref
   (syntax-rules ()
     ((_ ?struct-pointer)
      (pointer-add ?struct-pointer ^OFFSETOF_MPFI_STRUCT_LEFT^))))

 (define-syntax struct-mpfi-right-ref
   (syntax-rules ()
     ((_ ?struct-pointer)
      (pointer-add ?struct-pointer ^OFFSETOF_MPFI_STRUCT_RIGHT^)))))

(sizeof-lib-exports
 struct-mpfi-left-ref struct-mpfi-right-ref)

(define-c-defines "MPFI constants"
  MPFI_FLAGS_BOTH_ENDPOINTS_EXACT
  MPFI_FLAGS_LEFT_ENDPOINT_INEXACT
  MPFI_FLAGS_RIGHT_ENDPOINT_INEXACT
  MPFI_FLAGS_BOTH_ENDPOINTS_INEXACT)

(autoconf-lib "NAUSICAA_ENABLE_TESTS")


;;;; MPC

(autoconf-lib "
if test \"${nau_ENABLE_MPC}\" = no
then
  NAUSICAA_DISABLE_TESTS
fi
")

(define-c-type-alias mpc_ptr		pointer)
(define-c-type-alias mpc_srcptr		pointer)

(define-c-struct mpc_t
  "mpc_t")

(autoconf-lib "
NAUSICAA_OFFSETOF_FIELD_TEST([MPC_STRUCT_RE],[__mpc_struct],[re])
NAUSICAA_OFFSETOF_FIELD_TEST([MPC_STRUCT_IM],[__mpc_struct],[im])
")

(sizeof-lib
 (define-syntax struct-mpc-re-ref
   (syntax-rules ()
     ((_ ?struct-pointer)
      (pointer-add ?struct-pointer ^OFFSETOF_MPC_STRUCT_RE^))))

 (define-syntax struct-mpc-im-ref
   (syntax-rules ()
     ((_ ?struct-pointer)
      (pointer-add ?struct-pointer ^OFFSETOF_MPC_STRUCT_IM^)))))

(sizeof-lib-exports
 struct-mpc-re-ref struct-mpc-im-ref)

(define-c-type mpc_rnd_t		signed-int)

(define-c-defines "MPC constants"
  MPC_VERSION)

(autoconf-lib "NAUSICAA_ENABLE_TESTS")


;;; MPFRCX

(autoconf-lib "
if test \"${nau_ENABLE_MPFRCX}\" = no
then
  NAUSICAA_DISABLE_TESTS
fi
")

(define-c-type-alias mpfrx_ptr		pointer)
(define-c-type-alias mpfrx_srcptr	pointer)
(define-c-type-alias mpcx_ptr		pointer)
(define-c-type-alias mpcx_srcptr	pointer)

(define-c-defines "MPFRCX constants"
  MPFRCX_VERSION)

(define-c-struct mpfrx_t
  "__mpfrx_struct"
  (signed-int		size)
  (signed-int		deg)
  (unsigned-int		prec)
  (pointer		coeff))

(define-c-struct mpcx_t
  "__mpcx_struct"
  (signed-int		size)
  (signed-int		deg)
  (unsigned-int		prec)
  (pointer		coeff))

(autoconf-lib "NAUSICAA_ENABLE_TESTS")


;;;; done

(define mp-library-spec
  '(foreign math mp sizeof))

(define-shared-object gmp	libgmp.so)
(define-shared-object mpfr	libmpfr.so)
(define-shared-object mpfi	libmpfi.so)
(define-shared-object mpc	libmpc.so)
(define-shared-object mpfrcx	libmpfrcx.so)

(autoconf-lib-write "configuration/mp-inspector.m4" mp-library-spec
		    "NAUSICAA_MP")
(sizeof-lib-write   "src/libraries/foreign/math/mp/sizeof.sls.in" mp-library-spec)

;;; end of file
