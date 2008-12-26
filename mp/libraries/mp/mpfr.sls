;;;
;;;Part of: Nausicaa/MP
;;;Contents: interface to the MPFR library
;;;Date: Wed Dec 10, 2008
;;;Time-stamp: <2008-12-26 22:16:12 marco>
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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



;;;; setup

(library (mp mpfr)
  (export
    mpfr_abs
    mpfr_acos
    mpfr_acosh
    mpfr_add
    mpfr_add_q
    mpfr_add_si
    mpfr_add_ui
    mpfr_add_z
    mpfr_agm
    mpfr_asin
    mpfr_asinh
    mpfr_atan
    mpfr_atan2
    mpfr_atanh
    mpfr_can_round
    mpfr_cbrt
    mpfr_ceil
    mpfr_check_range
    mpfr_clear
    mpfr_clear_erangeflag
    mpfr_clear_flags
    mpfr_clear_inexflag
    mpfr_clear_nanflag
    mpfr_clear_overflow
    mpfr_clear_underflow
    mpfr_cmp
    mpfr_cmp3
    mpfr_cmp_d
    mpfr_cmp_f
    mpfr_cmp_q
    mpfr_cmp_si
    mpfr_cmp_si_2exp
    mpfr_cmp_ui
    mpfr_cmp_ui_2exp
    mpfr_cmp_z
    mpfr_cmpabs
    mpfr_const_catalan
    mpfr_const_euler
    mpfr_const_log2
    mpfr_const_pi
    mpfr_copysign
    mpfr_cos
    mpfr_cosh
    mpfr_cot
    mpfr_coth
    mpfr_csc
    mpfr_csch
    mpfr_custom_get_exp
    mpfr_custom_get_kind
    mpfr_custom_get_mantissa
    mpfr_custom_get_size
    mpfr_custom_init
    mpfr_custom_init_set
    mpfr_custom_move
    mpfr_dim
    mpfr_div
    mpfr_div_2exp
    mpfr_div_2si
    mpfr_div_2ui
    mpfr_div_q
    mpfr_div_si
    mpfr_div_ui
    mpfr_div_z
    mpfr_dump
    mpfr_eint
    mpfr_eq
    mpfr_equal_p
    mpfr_erangeflag_p
    mpfr_erf
    mpfr_erfc
    mpfr_exp
    mpfr_exp10
    mpfr_exp2
    mpfr_expm1
    mpfr_extract
    mpfr_fac_ui
;;;    mpfr_fits_intmax_p
    mpfr_fits_sint_p
    mpfr_fits_slong_p
    mpfr_fits_sshort_p
    mpfr_fits_uint_p
;;;    mpfr_fits_uintmax_p
    mpfr_fits_ulong_p
    mpfr_fits_ushort_p
    mpfr_floor
    mpfr_fma
    mpfr_fms
    mpfr_frac
    mpfr_free_cache
    mpfr_free_str
    mpfr_gamma
    mpfr_get_d
    mpfr_get_d1
    mpfr_get_d_2exp
    mpfr_get_default_prec
    mpfr_get_default_rounding_mode
    mpfr_get_emax
    mpfr_get_emax_max
    mpfr_get_emax_min
    mpfr_get_emin
    mpfr_get_emin_max
    mpfr_get_emin_min
    mpfr_get_exp
    mpfr_get_f
    mpfr_get_patches
    mpfr_get_prec
    mpfr_get_si
;;;    mpfr_get_sj
    mpfr_get_str
    mpfr_get_ui
;;;    mpfr_get_uj
    mpfr_get_version
    mpfr_get_z
    mpfr_get_z_exp
    mpfr_greater_p
    mpfr_greaterequal_p
    mpfr_hypot
    mpfr_inexflag_p
    mpfr_inf_p
    mpfr_init
    mpfr_init2
    mpfr_init_set_str
    mpfr_inp_str
    mpfr_integer_p
    mpfr_j0
    mpfr_j1
    mpfr_jn
    mpfr_less_p
    mpfr_lessequal_p
    mpfr_lessgreater_p
    mpfr_lgamma
    mpfr_lngamma
    mpfr_log
    mpfr_log10
    mpfr_log1p
    mpfr_log2
    mpfr_max
    mpfr_min
    mpfr_mul
    mpfr_mul_2exp
    mpfr_mul_2si
    mpfr_mul_2ui
    mpfr_mul_q
    mpfr_mul_si
    mpfr_mul_ui
    mpfr_mul_z
    mpfr_nan_p
    mpfr_nanflag_p
    mpfr_neg
    mpfr_nextabove
    mpfr_nextbelow
    mpfr_nexttoward
    mpfr_number_p
    mpfr_out_str
    mpfr_overflow_p
    mpfr_pow
    mpfr_pow_si
    mpfr_pow_ui
    mpfr_pow_z
    mpfr_prec_round
    mpfr_print_rnd_mode
    mpfr_random
    mpfr_random2
    mpfr_reldiff
    mpfr_remainder
    mpfr_remquo
    mpfr_rint
    mpfr_rint_ceil
    mpfr_rint_floor
    mpfr_rint_round
    mpfr_rint_trunc
    mpfr_root
    mpfr_round
    mpfr_sec
    mpfr_sech
    mpfr_set
    mpfr_set4
    mpfr_set_d
    mpfr_set_default_prec
    mpfr_set_default_rounding_mode
    mpfr_set_emax
    mpfr_set_emin
    mpfr_set_erangeflag
    mpfr_set_exp
    mpfr_set_f
    mpfr_set_inexflag
    mpfr_set_inf
    mpfr_set_nan
    mpfr_set_nanflag
    mpfr_set_overflow
    mpfr_set_prec
    mpfr_set_prec_raw
    mpfr_set_q
    mpfr_set_si
    mpfr_set_si_2exp
;;;    mpfr_set_sj
;;;    mpfr_set_sj_2exp
    mpfr_set_str
    mpfr_set_ui
    mpfr_set_ui_2exp
;;;    mpfr_set_uj
;;;    mpfr_set_uj_2exp
    mpfr_set_underflow
    mpfr_set_z
    mpfr_setsign
    mpfr_sgn
    mpfr_si_div
    mpfr_si_sub
    mpfr_signbit
    mpfr_sin
    mpfr_sin_cos
    mpfr_sinh
    mpfr_sqr
    mpfr_sqrt
    mpfr_sqrt_ui
    mpfr_strtofr
    mpfr_sub
    mpfr_sub_q
    mpfr_sub_si
    mpfr_sub_ui
    mpfr_sub_z
    mpfr_subnormalize
    mpfr_sum
    mpfr_swap
    mpfr_tan
    mpfr_tanh
    mpfr_trunc
    mpfr_ui_div
    mpfr_ui_pow
    mpfr_ui_pow_ui
    mpfr_ui_sub
    mpfr_underflow_p
    mpfr_unordered_p
    mpfr_urandomb
    mpfr_y0
    mpfr_y1
    mpfr_yn
    mpfr_zero_p
    mpfr_zeta
    mpfr_zeta_ui
;;; mpfr_cmp_ld
;;; mpfr_get_ld
;;; mpfr_get_ld_2exp
;;; mpfr_set_ld
    )
  (import (r6rs)
    (uriel ffi)
    (uriel ffi sizeof)
    (mp sizeof))

  (define mpfr-lib
    (let ((o (open-shared-object 'libmpfr.so)))
      (shared-object o)
      o))



;;;; functions

(define-c-function mpfr_get_version
  (char* mpfr_get_version (void)))
(define-c-function mpfr_get_patches
  (char* mpfr_get_patches (void)))

(define-c-function mpfr_get_emin
  (mp_exp_t mpfr_get_emin (void)))
(define-c-function mpfr_set_emin
  (int mpfr_set_emin (mp_exp_t)))
(define-c-function mpfr_get_emin_min
  (mp_exp_t mpfr_get_emin_min (void)))
(define-c-function mpfr_get_emin_max
  (mp_exp_t mpfr_get_emin_max (void)))
(define-c-function mpfr_get_emax
  (mp_exp_t mpfr_get_emax (void)))
(define-c-function mpfr_set_emax
  (int mpfr_set_emax (mp_exp_t)))
(define-c-function mpfr_get_emax_min
  (mp_exp_t mpfr_get_emax_min (void)))
(define-c-function mpfr_get_emax_max
  (mp_exp_t mpfr_get_emax_max (void)))

(define-c-function mpfr_set_default_rounding_mode
  (void mpfr_set_default_rounding_mode (mpfr_rnd_t)))
(define-c-function mpfr_get_default_rounding_mode
  (mp_rnd_t mpfr_get_default_rounding_mode (void)))
(define-c-function mpfr_print_rnd_mode
  (char* mpfr_print_rnd_mode (mpfr_rnd_t)))

(define-c-function mpfr_clear_flags
  (void mpfr_clear_flags (void)))
(define-c-function mpfr_clear_underflow
  (void mpfr_clear_underflow (void)))
(define-c-function mpfr_clear_overflow
  (void mpfr_clear_overflow (void)))
(define-c-function mpfr_clear_nanflag
  (void mpfr_clear_nanflag (void)))
(define-c-function mpfr_clear_inexflag
  (void mpfr_clear_inexflag (void)))
(define-c-function mpfr_clear_erangeflag
  (void mpfr_clear_erangeflag (void)))

(define-c-function mpfr_set_underflow
  (void mpfr_set_underflow (void)))
(define-c-function mpfr_set_overflow
  (void mpfr_set_overflow (void)))
(define-c-function mpfr_set_nanflag
  (void mpfr_set_nanflag (void)))
(define-c-function mpfr_set_inexflag
  (void mpfr_set_inexflag (void)))
(define-c-function mpfr_set_erangeflag
  (void mpfr_set_erangeflag (void)))

(define-c-function mpfr_underflow_p
  (int mpfr_underflow_p (void)))
(define-c-function mpfr_overflow_p
  (int mpfr_overflow_p (void)))
(define-c-function mpfr_nanflag_p
  (int mpfr_nanflag_p (void)))
(define-c-function mpfr_inexflag_p
  (int mpfr_inexflag_p (void)))
(define-c-function mpfr_erangeflag_p
  (int mpfr_erangeflag_p (void)))

(define-c-function mpfr_check_range
  (int mpfr_check_range (mpfr_ptr int mpfr_rnd_t)))

(define-c-function mpfr_init2
  (void mpfr_init2 (mpfr_ptr mpfr_prec_t)))
(define-c-function mpfr_init
  (void mpfr_init (mpfr_ptr)))
(define-c-function mpfr_clear
  (void mpfr_clear (mpfr_ptr)))

;; void mpfr_inits2 (mp_prec_t mpfr_ptr ...)
;; void mpfr_inits (mpfr_ptr ...)
;; void mpfr_clears (mpfr_ptr ...)

(define-c-function mpfr_prec_round
  (int mpfr_prec_round (mpfr_ptr mpfr_prec_t mpfr_rnd_t)))
(define-c-function mpfr_can_round
  (int mpfr_can_round (mpfr_srcptr mp_exp_t mpfr_rnd_t mpfr_rnd_t mpfr_prec_t)))

(define-c-function mpfr_get_exp
  (mp_exp_t mpfr_get_exp (mpfr_srcptr)))
(define-c-function mpfr_set_exp
  (int mpfr_set_exp (mpfr_ptr mp_exp_t)))
(define-c-function mpfr_get_prec
  (mp_prec_t mpfr_get_prec (mpfr_srcptr)))
(define-c-function mpfr_set_prec
  (void mpfr_set_prec (mpfr_ptr mpfr_prec_t)))
(define-c-function mpfr_set_prec_raw
  (void mpfr_set_prec_raw (mpfr_ptr mpfr_prec_t)))
(define-c-function mpfr_set_default_prec
  (void mpfr_set_default_prec (mpfr_prec_t)))
(define-c-function mpfr_get_default_prec
  (mp_prec_t mpfr_get_default_prec (void)))

(define-c-function mpfr_set_d
  (int mpfr_set_d (mpfr_ptr double mpfr_rnd_t)))

;; (define-c-function mpfr_set_ld
;;   (int mpfr_set_ld (mpfr_ptr long-double mpfr_rnd_t)))
(define-c-function mpfr_set_z
  (int mpfr_set_z (mpfr_ptr mpz_srcptr mpfr_rnd_t)))
(define-c-function mpfr_set_nan
  (void mpfr_set_nan (mpfr_ptr)))
(define-c-function mpfr_set_inf
  (void mpfr_set_inf (mpfr_ptr int)))
(define-c-function mpfr_set_f
  (int mpfr_set_f (mpfr_ptr mpf_srcptr mpfr_rnd_t)))
(define-c-function mpfr_get_f
  (int mpfr_get_f (mpf_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_set_si
  (int mpfr_set_si (mpfr_ptr long mpfr_rnd_t)))
(define-c-function mpfr_set_ui
  (int mpfr_set_ui (mpfr_ptr unsigned-long mpfr_rnd_t)))
(define-c-function mpfr_set_si_2exp
  (int mpfr_set_si_2exp (mpfr_ptr long mp_exp_t mpfr_rnd_t)))
(define-c-function mpfr_set_ui_2exp
  (int mpfr_set_ui_2exp (mpfr_ptr unsigned-long mp_exp_t mpfr_rnd_t)))
(define-c-function mpfr_set_q
  (int mpfr_set_q (mpfr_ptr mpq_srcptr mpfr_rnd_t)))
(define-c-function mpfr_set_str
  (int mpfr_set_str (mpfr_ptr char* int mpfr_rnd_t)))
(define-c-function mpfr_init_set_str
  (int mpfr_init_set_str (mpfr_ptr char* int mpfr_rnd_t)))
(define-c-function mpfr_set4
  (int mpfr_set4 (mpfr_ptr mpfr_srcptr mpfr_rnd_t int)))
(define-c-function mpfr_abs
  (int mpfr_abs (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_set
  (int mpfr_set (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_neg
  (int mpfr_neg (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_signbit
  (int mpfr_signbit (mpfr_srcptr)))
(define-c-function mpfr_setsign
  (int mpfr_setsign (mpfr_ptr mpfr_srcptr int mpfr_rnd_t)))
(define-c-function mpfr_copysign
  (int mpfr_copysign (mpfr_ptr mpfr_srcptr mpfr_srcptr mpfr_rnd_t)))

;;; The types "intmax_t" and "uintmax_t"  may turn out to be "long long"
;;; which are not supported (Wed Dec 10, 2008).
;; (define-c-function mpfr_set_sj
;;   (int __gmpfr_set_sj (mpfr_t intmax_t mpfr_rnd_t)))
;; (define-c-function mpfr_set_sj_2exp
;;   (int __gmpfr_set_sj_2exp (mpfr_t intmax_t intmax_t mpfr_rnd_t)))
;; (define-c-function mpfr_set_uj
;;   (int __gmpfr_set_uj (mpfr_t uintmax_t mpfr_rnd_t)))
;; (define-c-function mpfr_set_uj_2exp
;;   (int __gmpfr_set_uj_2exp (mpfr_t uintmax_t intmax_t mpfr_rnd_t)))
;; (define-c-function mpfr_get_sj
;;   (intmax_t __gmpfr_get_sj (mpfr_srcptr mpfr_rnd_t)))
;; (define-c-function mpfr_get_uj
;;   (uintmax_t __gmpfr_get_uj (mpfr_srcptr mpfr_rnd_t)))

(define-c-function mpfr_get_z_exp
  (mp_exp_t mpfr_get_z_exp (mpz_ptr mpfr_srcptr)))
(define-c-function mpfr_get_d
  (double mpfr_get_d (mpfr_srcptr mpfr_rnd_t)))
;; (define-c-function mpfr_get_ld
;;   (long-double mpfr_get_ld (mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_get_d1
  (double mpfr_get_d1 (mpfr_srcptr)))
(define-c-function mpfr_get_d_2exp
  (double mpfr_get_d_2exp (void* mpfr_srcptr mpfr_rnd_t)))
;; (define-c-function mpfr_get_ld_2exp
;;   (long-double mpfr_get_ld_2exp (void* mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_get_si
  (long mpfr_get_si (mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_get_ui
  (unsigned-long mpfr_get_ui (mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_get_str
  (char* mpfr_get_str (char* mp_exp_t* int size_t mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_get_z
  (void mpfr_get_z (mpz_ptr mpfr_srcptr mpfr_rnd_t)))

(define-c-function mpfr_free_str
  (void mpfr_free_str (char*)))

(define-c-function mpfr_random
  (void mpfr_random (mpfr_ptr)))
(define-c-function mpfr_random2
  (void mpfr_random2 (mpfr_ptr mp_size_t mp_exp_t)))
(define-c-function mpfr_urandomb
  (int mpfr_urandomb (mpfr_ptr gmp_randstate_t)))

(define-c-function mpfr_nextabove
  (void mpfr_nextabove (mpfr_ptr)))
(define-c-function mpfr_nextbelow
  (void mpfr_nextbelow (mpfr_ptr)))
(define-c-function mpfr_nexttoward
  (void mpfr_nexttoward (mpfr_ptr mpfr_srcptr)))

(define-c-function mpfr_inp_str
  (size_t __gmpfr_inp_str (mpfr_ptr FILE* int mpfr_rnd_t)))
(define-c-function mpfr_out_str
  (size_t __gmpfr_out_str (FILE* int size_t mpfr_srcptr mpfr_rnd_t)))

(define-c-function mpfr_pow
  (int mpfr_pow (mpfr_ptr mpfr_srcptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_pow_si
  (int mpfr_pow_si (mpfr_ptr mpfr_srcptr long int mpfr_rnd_t)))
(define-c-function mpfr_pow_ui
  (int mpfr_pow_ui (mpfr_ptr mpfr_srcptr unsigned-long int mpfr_rnd_t)))
(define-c-function mpfr_ui_pow_ui
  (int mpfr_ui_pow_ui (mpfr_ptr unsigned-long unsigned-long mpfr_rnd_t)))
(define-c-function mpfr_ui_pow
  (int mpfr_ui_pow (mpfr_ptr unsigned-long int mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_pow_z
  (int mpfr_pow_z (mpfr_ptr mpfr_srcptr mpz_srcptr mpfr_rnd_t)))

(define-c-function mpfr_sqrt
  (int mpfr_sqrt (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_sqrt_ui
  (int mpfr_sqrt_ui (mpfr_ptr unsigned-long mpfr_rnd_t)))

(define-c-function mpfr_add
  (int mpfr_add (mpfr_ptr mpfr_srcptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_sub
  (int mpfr_sub (mpfr_ptr mpfr_srcptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_mul
  (int mpfr_mul (mpfr_ptr mpfr_srcptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_div
  (int mpfr_div (mpfr_ptr mpfr_srcptr mpfr_srcptr mpfr_rnd_t)))

(define-c-function mpfr_add_ui
  (int mpfr_add_ui (mpfr_ptr mpfr_srcptr unsigned-long mpfr_rnd_t)))
(define-c-function mpfr_sub_ui
  (int mpfr_sub_ui (mpfr_ptr mpfr_srcptr unsigned-long mpfr_rnd_t)))
(define-c-function mpfr_ui_sub
  (int mpfr_ui_sub (mpfr_ptr unsigned-long mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_mul_ui
  (int mpfr_mul_ui (mpfr_ptr mpfr_srcptr unsigned-long mpfr_rnd_t)))
(define-c-function mpfr_div_ui
  (int mpfr_div_ui (mpfr_ptr mpfr_srcptr unsigned-long mpfr_rnd_t)))
(define-c-function mpfr_ui_div
  (int mpfr_ui_div (mpfr_ptr unsigned-long mpfr_srcptr mpfr_rnd_t)))

(define-c-function mpfr_add_si
  (int mpfr_add_si (mpfr_ptr mpfr_srcptr long int mpfr_rnd_t)))
(define-c-function mpfr_sub_si
  (int mpfr_sub_si (mpfr_ptr mpfr_srcptr long int mpfr_rnd_t)))
(define-c-function mpfr_si_sub
  (int mpfr_si_sub (mpfr_ptr long int mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_mul_si
  (int mpfr_mul_si (mpfr_ptr mpfr_srcptr long int mpfr_rnd_t)))
(define-c-function mpfr_div_si
  (int mpfr_div_si (mpfr_ptr mpfr_srcptr long int mpfr_rnd_t)))
(define-c-function mpfr_si_div
  (int mpfr_si_div (mpfr_ptr long int mpfr_srcptr mpfr_rnd_t)))

(define-c-function mpfr_sqr
  (int mpfr_sqr (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))

(define-c-function mpfr_const_pi
  (int mpfr_const_pi (mpfr_ptr mpfr_rnd_t)))
(define-c-function mpfr_const_log2
  (int mpfr_const_log2 (mpfr_ptr mpfr_rnd_t)))
(define-c-function mpfr_const_euler
  (int mpfr_const_euler (mpfr_ptr mpfr_rnd_t)))
(define-c-function mpfr_const_catalan
  (int mpfr_const_catalan (mpfr_ptr mpfr_rnd_t)))

(define-c-function mpfr_agm
  (int mpfr_agm (mpfr_ptr mpfr_srcptr mpfr_srcptr mpfr_rnd_t)))

(define-c-function mpfr_log
  (int mpfr_log (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_log2
  (int mpfr_log2 (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_log10
  (int mpfr_log10 (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_log1p
  (int mpfr_log1p (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))

(define-c-function mpfr_exp
  (int mpfr_exp (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_exp2
  (int mpfr_exp2 (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_exp10
  (int mpfr_exp10 (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_expm1
  (int mpfr_expm1 (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_eint
  (int mpfr_eint (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))

(define-c-function mpfr_cmp
  (int mpfr_cmp  (mpfr_srcptr mpfr_srcptr)))
(define-c-function mpfr_cmp3
  (int mpfr_cmp3 (mpfr_srcptr mpfr_srcptr int)))
(define-c-function mpfr_cmp_d
  (int mpfr_cmp_d (mpfr_srcptr double)))
;; (define-c-function mpfr_cmp_ld
;;   (int mpfr_cmp_ld (mpfr_srcptr long-double)))
(define-c-function mpfr_cmpabs
  (int mpfr_cmpabs (mpfr_srcptr mpfr_srcptr)))
(define-c-function mpfr_cmp_ui
  (int mpfr_cmp_ui (mpfr_srcptr unsigned-long)))
(define-c-function mpfr_cmp_si
  (int mpfr_cmp_si (mpfr_srcptr long)))
(define-c-function mpfr_cmp_ui_2exp
  (int mpfr_cmp_ui_2exp (mpfr_srcptr unsigned-long mp_exp_t)))
(define-c-function mpfr_cmp_si_2exp
  (int mpfr_cmp_si_2exp (mpfr_srcptr long mp_exp_t)))
(define-c-function mpfr_reldiff
  (void mpfr_reldiff (mpfr_ptr mpfr_srcptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_eq
  (int mpfr_eq (mpfr_srcptr mpfr_srcptr unsigned-long)))
(define-c-function mpfr_sgn
  (int mpfr_sgn (mpfr_srcptr)))

(define-c-function mpfr_mul_2exp
  (int mpfr_mul_2exp (mpfr_ptr mpfr_srcptr unsigned-long mpfr_rnd_t)))
(define-c-function mpfr_div_2exp
  (int mpfr_div_2exp (mpfr_ptr mpfr_srcptr unsigned-long mpfr_rnd_t)))
(define-c-function mpfr_mul_2ui
  (int mpfr_mul_2ui (mpfr_ptr mpfr_srcptr unsigned-long mpfr_rnd_t)))
(define-c-function mpfr_div_2ui
  (int mpfr_div_2ui (mpfr_ptr mpfr_srcptr unsigned-long mpfr_rnd_t)))
(define-c-function mpfr_mul_2si
  (int mpfr_mul_2si (mpfr_ptr mpfr_srcptr long mpfr_rnd_t)))
(define-c-function mpfr_div_2si
  (int mpfr_div_2si (mpfr_ptr mpfr_srcptr long mpfr_rnd_t)))

(define-c-function mpfr_rint
  (int mpfr_rint (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_round
  (int mpfr_round (mpfr_ptr mpfr_srcptr)))
(define-c-function mpfr_trunc
  (int mpfr_trunc (mpfr_ptr mpfr_srcptr)))
(define-c-function mpfr_ceil
  (int mpfr_ceil (mpfr_ptr mpfr_srcptr)))
(define-c-function mpfr_floor
  (int mpfr_floor (mpfr_ptr mpfr_srcptr)))
(define-c-function mpfr_rint_round
  (int mpfr_rint_round (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_rint_trunc
  (int mpfr_rint_trunc (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_rint_ceil
  (int mpfr_rint_ceil (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_rint_floor
  (int mpfr_rint_floor (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_frac
  (int mpfr_frac (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_remquo
  (int mpfr_remquo (mpfr_ptr void* mpfr_srcptr mpfr_srcptr mp_rnd_t)))
(define-c-function mpfr_remainder
  (int mpfr_remainder (mpfr_ptr mpfr_srcptr mpfr_srcptr mp_rnd_t)))

(define-c-function mpfr_fits_ulong_p
  (int mpfr_fits_ulong_p (mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_fits_slong_p
  (int mpfr_fits_slong_p (mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_fits_uint_p
  (int mpfr_fits_uint_p (mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_fits_sint_p
  (int mpfr_fits_sint_p (mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_fits_ushort_p
  (int mpfr_fits_ushort_p (mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_fits_sshort_p
  (int mpfr_fits_sshort_p (mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_fits_uintmax_p
  (int mpfr_fits_uintmax_p (mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_fits_intmax_p
  (int mpfr_fits_intmax_p (mpfr_srcptr mpfr_rnd_t)))

(define-c-function mpfr_extract
  (void mpfr_extract (mpz_ptr mpfr_srcptr unsigned int)))
(define-c-function mpfr_swap
  (void mpfr_swap (mpfr_ptr mpfr_ptr)))
(define-c-function mpfr_dump
  (void mpfr_dump (mpfr_srcptr)))

(define-c-function mpfr_nan_p
  (int mpfr_nan_p (mpfr_srcptr)))
(define-c-function mpfr_inf_p
  (int mpfr_inf_p (mpfr_srcptr)))
(define-c-function mpfr_number_p
  (int mpfr_number_p (mpfr_srcptr)))
(define-c-function mpfr_integer_p
  (int mpfr_integer_p (mpfr_srcptr)))
(define-c-function mpfr_zero_p
  (int mpfr_zero_p (mpfr_srcptr)))

(define-c-function mpfr_greater_p
  (int mpfr_greater_p (mpfr_srcptr mpfr_srcptr)))
(define-c-function mpfr_greaterequal_p
  (int mpfr_greaterequal_p (mpfr_srcptr mpfr_srcptr)))
(define-c-function mpfr_less_p
  (int mpfr_less_p (mpfr_srcptr mpfr_srcptr)))
(define-c-function mpfr_lessequal_p
  (int mpfr_lessequal_p (mpfr_srcptr mpfr_srcptr)))
(define-c-function mpfr_lessgreater_p
  (int mpfr_lessgreater_p (mpfr_srcptr mpfr_srcptr)))
(define-c-function mpfr_equal_p
  (int mpfr_equal_p (mpfr_srcptr mpfr_srcptr)))
(define-c-function mpfr_unordered_p
  (int mpfr_unordered_p (mpfr_srcptr mpfr_srcptr)))

(define-c-function mpfr_atanh
  (int mpfr_atanh (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_acosh
  (int mpfr_acosh (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_asinh
  (int mpfr_asinh (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_cosh
  (int mpfr_cosh (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_sinh
  (int mpfr_sinh (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_tanh
  (int mpfr_tanh (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))

(define-c-function mpfr_sech
  (int mpfr_sech (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_csch
  (int mpfr_csch (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_coth
  (int mpfr_coth (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))

(define-c-function mpfr_acos
  (int mpfr_acos (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_asin
  (int mpfr_asin (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_atan
  (int mpfr_atan (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_sin
  (int mpfr_sin (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_sin_cos
  (int mpfr_sin_cos (mpfr_ptr mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_cos
  (int mpfr_cos (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_tan
  (int mpfr_tan (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_atan2
  (int mpfr_atan2 (mpfr_ptr mpfr_srcptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_sec
  (int mpfr_sec (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_csc
  (int mpfr_csc (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_cot
  (int mpfr_cot (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))

(define-c-function mpfr_hypot
  (int mpfr_hypot (mpfr_ptr mpfr_srcptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_erf
  (int mpfr_erf (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_erfc
  (int mpfr_erfc (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_cbrt
  (int mpfr_cbrt (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_root
  (int mpfr_root (mpfr_ptr mpfr_srcptr unsigned-long mpfr_rnd_t)))
(define-c-function mpfr_gamma
  (int mpfr_gamma (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_lngamma
  (int mpfr_lngamma (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_lgamma
  (int mpfr_lgamma (mpfr_ptr pointer mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_zeta
  (int mpfr_zeta (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_zeta_ui
  (int mpfr_zeta_ui (mpfr_ptr unsigned-long mpfr_rnd_t)))
(define-c-function mpfr_fac_ui
  (int mpfr_fac_ui (mpfr_ptr unsigned-long mpfr_rnd_t)))
(define-c-function mpfr_j0
  (int mpfr_j0 (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_j1
  (int mpfr_j1 (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_jn
  (int mpfr_jn (mpfr_ptr long mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_y0
  (int mpfr_y0 (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_y1
  (int mpfr_y1 (mpfr_ptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_yn
  (int mpfr_yn (mpfr_ptr long mpfr_srcptr mpfr_rnd_t)))

(define-c-function mpfr_min
  (int mpfr_min (mpfr_ptr mpfr_srcptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_max
  (int mpfr_max (mpfr_ptr mpfr_srcptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_dim
  (int mpfr_dim (mpfr_ptr mpfr_srcptr mpfr_srcptr mpfr_rnd_t)))

(define-c-function mpfr_mul_z
  (int mpfr_mul_z (mpfr_ptr mpfr_srcptr mpz_srcptr mpfr_rnd_t)))
(define-c-function mpfr_div_z
  (int mpfr_div_z (mpfr_ptr mpfr_srcptr mpz_srcptr mpfr_rnd_t)))
(define-c-function mpfr_add_z
  (int mpfr_add_z (mpfr_ptr mpfr_srcptr mpz_srcptr mpfr_rnd_t)))
(define-c-function mpfr_sub_z
  (int mpfr_sub_z (mpfr_ptr mpfr_srcptr mpz_srcptr mpfr_rnd_t)))
(define-c-function mpfr_cmp_z
  (int mpfr_cmp_z (mpfr_srcptr mpz_srcptr)))

(define-c-function mpfr_mul_q
  (int mpfr_mul_q (mpfr_ptr mpfr_srcptr mpq_srcptr mpfr_rnd_t)))
(define-c-function mpfr_div_q
  (int mpfr_div_q (mpfr_ptr mpfr_srcptr mpq_srcptr mpfr_rnd_t)))
(define-c-function mpfr_add_q
  (int mpfr_add_q (mpfr_ptr mpfr_srcptr mpq_srcptr mpfr_rnd_t)))
(define-c-function mpfr_sub_q
  (int mpfr_sub_q (mpfr_ptr mpfr_srcptr mpq_srcptr mpfr_rnd_t)))
(define-c-function mpfr_cmp_q
  (int mpfr_cmp_q (mpfr_srcptr mpq_srcptr)))

(define-c-function mpfr_cmp_f
  (int mpfr_cmp_f (mpfr_srcptr mpf_srcptr)))

(define-c-function mpfr_fma
  (int mpfr_fma (mpfr_ptr mpfr_srcptr mpfr_srcptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_fms
  (int mpfr_fms (mpfr_ptr mpfr_srcptr mpfr_srcptr mpfr_srcptr mpfr_rnd_t)))
(define-c-function mpfr_sum
  (int mpfr_sum (mpfr_ptr mpfr_ptr unsigned-long mpfr_rnd_t)))

(define-c-function mpfr_free_cache
  (void mpfr_free_cache (void)))

(define-c-function mpfr_subnormalize
  (int mpfr_subnormalize (mpfr_ptr int mp_rnd_t)))

(define-c-function mpfr_strtofr
  (int mpfr_strtofr (mpfr_ptr char* pointer int mpfr_rnd_t)))

(define-c-function mpfr_custom_get_size
  (size_t mpfr_custom_get_size (mp_prec_t)))
(define-c-function mpfr_custom_init
  (void mpfr_custom_init (void* mp_prec_t)))
(define-c-function mpfr_custom_get_mantissa
  (void* mpfr_custom_get_mantissa (mpfr_srcptr)))
(define-c-function mpfr_custom_get_exp
  (mp_exp_t mpfr_custom_get_exp (mpfr_srcptr)))
(define-c-function mpfr_custom_move
  (void mpfr_custom_move (mpfr_ptr void*)))
(define-c-function mpfr_custom_init_set
  (void mpfr_custom_init_set (mpfr_ptr int mp_exp_t mp_prec_t void*)))
(define-c-function mpfr_custom_get_kind
  (int mpfr_custom_get_kind (mpfr_srcptr)))


;;;; done

)

;;; end of file
