;;;
;;;Part of: Nausicaa/MP
;;;Contents: interface to MPFI numbers
;;;Date: Wed Dec 10, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (foreign math mp mpfi)
  (export
    mpfi_round_prec
    mpfi_init
    mpfi_init2
    mpfi_clear
    mpfi_get_prec
    mpfi_set_prec
    mpfi_set
    mpfi_set_si
    mpfi_set_ui
    mpfi_set_d
    mpfi_set_z
    mpfi_set_q
    mpfi_set_fr
    mpfi_set_str
    mpfi_init_set
    mpfi_init_set_si
    mpfi_init_set_ui
    mpfi_init_set_d
    mpfi_init_set_z
    mpfi_init_set_q
    mpfi_init_set_fr
    mpfi_init_set_str
    mpfi_swap
    mpfi_diam_abs
    mpfi_diam_rel
    mpfi_diam
    mpfi_mag
    mpfi_mig
    mpfi_mid
    mpfi_alea
    mpfi_get_d
    mpfi_get_fr
    mpfi_add
    mpfi_sub
    mpfi_mul
    mpfi_div
    mpfi_add_d
    mpfi_sub_d
    mpfi_d_sub
    mpfi_mul_d
    mpfi_div_d
    mpfi_d_div
    mpfi_add_ui
    mpfi_sub_ui
    mpfi_ui_sub
    mpfi_mul_ui
    mpfi_div_ui
    mpfi_ui_div
    mpfi_add_si
    mpfi_sub_si
    mpfi_si_sub
    mpfi_mul_si
    mpfi_div_si
    mpfi_si_div
    mpfi_add_z
    mpfi_sub_z
    mpfi_z_sub
    mpfi_mul_z
    mpfi_div_z
    mpfi_z_div
    mpfi_add_q
    mpfi_sub_q
    mpfi_q_sub
    mpfi_mul_q
    mpfi_div_q
    mpfi_q_div
    mpfi_add_fr
    mpfi_sub_fr
    mpfi_fr_sub
    mpfi_mul_fr
    mpfi_div_fr
    mpfi_fr_div
    mpfi_neg
    mpfi_sqr
    mpfi_inv
    mpfi_sqrt
    mpfi_abs
    mpfi_mul_2exp
    mpfi_mul_2ui
    mpfi_mul_2si
    mpfi_div_2exp
    mpfi_div_2ui
    mpfi_div_2si
    mpfi_log
    mpfi_exp
    mpfi_exp2
    mpfi_cos
    mpfi_sin
    mpfi_tan
    mpfi_acos
    mpfi_asin
    mpfi_atan
    mpfi_cosh
    mpfi_sinh
    mpfi_tanh
    mpfi_acosh
    mpfi_asinh
    mpfi_atanh
    mpfi_log1p
    mpfi_expm1
    mpfi_log2
    mpfi_log10
    mpfi_const_log2
    mpfi_const_pi
    mpfi_const_euler
    mpfi_is_pos_default
    mpfi_is_nonneg_default
    mpfi_is_neg_default
    mpfi_is_nonpos_default
    mpfi_is_zero_default
    mpfi_is_strictly_neg_default
    mpfi_is_strictly_pos_default
    mpfi_cmp_default
    mpfi_cmp_d_default
    mpfi_cmp_ui_default
    mpfi_cmp_si_default
    mpfi_cmp_z_default
    mpfi_cmp_q_default
    mpfi_cmp_fr_default
    mpfi_has_zero
    mpfi_nan_p
    mpfi_inf_p
    mpfi_bounded_p
    mpfi_get_left
    mpfi_get_right
    mpfi_revert_if_needed
    mpfi_put
    mpfi_put_d
    mpfi_put_si
    mpfi_put_ui
    mpfi_put_z
    mpfi_put_q
    mpfi_put_fr
    mpfi_interv_d
    mpfi_interv_si
    mpfi_interv_ui
    mpfi_interv_z
    mpfi_interv_q
    mpfi_interv_fr
    mpfi_is_strictly_inside
    mpfi_is_inside
    mpfi_is_inside_d
    mpfi_is_inside_ui
    mpfi_is_inside_si
    mpfi_is_inside_z
    mpfi_is_inside_q
    mpfi_is_inside_fr
    mpfi_is_empty
    mpfi_intersect
    mpfi_union
    mpfi_increase
    mpfi_blow
    mpfi_bisect
    mpfi_get_version
    mpfi_reset_error
    mpfi_set_error
    mpfi_is_error)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign math mp sizeof))

  (define-shared-object mpfi-shared-object
    MPFI_SHARED_OBJECT)


;;;; functions

(define-c-functions mpfi-shared-object
  (mpfi_round_prec
   (int mpfi_round_prec (mpfi_ptr mp_prec_t)))
  (mpfi_init
   (void mpfi_init (mpfi_ptr)))
  (mpfi_init2
   (void mpfi_init2 (mpfi_ptr mp_prec_t)))
  (mpfi_clear
   (void mpfi_clear (mpfi_ptr)))
  (mpfi_get_prec
   (mp_prec_t mpfi_get_prec (mpfi_srcptr)))
  (mpfi_set_prec
   (void mpfi_set_prec (mpfi_ptr mp_prec_t)))

  (mpfi_set
   (int mpfi_set (mpfi_ptr mpfi_srcptr)))
  (mpfi_set_si
   (int mpfi_set_si (mpfi_ptr long)))
  (mpfi_set_ui
   (int mpfi_set_ui (mpfi_ptr unsigned-long)))
  (mpfi_set_d
   (int mpfi_set_d (mpfi_ptr double)))
  (mpfi_set_z
   (int mpfi_set_z (mpfi_ptr mpz_srcptr)))
  (mpfi_set_q
   (int mpfi_set_q (mpfi_ptr mpq_srcptr)))
  (mpfi_set_fr
   (int mpfi_set_fr (mpfi_ptr mpfr_srcptr)))
  (mpfi_set_str
   (int mpfi_set_str (mpfi_ptr char* int)))

  (mpfi_init_set
   (int mpfi_init_set (mpfi_ptr mpfi_srcptr)))
  (mpfi_init_set_si
   (int mpfi_init_set_si (mpfi_ptr long)))
  (mpfi_init_set_ui
   (int mpfi_init_set_ui (mpfi_ptr unsigned-long)))
  (mpfi_init_set_d
   (int mpfi_init_set_d (mpfi_ptr double)))
  (mpfi_init_set_z
   (int mpfi_init_set_z (mpfi_ptr mpz_srcptr)))
  (mpfi_init_set_q
   (int mpfi_init_set_q (mpfi_ptr mpq_srcptr)))
  (mpfi_init_set_fr
   (int mpfi_init_set_fr (mpfi_ptr mpfr_srcptr)))
  (mpfi_init_set_str
   (int mpfi_init_set_str (mpfi_ptr char* int)))

  (mpfi_swap
   (void mpfi_swap (mpfi_ptr mpfi_ptr)))

  (mpfi_diam_abs
   (int mpfi_diam_abs (mpfr_ptr mpfi_srcptr)))
  (mpfi_diam_rel
   (int mpfi_diam_rel (mpfr_ptr mpfi_srcptr)))
  (mpfi_diam
   (int mpfi_diam (mpfr_ptr mpfi_srcptr)))
  (mpfi_mag
   (int mpfi_mag (mpfr_ptr mpfi_srcptr)))
  (mpfi_mig
   (int mpfi_mig (mpfr_ptr mpfi_srcptr)))
  (mpfi_mid
   (int mpfi_mid (mpfr_ptr mpfi_srcptr)))
  (mpfi_alea
   (void mpfi_alea (mpfr_ptr mpfi_srcptr)))

  (mpfi_get_d
   (double mpfi_get_d (mpfi_srcptr)))
  (mpfi_get_fr
   (void mpfi_get_fr (mpfr_ptr mpfi_srcptr)))

  (mpfi_add
   (int mpfi_add (mpfi_ptr mpfi_srcptr mpfi_srcptr)))
  (mpfi_sub
   (int mpfi_sub (mpfi_ptr mpfi_srcptr mpfi_srcptr)))
  (mpfi_mul
   (int mpfi_mul (mpfi_ptr mpfi_srcptr mpfi_srcptr)))
  (mpfi_div
   (int mpfi_div (mpfi_ptr mpfi_srcptr mpfi_srcptr)))

  (mpfi_add_d
   (int mpfi_add_d (mpfi_ptr mpfi_srcptr double)))
  (mpfi_sub_d
   (int mpfi_sub_d (mpfi_ptr mpfi_srcptr double)))
  (mpfi_d_sub
   (int mpfi_d_sub (mpfi_ptr double mpfi_srcptr)))
  (mpfi_mul_d
   (int mpfi_mul_d (mpfi_ptr mpfi_srcptr double)))
  (mpfi_div_d
   (int mpfi_div_d (mpfi_ptr mpfi_srcptr double)))
  (mpfi_d_div
   (int mpfi_d_div (mpfi_ptr double mpfi_srcptr)))

  (mpfi_add_ui
   (int mpfi_add_ui (mpfi_ptr mpfi_srcptr unsigned-long)))
  (mpfi_sub_ui
   (int mpfi_sub_ui (mpfi_ptr mpfi_srcptr unsigned-long)))
  (mpfi_ui_sub
   (int mpfi_ui_sub (mpfi_ptr unsigned-long mpfi_srcptr)))
  (mpfi_mul_ui
   (int mpfi_mul_ui (mpfi_ptr mpfi_srcptr unsigned-long)))
  (mpfi_div_ui
   (int mpfi_div_ui (mpfi_ptr mpfi_srcptr unsigned-long)))
  (mpfi_ui_div
   (int mpfi_ui_div (mpfi_ptr unsigned-long mpfi_srcptr)))

  (mpfi_add_si
   (int mpfi_add_si (mpfi_ptr mpfi_srcptr long)))
  (mpfi_sub_si
   (int mpfi_sub_si (mpfi_ptr mpfi_srcptr long)))
  (mpfi_si_sub
   (int mpfi_si_sub (mpfi_ptr long mpfi_srcptr)))
  (mpfi_mul_si
   (int mpfi_mul_si (mpfi_ptr mpfi_srcptr long)))
  (mpfi_div_si
   (int mpfi_div_si (mpfi_ptr mpfi_srcptr long)))
  (mpfi_si_div
   (int mpfi_si_div (mpfi_ptr long mpfi_srcptr)))

  (mpfi_add_z
   (int mpfi_add_z (mpfi_ptr mpfi_srcptr mpz_srcptr)))
  (mpfi_sub_z
   (int mpfi_sub_z (mpfi_ptr mpfi_srcptr mpz_srcptr)))
  (mpfi_z_sub
   (int mpfi_z_sub (mpfi_ptr mpz_srcptr mpfi_srcptr)))
  (mpfi_mul_z
   (int mpfi_mul_z (mpfi_ptr mpfi_srcptr mpz_srcptr)))
  (mpfi_div_z
   (int mpfi_div_z (mpfi_ptr mpfi_srcptr mpz_srcptr)))
  (mpfi_z_div
   (int mpfi_z_div (mpfi_ptr mpz_srcptr mpfi_srcptr)))

  (mpfi_add_q
   (int mpfi_add_q (mpfi_ptr mpfi_srcptr mpq_srcptr)))
  (mpfi_sub_q
   (int mpfi_sub_q (mpfi_ptr mpfi_srcptr mpq_srcptr)))
  (mpfi_q_sub
   (int mpfi_q_sub (mpfi_ptr mpq_srcptr mpfi_srcptr)))
  (mpfi_mul_q
   (int mpfi_mul_q (mpfi_ptr mpfi_srcptr mpq_srcptr)))
  (mpfi_div_q
   (int mpfi_div_q (mpfi_ptr mpfi_srcptr mpq_srcptr)))
  (mpfi_q_div
   (int mpfi_q_div (mpfi_ptr mpq_srcptr mpfi_srcptr)))

  (mpfi_add_fr
   (int mpfi_add_fr (mpfi_ptr mpfi_srcptr mpfr_srcptr)))
  (mpfi_sub_fr
   (int mpfi_sub_fr (mpfi_ptr mpfi_srcptr mpfr_srcptr)))
  (mpfi_fr_sub
   (int mpfi_fr_sub (mpfi_ptr mpfr_srcptr mpfi_srcptr)))
  (mpfi_mul_fr
   (int mpfi_mul_fr (mpfi_ptr mpfi_srcptr mpfr_srcptr)))
  (mpfi_div_fr
   (int mpfi_div_fr (mpfi_ptr mpfi_srcptr mpfr_srcptr)))
  (mpfi_fr_div
   (int mpfi_fr_div (mpfi_ptr mpfr_srcptr mpfi_srcptr)))

  (mpfi_neg
   (int mpfi_neg (mpfi_ptr mpfi_srcptr)))
  (mpfi_sqr
   (int mpfi_sqr (mpfi_ptr mpfi_srcptr)))
  (mpfi_inv
   (int mpfi_inv (mpfi_ptr mpfi_srcptr)))
  (mpfi_sqrt
   (int mpfi_sqrt (mpfi_ptr mpfi_srcptr)))
  (mpfi_abs
   (int mpfi_abs (mpfi_ptr mpfi_srcptr)))

  (mpfi_mul_2exp
   (int mpfi_mul_2exp (mpfi_ptr mpfi_srcptr unsigned long)))
  (mpfi_mul_2ui
   (int mpfi_mul_2ui (mpfi_ptr mpfi_srcptr unsigned long)))
  (mpfi_mul_2si
   (int mpfi_mul_2si (mpfi_ptr mpfi_srcptr long)))
  (mpfi_div_2exp
   (int mpfi_div_2exp (mpfi_ptr mpfi_srcptr unsigned long)))
  (mpfi_div_2ui
   (int mpfi_div_2ui (mpfi_ptr mpfi_srcptr unsigned long)))
  (mpfi_div_2si
   (int mpfi_div_2si (mpfi_ptr mpfi_srcptr long)))

  (mpfi_log
   (int mpfi_log (mpfi_ptr mpfi_srcptr)))
  (mpfi_exp
   (int mpfi_exp (mpfi_ptr mpfi_srcptr)))
  (mpfi_exp2
   (int mpfi_exp2 (mpfi_ptr mpfi_srcptr)))

  (mpfi_cos
   (int mpfi_cos (mpfi_ptr mpfi_srcptr)))
  (mpfi_sin
   (int mpfi_sin (mpfi_ptr mpfi_srcptr)))
  (mpfi_tan
   (int mpfi_tan (mpfi_ptr mpfi_srcptr)))
  (mpfi_acos
   (int mpfi_acos (mpfi_ptr mpfi_srcptr)))
  (mpfi_asin
   (int mpfi_asin (mpfi_ptr mpfi_srcptr)))
  (mpfi_atan
   (int mpfi_atan (mpfi_ptr mpfi_srcptr)))

  (mpfi_cosh
   (int mpfi_cosh (mpfi_ptr mpfi_srcptr)))
  (mpfi_sinh
   (int mpfi_sinh (mpfi_ptr mpfi_srcptr)))
  (mpfi_tanh
   (int mpfi_tanh (mpfi_ptr mpfi_srcptr)))
  (mpfi_acosh
   (int mpfi_acosh (mpfi_ptr mpfi_srcptr)))
  (mpfi_asinh
   (int mpfi_asinh (mpfi_ptr mpfi_srcptr)))
  (mpfi_atanh
   (int mpfi_atanh (mpfi_ptr mpfi_srcptr)))

  (mpfi_log1p
   (int mpfi_log1p (mpfi_ptr mpfi_srcptr)))
  (mpfi_expm1
   (int mpfi_expm1 (mpfi_ptr mpfi_srcptr)))

  (mpfi_log2
   (int mpfi_log2 (mpfi_ptr mpfi_srcptr)))
  (mpfi_log10
   (int mpfi_log10 (mpfi_ptr mpfi_srcptr)))

  (mpfi_const_log2
   (int mpfi_const_log2 (mpfi_ptr)))
  (mpfi_const_pi
   (int mpfi_const_pi (mpfi_ptr)))
  (mpfi_const_euler
   (int mpfi_const_euler (mpfi_ptr)))

  ;; int (*mpfi_cmp) (mpfi_srcptr mpfi_srcptr)
  ;; int (*mpfi_cmp_d) (mpfi_srcptr double)
  ;; int (*mpfi_cmp_ui) (mpfi_srcptr unsigned-long)
  ;; int (*mpfi_cmp_si) (mpfi_srcptr long)
  ;; int (*mpfi_cmp_z) (mpfi_srcptr mpz_srcptr)
  ;; int (*mpfi_cmp_q) (mpfi_srcptr mpq_srcptr)
  ;; int (*mpfi_cmp_fr)(mpfi_srcptr mpfr_srcptr)

  ;; int (*mpfi_is_pos) (mpfi_srcptr)
  ;; int (*mpfi_is_nonneg) (mpfi_srcptr)
  ;; int (*mpfi_is_neg) (mpfi_srcptr)
  ;; int (*mpfi_is_nonpos) (mpfi_srcptr)
  ;; int (*mpfi_is_zero) (mpfi_srcptr)
  ;; int (*mpfi_is_strictly_pos) (mpfi_srcptr)
  ;; int (*mpfi_is_strictly_neg) (mpfi_srcptr)

  (mpfi_is_pos_default
   (int mpfi_is_pos_default (mpfi_srcptr)))
  (mpfi_is_nonneg_default
   (int mpfi_is_nonneg_default (mpfi_srcptr)))
  (mpfi_is_neg_default
   (int mpfi_is_neg_default (mpfi_srcptr)))
  (mpfi_is_nonpos_default
   (int mpfi_is_nonpos_default (mpfi_srcptr)))
  (mpfi_is_zero_default
   (int mpfi_is_zero_default (mpfi_srcptr)))
  (mpfi_is_strictly_neg_default
   (int mpfi_is_strictly_neg_default (mpfi_srcptr)))
  (mpfi_is_strictly_pos_default
   (int mpfi_is_strictly_pos_default (mpfi_srcptr)))

  (mpfi_cmp_default
   (int mpfi_cmp_default (mpfi_srcptr mpfi_srcptr)))
  (mpfi_cmp_d_default
   (int mpfi_cmp_d_default (mpfi_srcptr double)))
  (mpfi_cmp_ui_default
   (int mpfi_cmp_ui_default (mpfi_srcptr unsigned-long)))
  (mpfi_cmp_si_default
   (int mpfi_cmp_si_default (mpfi_srcptr long)))
  (mpfi_cmp_z_default
   (int mpfi_cmp_z_default (mpfi_srcptr mpz_srcptr)))
  (mpfi_cmp_q_default
   (int mpfi_cmp_q_default (mpfi_srcptr mpq_srcptr)))
  (mpfi_cmp_fr_default
   (int mpfi_cmp_fr_default (mpfi_srcptr mpfr_srcptr)))

  (mpfi_has_zero
   (int mpfi_has_zero (mpfi_srcptr)))

  (mpfi_nan_p
   (int mpfi_nan_p (mpfi_srcptr)))
  (mpfi_inf_p
   (int mpfi_inf_p (mpfi_srcptr)))
  (mpfi_bounded_p
   (int mpfi_bounded_p (mpfi_srcptr)))

  (mpfi_get_left
   (int mpfi_get_left (mpfr_ptr mpfi_srcptr)))
  (mpfi_get_right
   (int mpfi_get_right (mpfr_ptr mpfi_srcptr)))
  (mpfi_revert_if_needed
   (int mpfi_revert_if_needed (mpfi_ptr)))

  (mpfi_put
   (int mpfi_put (mpfi_ptr mpfi_srcptr)))
  (mpfi_put_d
   (int mpfi_put_d (mpfi_ptr double)))
  (mpfi_put_si
   (int mpfi_put_si (mpfi_ptr long)))
  (mpfi_put_ui
   (int mpfi_put_ui (mpfi_ptr unsigned-long)))
  (mpfi_put_z
   (int mpfi_put_z (mpfi_ptr mpz_srcptr)))
  (mpfi_put_q
   (int mpfi_put_q (mpfi_ptr mpq_srcptr)))
  (mpfi_put_fr
   (int mpfi_put_fr (mpfi_ptr mpfr_srcptr)))

  (mpfi_interv_d
   (int mpfi_interv_d (mpfi_ptr double double)))
  (mpfi_interv_si
   (int mpfi_interv_si (mpfi_ptr long long)))
  (mpfi_interv_ui
   (int mpfi_interv_ui (mpfi_ptr unsigned-long unsigned-long)))
  (mpfi_interv_z
   (int mpfi_interv_z (mpfi_ptr mpz_srcptr mpz_srcptr)))
  (mpfi_interv_q
   (int mpfi_interv_q (mpfi_ptr mpq_srcptr mpq_srcptr)))
  (mpfi_interv_fr
   (int mpfi_interv_fr (mpfi_ptr mpfr_srcptr mpfr_srcptr)))

  (mpfi_is_strictly_inside
   (int mpfi_is_strictly_inside (mpfi_srcptr mpfi_srcptr)))
  (mpfi_is_inside
   (int mpfi_is_inside 	(mpfi_srcptr mpfi_srcptr)))
  (mpfi_is_inside_d
   (int mpfi_is_inside_d (double mpfi_srcptr)))
  (mpfi_is_inside_ui
   (int mpfi_is_inside_ui (unsigned-long mpfi_srcptr)))
  (mpfi_is_inside_si
   (int mpfi_is_inside_si (long mpfi_srcptr)))
  (mpfi_is_inside_z
   (int mpfi_is_inside_z (mpz_srcptr mpfi_srcptr)))
  (mpfi_is_inside_q
   (int mpfi_is_inside_q (mpq_srcptr mpfi_srcptr)))
  (mpfi_is_inside_fr
   (int mpfi_is_inside_fr (mpfr_srcptr mpfi_srcptr)))

  (mpfi_is_empty
   (int mpfi_is_empty (mpfi_srcptr)))
  (mpfi_intersect
   (int mpfi_intersect (mpfi_ptr mpfi_srcptr mpfi_srcptr)))
  (mpfi_union
   (int mpfi_union (mpfi_ptr mpfi_srcptr mpfi_srcptr)))

  (mpfi_increase
   (int mpfi_increase (mpfi_ptr mpfr_srcptr)))
  (mpfi_blow
   (int mpfi_blow (mpfi_ptr mpfi_srcptr double)))
  (mpfi_bisect
   (int mpfi_bisect (mpfi_ptr mpfi_ptr mpfi_srcptr)))

  (mpfi_get_version
   (char* mpfi_get_version (void)))

  (mpfi_reset_error
   (void mpfi_reset_error (void)))
  (mpfi_set_error
   (void mpfi_set_error (int)))
  (mpfi_is_error
   (int mpfi_is_error (void))))


;;;; done

)

;;; end of file
