;;;
;;;Part of: Nausicaa/MP
;;;Contents: interface to GMP, MPF functions
;;;Date: Tue Nov 25, 2008
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


(library (foreign math mp mpf)
  (export
    mpf_abs
    mpf_add
    mpf_add_ui
    mpf_ceil
    mpf_clear
    mpf_cmp
    mpf_cmp_d
    mpf_cmp_si
    mpf_cmp_ui
    mpf_div
    mpf_div_2exp
    mpf_div_ui
    mpf_dump
    mpf_eq
    mpf_fits_sint_p
    mpf_fits_slong_p
    mpf_fits_sshort_p
    mpf_fits_uint_p
    mpf_fits_ulong_p
    mpf_fits_ushort_p
    mpf_floor
    mpf_get_d
    mpf_get_d_2exp
    mpf_get_prec
    mpf_get_si
    mpf_get_str
    mpf_get_ui
    mpf_init
    mpf_init2
    mpf_init_set
    mpf_init_set_d
    mpf_init_set_si
    mpf_init_set_str
    mpf_init_set_ui
    mpf_inp_str
    mpf_integer_p
    mpf_mul
    mpf_mul_2exp
    mpf_mul_ui
    mpf_neg
    mpf_out_str
    mpf_pow_ui
    mpf_random2
    mpf_reldiff
    mpf_set
    mpf_set_d
    mpf_set_prec
    mpf_set_prec_raw
    mpf_set_q
    mpf_set_si
    mpf_set_str
    mpf_set_ui
    mpf_set_z
    mpf_size
    mpf_sqrt
    mpf_sqrt_ui
    mpf_sub
    mpf_sub_ui
    mpf_swap
    mpf_trunc
    mpf_ui_div
    mpf_ui_sub
    mpf_urandomb
    mpf_set_default_prec
    mpf_get_default_prec)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign math mp sizeof))

  (define-shared-object gmp-shared-object
    GMP_SHARED_OBJECT)


(define-c-functions gmp-shared-object
  (mpf_abs
   (void __gmpf_abs (mpf_ptr mpf_srcptr)))

  (mpf_add
   (void __gmpf_add (mpf_ptr mpf_srcptr mpf_srcptr)))

  (mpf_add_ui
   (void __gmpf_add_ui (mpf_ptr mpf_srcptr ulong)))

  (mpf_ceil
   (void __gmpf_ceil (mpf_ptr mpf_srcptr)))

  (mpf_clear
   (void __gmpf_clear (mpf_ptr)))

  (mpf_cmp
   (int __gmpf_cmp (mpf_srcptr mpf_srcptr)))

  (mpf_cmp_d
   (int __gmpf_cmp_d (mpf_srcptr double)))

  (mpf_cmp_si
   (int __gmpf_cmp_si (mpf_srcptr long)))

  (mpf_cmp_ui
   (int __gmpf_cmp_ui (mpf_srcptr ulong)))

  (mpf_div
   (void __gmpf_div (mpf_ptr mpf_srcptr mpf_srcptr)))

  (mpf_div_2exp
   (void __gmpf_div_2exp (mpf_ptr mpf_srcptr ulong)))

  (mpf_div_ui
   (void __gmpf_div_ui (mpf_ptr mpf_srcptr ulong)))

  (mpf_dump
   (void __gmpf_dump (mpf_srcptr)))

  (mpf_eq
   (int __gmpf_eq (mpf_srcptr mpf_srcptr ulong)))

  (mpf_fits_sint_p
   (int __gmpf_fits_sint_p (mpf_srcptr)))

  (mpf_fits_slong_p
   (int __gmpf_fits_slong_p (mpf_srcptr)))

  (mpf_fits_sshort_p
   (int __gmpf_fits_sshort_p (mpf_srcptr)))

  (mpf_fits_uint_p
   (int __gmpf_fits_uint_p (mpf_srcptr)))

  (mpf_fits_ulong_p
   (int __gmpf_fits_ulong_p (mpf_srcptr)))

  (mpf_fits_ushort_p
   (int __gmpf_fits_ushort_p (mpf_srcptr)))

  (mpf_floor
   (void __gmpf_floor (mpf_ptr mpf_srcptr)))

  (mpf_get_d
   (double __gmpf_get_d (mpf_srcptr)))

  (mpf_get_d_2exp
   (double __gmpf_get_d_2exp (long mpf_srcptr)))

  (mpf_get_default_prec
   (ulong __gmpf_get_default_prec (void)))

  (mpf_get_prec
   (ulong __gmpf_get_prec (mpf_srcptr)))

  (mpf_get_si
   (long __gmpf_get_si (mpf_srcptr)))

  (mpf_get_str
   (char* __gmpf_get_str (char*  mp_exp_t* int size_t mpf_srcptr)))

  (mpf_get_ui
   (ulong __gmpf_get_ui (mpf_srcptr)))

  (mpf_init
   (void __gmpf_init (mpf_ptr)))

  (mpf_init2
   (void __gmpf_init2 (mpf_ptr ulong)))

  (mpf_init_set
   (void __gmpf_init_set (mpf_ptr mpf_srcptr)))

  (mpf_init_set_d
   (void __gmpf_init_set_d (mpf_ptr double)))

  (mpf_init_set_si
   (void __gmpf_init_set_si (mpf_ptr long)))

  (mpf_init_set_str
   (int __gmpf_init_set_str (mpf_ptr char* int)))

  (mpf_init_set_ui
   (void __gmpf_init_set_ui (mpf_ptr ulong)))

  (mpf_inp_str
   (size_t __gmpf_inp_str (mpf_ptr FILE* int)))

  (mpf_integer_p
   (int __gmpf_integer_p (mpf_srcptr)))

  (mpf_mul
   (void __gmpf_mul (mpf_ptr mpf_srcptr mpf_srcptr)))

  (mpf_mul_2exp
   (void __gmpf_mul_2exp (mpf_ptr mpf_srcptr ulong)))

  (mpf_mul_ui
   (void __gmpf_mul_ui (mpf_ptr mpf_srcptr ulong)))

  (mpf_neg
   (void __gmpf_neg (mpf_ptr mpf_srcptr)))

  (mpf_out_str
   (size_t __gmpf_out_str (FILE* int size_t mpf_srcptr)))

  (mpf_pow_ui
   (void __gmpf_pow_ui (mpf_ptr mpf_srcptr ulong)))

  (mpf_random2
   (void __gmpf_random2 (mpf_ptr mp_size_t mp_exp_t)))

  (mpf_reldiff
   (void __gmpf_reldiff (mpf_ptr mpf_srcptr mpf_srcptr)))

  (mpf_set
   (void __gmpf_set (mpf_ptr mpf_srcptr)))

  (mpf_set_d
   (void __gmpf_set_d (mpf_ptr double)))

  (mpf_set_default_prec
   (void __gmpf_set_default_prec (ulong)))

  (mpf_set_prec
   (void __gmpf_set_prec (mpf_ptr ulong)))

  (mpf_set_prec_raw
   (void __gmpf_set_prec_raw (mpf_ptr ulong)))

  (mpf_set_q
   (void __gmpf_set_q (mpf_ptr mpq_srcptr)))

  (mpf_set_si
   (void __gmpf_set_si (mpf_ptr long)))

  (mpf_set_str
   (int __gmpf_set_str (mpf_ptr char* int)))

  (mpf_set_ui
   (void __gmpf_set_ui (mpf_ptr ulong)))

  (mpf_set_z
   (void __gmpf_set_z (mpf_ptr mpz_srcptr)))

  (mpf_size
   (size_t __gmpf_size (mpf_srcptr)))

  (mpf_sqrt
   (void __gmpf_sqrt (mpf_ptr mpf_srcptr)))

  (mpf_sqrt_ui
   (void __gmpf_sqrt_ui (mpf_ptr ulong)))

  (mpf_sub
   (void __gmpf_sub (mpf_ptr mpf_srcptr mpf_srcptr)))

  (mpf_sub_ui
   (void __gmpf_sub_ui (mpf_ptr mpf_srcptr ulong)))

  (mpf_swap
   (void __gmpf_swap (mpf_ptr mpf_ptr)))

  (mpf_trunc
   (void __gmpf_trunc (mpf_ptr mpf_srcptr)))

  (mpf_ui_div
   (void __gmpf_ui_div (mpf_ptr ulong mpf_srcptr)))

  (mpf_ui_sub
   (void __gmpf_ui_sub (mpf_ptr ulong mpf_srcptr)))

  (mpf_urandomb
   (void __gmpf_urandomb (mpf_ptr gmp_randstate_t ulong))))


;;;; done

)

;;; end of file
