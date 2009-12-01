;;;
;;;Part of: Nausicaa/MP
;;;Contents: interface to GMP, MPZ functions
;;;Date: Fri Nov 28, 2008
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


(library (foreign math mp mpz)
  (export
;;;This is not always defined.
;;;    mpz_abs
    mpz_add
    mpz_add_ui
    mpz_addmul
    mpz_addmul_ui
    mpz_and
    mpz_array_init
    mpz_bin_ui
    mpz_bin_uiui
    mpz_cdiv_q
    mpz_cdiv_q_2exp
    mpz_cdiv_q_ui
    mpz_cdiv_qr
    mpz_cdiv_qr_ui
    mpz_cdiv_r
    mpz_cdiv_r_2exp
    mpz_cdiv_r_ui
    mpz_cdiv_ui
    mpz_clear
    mpz_clrbit
    mpz_cmp
    mpz_cmp_d
    mpz_cmp_si
    mpz_cmp_ui
    mpz_cmpabs
    mpz_cmpabs_d
    mpz_cmpabs_ui
    mpz_com
    mpz_combit
    mpz_congruent_p
    mpz_congruent_2exp_p
    mpz_congruent_ui_p
    mpz_divexact
    mpz_divexact_ui
    mpz_divisible_p
    mpz_divisible_ui_p
    mpz_divisible_2exp_p
    mpz_dump
    mpz_export
    mpz_fac_ui
    mpz_fdiv_q
    mpz_fdiv_q_2exp
    mpz_fdiv_q_ui
    mpz_fdiv_qr
    mpz_fdiv_qr_ui
    mpz_fdiv_r
    mpz_fdiv_r_2exp
    mpz_fdiv_r_ui
    mpz_fdiv_ui
    mpz_fib_ui
    mpz_fib2_ui
    mpz_fits_sint_p
    mpz_fits_slong_p
    mpz_fits_sshort_p
    mpz_fits_uint_p
    mpz_fits_ulong_p
    mpz_fits_ushort_p
    mpz_gcd
    mpz_gcd_ui
    mpz_gcdext
    mpz_get_d
    mpz_get_d_2exp
    mpz_get_si
    mpz_get_str
    mpz_get_ui
    mpz_getlimbn
    mpz_hamdist
    mpz_import
    mpz_init
    mpz_init2
    mpz_init_set
    mpz_init_set_d
    mpz_init_set_si
    mpz_init_set_str
    mpz_init_set_ui
    mpz_inp_raw
    mpz_inp_str
    mpz_invert
    mpz_ior
    mpz_jacobi
    mpz_kronecker_si
    mpz_kronecker_ui
    mpz_si_kronecker
    mpz_ui_kronecker
    mpz_lcm
    mpz_lcm_ui
    mpz_lucnum_ui
    mpz_lucnum2_ui
    mpz_millerrabin
    mpz_mod
    mpz_mul
    mpz_mul_2exp
    mpz_mul_si
    mpz_mul_ui
    mpz_neg
    mpz_nextprime
    mpz_out_raw
    mpz_out_str
    mpz_perfect_power_p
    mpz_perfect_square_p
    mpz_popcount
    mpz_pow_ui
    mpz_powm
    mpz_powm_ui
    mpz_probab_prime_p
    mpz_random
    mpz_random2
    mpz_realloc2
    mpz_remove
    mpz_root
    mpz_rootrem
    mpz_rrandomb
    mpz_scan0
    mpz_scan1
    mpz_set
    mpz_set_d
    mpz_set_f
    mpz_set_q
    mpz_set_si
    mpz_set_str
    mpz_set_ui
    mpz_setbit
    mpz_size
    mpz_sizeinbase
    mpz_sqrt
    mpz_sqrtrem
    mpz_sub
    mpz_sub_ui
    mpz_ui_sub
    mpz_submul
    mpz_submul_ui
    mpz_swap
    mpz_tdiv_ui
    mpz_tdiv_q
    mpz_tdiv_q_2exp
    mpz_tdiv_q_ui
    mpz_tdiv_qr
    mpz_tdiv_qr_ui
    mpz_tdiv_r
    mpz_tdiv_r_2exp
    mpz_tdiv_r_ui
    mpz_tstbit
    mpz_ui_pow_ui
    mpz_urandomb
    mpz_urandomm
    mpz_xor

    (rename (mpz_jacobi mpz_kronecker))
    (rename (mpz_jacobi mpz_legendre))
    (rename (mpz_fdiv_r_ui mpz_mod_ui))
    (rename (mpz_xor mpz_eor)))
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign math mp sizeof))


;;;; loading

(define-shared-object gmp-shared-object
  GMP_SHARED_OBJECT)


;;;; MPZ functions

(define-c-functions gmp-shared-object

;;;This is not always defined.
;;;   (mpz_abs
;;;   (void __mpz_abs (mpz_ptr mpz_srcptr)))

  (mpz_add
   (void __gmpz_add (mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_add_ui
   (void __gmpz_add_ui (mpz_ptr mpz_srcptr ulong)))

  (mpz_addmul
   (void __gmpz_addmul (mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_addmul_ui
   (void __gmpz_addmul_ui (mpz_ptr mpz_srcptr ulong)))

  (mpz_and
   (void __gmpz_and (mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_array_init
   (void __gmpz_array_init (mpz_ptr mp_size_t mp_size_t)))

  (mpz_bin_ui
   (void __gmpz_bin_ui (mpz_ptr mpz_srcptr ulong)))

  (mpz_bin_uiui
   (void __gmpz_bin_uiui (mpz_ptr ulong ulong)))

  (mpz_cdiv_q
   (void __gmpz_cdiv_q (mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_cdiv_q_2exp
   (void __gmpz_cdiv_q_2exp (mpz_ptr mpz_srcptr long)))

  (mpz_cdiv_q_ui
   (ulong __gmpz_cdiv_q_ui (mpz_ptr mpz_srcptr ulong)))

  (mpz_cdiv_qr
   (void __gmpz_cdiv_qr (mpz_ptr mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_cdiv_qr_ui
   (ulong __gmpz_cdiv_qr_ui (mpz_ptr mpz_ptr mpz_srcptr ulong)))

  (mpz_cdiv_r
   (void __gmpz_cdiv_r (mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_cdiv_r_2exp
   (void __gmpz_cdiv_r_2exp (mpz_ptr mpz_srcptr long)))

  (mpz_cdiv_r_ui
   (ulong __gmpz_cdiv_r_ui (mpz_ptr mpz_srcptr ulong)))

  (mpz_cdiv_ui
   (ulong __gmpz_cdiv_ui (mpz_srcptr ulong)))

  (mpz_clear
   (void __gmpz_clear (mpz_ptr)))

  (mpz_clrbit
   (void __gmpz_clrbit (mpz_ptr ulong)))

  (mpz_cmp
   (int __gmpz_cmp (mpz_srcptr mpz_srcptr)))

  (mpz_cmp_d
   (int __gmpz_cmp_d (mpz_srcptr double)))

  (mpz_cmp_si
   (int __gmpz_cmp_si (mpz_srcptr long)))

  (mpz_cmp_ui
   (int __gmpz_cmp_ui (mpz_srcptr ulong)))

  (mpz_cmpabs
   (int __gmpz_cmpabs (mpz_srcptr mpz_srcptr)))

  (mpz_cmpabs_d
   (int __gmpz_cmpabs_d (mpz_srcptr double)))

  (mpz_cmpabs_ui
   (int __gmpz_cmpabs_ui (mpz_srcptr ulong)))

  (mpz_com
   (void __gmpz_com (mpz_ptr mpz_srcptr)))

  (mpz_combit
   (void __gmpz_combit (mpz_ptr ulong)))

  (mpz_congruent_p
   (int __gmpz_congruent_p (mpz_srcptr mpz_srcptr mpz_srcptr)))

  (mpz_congruent_2exp_p
   (int __gmpz_congruent_2exp_p (mpz_srcptr mpz_srcptr ulong)))

  (mpz_congruent_ui_p
   (int __gmpz_congruent_ui_p (mpz_srcptr ulong ulong)))

  (mpz_divexact
   (void __gmpz_divexact (mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_divexact_ui
   (void __gmpz_divexact_ui (mpz_ptr mpz_srcptr ulong)))

  (mpz_divisible_p
   (int __gmpz_divisible_p (mpz_srcptr mpz_srcptr)))

  (mpz_divisible_ui_p
   (int __gmpz_divisible_ui_p (mpz_srcptr ulong)))

  (mpz_divisible_2exp_p
   (int __gmpz_divisible_2exp_p (mpz_srcptr ulong)))

  (mpz_dump
   (void __gmpz_dump (mpz_srcptr)))

  (mpz_export
   (pointer __gmpz_export (pointer pointer int size_t int size_t mpz_srcptr)))

  (mpz_fac_ui
   (void __gmpz_fac_ui (mpz_ptr ulong)))

  (mpz_fdiv_q
   (void __gmpz_fdiv_q (mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_fdiv_q_2exp
   (void __gmpz_fdiv_q_2exp (mpz_ptr mpz_srcptr ulong)))

  (mpz_fdiv_q_ui
   (ulong __gmpz_fdiv_q_ui (mpz_ptr mpz_srcptr ulong)))

  (mpz_fdiv_qr
   (void __gmpz_fdiv_qr (mpz_ptr mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_fdiv_qr_ui
   (ulong __gmpz_fdiv_qr_ui (mpz_ptr mpz_ptr mpz_srcptr ulong)))

  (mpz_fdiv_r
   (void __gmpz_fdiv_r (mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_fdiv_r_2exp
   (void __gmpz_fdiv_r_2exp (mpz_ptr mpz_srcptr ulong)))

  (mpz_fdiv_r_ui
   (ulong __gmpz_fdiv_r_ui (mpz_ptr mpz_srcptr ulong)))

  (mpz_fdiv_ui
   (ulong __gmpz_fdiv_ui (mpz_srcptr ulong)))

  (mpz_fib_ui
   (void __gmpz_fib_ui (mpz_ptr ulong)))

  (mpz_fib2_ui
   (void __gmpz_fib2_ui (mpz_ptr mpz_ptr ulong)))

  (mpz_fits_sint_p
   (int __gmpz_fits_sint_p (mpz_srcptr)))

  (mpz_fits_slong_p
   (int __gmpz_fits_slong_p (mpz_srcptr)))

  (mpz_fits_sshort_p
   (int __gmpz_fits_sshort_p (mpz_srcptr)))

  (mpz_fits_uint_p
   (int __gmpz_fits_uint_p (mpz_srcptr)))

  (mpz_fits_ulong_p
   (int __gmpz_fits_ulong_p (mpz_srcptr)))

  (mpz_fits_ushort_p
   (int __gmpz_fits_ushort_p (mpz_srcptr)))

  (mpz_gcd
   (void __gmpz_gcd (mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_gcd_ui
   (ulong __gmpz_gcd_ui (mpz_ptr mpz_srcptr ulong)))

  (mpz_gcdext
   (void __gmpz_gcdext (mpz_ptr mpz_ptr mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_get_d
   (double __gmpz_get_d (mpz_srcptr)))

  (mpz_get_d_2exp
   (double __gmpz_get_d_2exp (pointer mpz_srcptr)))

  (mpz_get_si
   (long __gmpz_get_si (mpz_srcptr)))

  (mpz_get_str
   (char* __gmpz_get_str (char* int mpz_srcptr)))

  (mpz_get_ui
   (ulong __gmpz_get_ui (mpz_srcptr)))

  (mpz_getlimbn
   (mp_limb_t __gmpz_getlimbn (mpz_srcptr mp_size_t)))

  (mpz_hamdist
   (ulong __gmpz_hamdist (mpz_srcptr mpz_srcptr)))

  (mpz_import
   (void __gmpz_import (mpz_ptr size_t int size_t int size_t pointer)))

  (mpz_init
   (void __gmpz_init (mpz_ptr)))

  (mpz_init2
   (void __gmpz_init2 (mpz_ptr ulong)))

  (mpz_init_set
   (void __gmpz_init_set (mpz_ptr mpz_srcptr)))

  (mpz_init_set_d
   (void __gmpz_init_set_d (mpz_ptr double)))

  (mpz_init_set_si
   (void __gmpz_init_set_si (mpz_ptr long)))

  (mpz_init_set_str
   (int __gmpz_init_set_str (mpz_ptr char* int)))

  (mpz_init_set_ui
   (void __gmpz_init_set_ui (mpz_ptr ulong)))

  (mpz_inp_raw
   (size_t __gmpz_inp_raw (mpz_ptr FILE*)))

  (mpz_inp_str
   (size_t __gmpz_inp_str (mpz_ptr FILE* int)))

  (mpz_invert
   (int __gmpz_invert (mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_ior
   (void __gmpz_ior (mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_jacobi
   (int __gmpz_jacobi (mpz_srcptr mpz_srcptr)))

  (mpz_kronecker_si
   (int __gmpz_kronecker_si (mpz_srcptr long)))

  (mpz_kronecker_ui
   (int __gmpz_kronecker_ui (mpz_srcptr ulong)))

  (mpz_si_kronecker
   (int __gmpz_si_kronecker (long mpz_srcptr)))

  (mpz_ui_kronecker
   (int __gmpz_ui_kronecker (ulong mpz_srcptr)))

  (mpz_lcm
   (void __gmpz_lcm (mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_lcm_ui
   (void __gmpz_lcm_ui (mpz_ptr mpz_srcptr ulong)))

  (mpz_lucnum_ui
   (void __gmpz_lucnum_ui (mpz_ptr ulong)))

  (mpz_lucnum2_ui
   (void __gmpz_lucnum2_ui (mpz_ptr mpz_ptr ulong)))

  (mpz_millerrabin
   (int __gmpz_millerrabin (mpz_srcptr int)))

  (mpz_mod
   (void __gmpz_mod (mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_mul
   (void __gmpz_mul (mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_mul_2exp
   (void __gmpz_mul_2exp (mpz_ptr mpz_srcptr ulong)))

  (mpz_mul_si
   (void __gmpz_mul_si (mpz_ptr mpz_srcptr long int)))

  (mpz_mul_ui
   (void __gmpz_mul_ui (mpz_ptr mpz_srcptr ulong)))

  (mpz_neg
   (void __gmpz_neg (mpz_ptr mpz_srcptr)))

  (mpz_nextprime
   (void __gmpz_nextprime (mpz_ptr mpz_srcptr)))

  (mpz_out_raw
   (size_t __gmpz_out_raw (FILE* mpz_srcptr)))

  (mpz_out_str
   (size_t __gmpz_out_str (FILE* int mpz_srcptr)))

  (mpz_perfect_power_p
   (int __gmpz_perfect_power_p (mpz_srcptr)))

  (mpz_perfect_square_p
   (int __gmpz_perfect_square_p (mpz_srcptr)))

  (mpz_popcount
   (ulong __gmpz_popcount (mpz_srcptr)))

  (mpz_pow_ui
   (void __gmpz_pow_ui (mpz_ptr mpz_srcptr ulong)))

  (mpz_powm
   (void __gmpz_powm (mpz_ptr mpz_srcptr mpz_srcptr mpz_srcptr)))

  (mpz_powm_ui
   (void __gmpz_powm_ui (mpz_ptr mpz_srcptr ulong mpz_srcptr)))

  (mpz_probab_prime_p
   (int __gmpz_probab_prime_p (mpz_srcptr int)))

  (mpz_random
   (void __gmpz_random (mpz_ptr mp_size_t)))

  (mpz_random2
   (void __gmpz_random2 (mpz_ptr mp_size_t)))

  (mpz_realloc2
   (void __gmpz_realloc2 (mpz_ptr ulong)))

  (mpz_remove
   (ulong __gmpz_remove (mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_root
   (int __gmpz_root (mpz_ptr mpz_srcptr ulong)))

  (mpz_rootrem
   (void __gmpz_rootrem (mpz_ptr mpz_ptr mpz_srcptr ulong)))

  (mpz_rrandomb
   (void __gmpz_rrandomb (mpz_ptr gmp_randstate_t ulong)))

  (mpz_scan0
   (ulong __gmpz_scan0 (mpz_srcptr ulong)))

  (mpz_scan1
   (ulong __gmpz_scan1 (mpz_srcptr ulong)))

  (mpz_set
   (void __gmpz_set (mpz_ptr mpz_srcptr)))

  (mpz_set_d
   (void __gmpz_set_d (mpz_ptr double)))

  (mpz_set_f
   (void __gmpz_set_f (mpz_ptr mpf_srcptr)))

  (mpz_set_q
   (void __gmpz_set_q (mpz_ptr mpq_srcptr)))

  (mpz_set_si
   (void __gmpz_set_si (mpz_ptr long)))

  (mpz_set_str
   (int __gmpz_set_str (mpz_ptr char* int)))

  (mpz_set_ui
   (void __gmpz_set_ui (mpz_ptr ulong)))

  (mpz_setbit
   (void __gmpz_setbit (mpz_ptr ulong)))

  (mpz_size
   (size_t __gmpz_size (mpz_srcptr)))

  (mpz_sizeinbase
   (size_t __gmpz_sizeinbase (mpz_srcptr int)))

  (mpz_sqrt
   (void __gmpz_sqrt (mpz_ptr mpz_srcptr)))

  (mpz_sqrtrem
   (void __gmpz_sqrtrem (mpz_ptr mpz_ptr mpz_srcptr)))

  (mpz_sub
   (void __gmpz_sub (mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_sub_ui
   (void __gmpz_sub_ui (mpz_ptr mpz_srcptr ulong)))

  (mpz_ui_sub
   (void __gmpz_ui_sub (mpz_ptr ulong mpz_srcptr)))

  (mpz_submul
   (void __gmpz_submul (mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_submul_ui
   (void __gmpz_submul_ui (mpz_ptr mpz_srcptr ulong)))

  (mpz_swap
   (void __gmpz_swap (mpz_ptr mpz_ptr)))

  (mpz_tdiv_ui
   (ulong __gmpz_tdiv_ui (mpz_srcptr ulong)))

  (mpz_tdiv_q
   (void __gmpz_tdiv_q (mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_tdiv_q_2exp
   (void __gmpz_tdiv_q_2exp (mpz_ptr mpz_srcptr ulong)))

  (mpz_tdiv_q_ui
   (ulong __gmpz_tdiv_q_ui (mpz_ptr mpz_srcptr ulong)))

  (mpz_tdiv_qr
   (void __gmpz_tdiv_qr (mpz_ptr mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_tdiv_qr_ui
   (ulong __gmpz_tdiv_qr_ui (mpz_ptr mpz_ptr mpz_srcptr ulong)))

  (mpz_tdiv_r
   (void __gmpz_tdiv_r (mpz_ptr mpz_srcptr mpz_srcptr)))

  (mpz_tdiv_r_2exp
   (void __gmpz_tdiv_r_2exp (mpz_ptr mpz_srcptr ulong)))

  (mpz_tdiv_r_ui
   (ulong __gmpz_tdiv_r_ui (mpz_ptr mpz_srcptr ulong)))

  (mpz_tstbit
   (int __gmpz_tstbit (mpz_srcptr ulong)))

  (mpz_ui_pow_ui
   (void __gmpz_ui_pow_ui (mpz_ptr ulong ulong)))

  (mpz_urandomb
   (void __gmpz_urandomb (mpz_ptr gmp_randstate_t ulong)))

  (mpz_urandomm
   (void __gmpz_urandomm (mpz_ptr gmp_randstate_t mpz_srcptr)))

  (mpz_xor
   (void __gmpz_xor (mpz_ptr mpz_srcptr mpz_srcptr))))


;;;; done

)

;;; end of file
