;;;
;;;Part of: Nausicaa/MP
;;;Contents: foreign functions interface to GMP
;;;Date: Tue Nov 25, 2008
;;;Time-stamp: <2008-11-27 18:27:51 marco>
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
;;;
;;;This  program  is free  software:  you  can redistribute  it
;;;and/or modify it  under the terms of the  GNU General Public
;;;License as published by the Free Software Foundation, either
;;;version  3 of  the License,  or (at  your option)  any later
;;;version.
;;;
;;;This  program is  distributed in  the hope  that it  will be
;;;useful, but  WITHOUT ANY WARRANTY; without  even the implied
;;;warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
;;;PURPOSE.   See  the  GNU  General Public  License  for  more
;;;details.
;;;
;;;You should  have received a  copy of the GNU  General Public
;;;License   along   with    this   program.    If   not,   see
;;;<http://www.gnu.org/licenses/>.
;;;



;;;; setup

(library (mp gmp)
  (export
    gmp_randinit_default
    gmp_randinit_lc_2exp
    gmp_randinit_lc_2exp_size
    gmp_randinit_mt
    gmp_randinit_set
    gmp_randseed
    gmp_randseed_ui
    gmp_randclear
    gmp_urandomb_ui
    gmp_urandomm_ui
    mpz_abs
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
    mpz_kronecker
    mpz_kronecker_si
    mpz_kronecker_ui
    mpz_si_kronecker
    mpz_ui_kronecker
    mpz_lcm
    mpz_lcm_ui
    mpz_legendre
    mpz_lucnum_ui
    mpz_lucnum2_ui
    mpz_millerrabin
    mpz_mod
    mpz_mod_ui
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
    mpz_eor
    mpq_abs
    mpq_add
    mpq_canonicalize
    mpq_clear
    mpq_cmp
    mpq_cmp_si
    mpq_cmp_ui
    mpq_div
    mpq_div_2exp
    mpq_equal
    mpq_get_num
    mpq_get_den
    mpq_get_d
    mpq_get_str
    mpq_init
    mpq_inp_str
    mpq_inv
    mpq_mul
    mpq_mul_2exp
    mpq_neg
    mpq_out_str
    mpq_set
    mpq_set_d
    mpq_set_den
    mpq_set_f
    mpq_set_num
    mpq_set_si
    mpq_set_str
    mpq_set_ui
    mpq_set_z
    mpq_sub
    mpq_swap
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
;;    (uriel printing)
    (uriel ffi)
    (mp sizeof))



;;;; loading

(define gmp-lib
  (let ((o (open-shared-object 'libgmp.so)))
    (shared-object o)
    o))



;;;; General functions.


;; Dangerous.  Let's avoid this.
;;
;; void mp_set_memory_functions (void *(*) (size_t),
;;				 void *(*) (void *, size_t, size_t),
;;				 void  (*) (void *, size_t))



;;;; random numbers

(define-c-function gmp_randinit_default
  (void __gmp_randinit_default (gmp_randstate_t)))

(define-c-function gmp_randinit_lc_2exp
  (void gmp_randinit_lc_2exp (gmp_randstate_t mpz_srcptr ulong ulong)))

(define-c-function gmp_randinit_lc_2exp_size
  (int gmp_randinit_lc_2exp_size (gmp_randstate_t ulong)))

(define-c-function gmp_randinit_mt
  (void gmp_randinit_mt (gmp_randstate_t)))

(define-c-function gmp_randinit_set
  (void gmp_randinit_set (gmp_randstate_t gmp_randstate_t)))

(define-c-function gmp_randseed
  (void gmp_randseed (gmp_randstate_t mpz_srcptr)))

(define-c-function gmp_randseed_ui
  (void gmp_randseed_ui (gmp_randstate_t ulong)))

(define-c-function gmp_randclear
  (void gmp_randclear (gmp_randstate_t)))

(define-c-function gmp_urandomb_ui
  (ulong gmp_urandomb_ui (gmp_randstate_t ulong)))

(define-c-function gmp_urandomm_ui
  (ulong gmp_urandomm_ui (gmp_randstate_t ulong)))




;;;; MPZ functions

(define-c-function mpz_abs
  (void mpz_abs (mpz_ptr mpz_srcptr)))

(define-c-function mpz_add
  (void mpz_add (mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_add_ui
  (void mpz_add_ui (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_addmul
  (void mpz_addmul (mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_addmul_ui
  (void mpz_addmul_ui (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_and
  (void mpz_and (mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_array_init
  (void mpz_array_init (mpz_ptr mp_size_t mp_size_t)))

(define-c-function mpz_bin_ui
  (void mpz_bin_ui (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_bin_uiui
  (void mpz_bin_uiui (mpz_ptr ulong ulong)))

(define-c-function mpz_cdiv_q
  (void mpz_cdiv_q (mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_cdiv_q_2exp
  (void mpz_cdiv_q_2exp (mpz_ptr mpz_srcptr unsigned long)))

(define-c-function mpz_cdiv_q_ui
  (ulong mpz_cdiv_q_ui (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_cdiv_qr
  (void mpz_cdiv_qr (mpz_ptr mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_cdiv_qr_ui
  (ulong mpz_cdiv_qr_ui (mpz_ptr mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_cdiv_r
  (void mpz_cdiv_r (mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_cdiv_r_2exp
  (void mpz_cdiv_r_2exp (mpz_ptr mpz_srcptr unsigned long)))

(define-c-function mpz_cdiv_r_ui
  (ulong mpz_cdiv_r_ui (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_cdiv_ui
  (ulong mpz_cdiv_ui (mpz_srcptr ulong)))

(define-c-function mpz_clear
  (void mpz_clear (mpz_ptr)))

(define-c-function mpz_clrbit
  (void mpz_clrbit (mpz_ptr ulong)))

(define-c-function mpz_cmp
  (int mpz_cmp (mpz_srcptr mpz_srcptr)))

(define-c-function mpz_cmp_d
  (int mpz_cmp_d (mpz_srcptr double)))

(define-c-function mpz_cmp_si
  (int mpz_cmp_si (mpz_srcptr long)))

(define-c-function mpz_cmp_ui
  (int mpz_cmp_ui (mpz_srcptr ulong)))

(define-c-function mpz_cmpabs
  (int mpz_cmpabs (mpz_srcptr mpz_srcptr)))

(define-c-function mpz_cmpabs_d
  (int mpz_cmpabs_d (mpz_srcptr double)))

(define-c-function mpz_cmpabs_ui
  (int mpz_cmpabs_ui (mpz_srcptr ulong)))

(define-c-function mpz_com
  (void mpz_com (mpz_ptr mpz_srcptr)))

(define-c-function mpz_combit
  (void mpz_combit (mpz_ptr ulong)))

(define-c-function mpz_congruent_p
  (int mpz_congruent_p (mpz_srcptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_congruent_2exp_p
  (int mpz_congruent_2exp_p (mpz_srcptr mpz_srcptr ulong)))

(define-c-function mpz_congruent_ui_p
  (int mpz_congruent_ui_p (mpz_srcptr ulong ulong)))

(define-c-function mpz_divexact
  (void mpz_divexact (mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_divexact_ui
  (void mpz_divexact_ui (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_divisible_p
  (int mpz_divisible_p (mpz_srcptr mpz_srcptr)))

(define-c-function mpz_divisible_ui_p
  (int mpz_divisible_ui_p (mpz_srcptr ulong)))

(define-c-function mpz_divisible_2exp_p
  (int mpz_divisible_2exp_p (mpz_srcptr ulong)))

(define-c-function mpz_dump
  (void mpz_dump (mpz_srcptr)))

(define-c-function mpz_export
  (pointer mpz_export (pointer pointer int size_t int size_t mpz_srcptr)))

(define-c-function mpz_fac_ui
  (void mpz_fac_ui (mpz_ptr ulong)))

(define-c-function mpz_fdiv_q
  (void mpz_fdiv_q (mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_fdiv_q_2exp
  (void mpz_fdiv_q_2exp (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_fdiv_q_ui
  (ulong mpz_fdiv_q_ui (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_fdiv_qr
  (void mpz_fdiv_qr (mpz_ptr mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_fdiv_qr_ui
  (ulong mpz_fdiv_qr_ui (mpz_ptr mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_fdiv_r
  (void mpz_fdiv_r (mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_fdiv_r_2exp
  (void mpz_fdiv_r_2exp (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_fdiv_r_ui
  (ulong mpz_fdiv_r_ui (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_fdiv_ui
  (ulong mpz_fdiv_ui (mpz_srcptr ulong)))

(define-c-function mpz_fib_ui
  (void mpz_fib_ui (mpz_ptr ulong)))

(define-c-function mpz_fib2_ui
  (void mpz_fib2_ui (mpz_ptr mpz_ptr ulong)))

(define-c-function mpz_fits_sint_p
  (int mpz_fits_sint_p (mpz_srcptr)))

(define-c-function mpz_fits_slong_p
  (int mpz_fits_slong_p (mpz_srcptr)))

(define-c-function mpz_fits_sshort_p
  (int mpz_fits_sshort_p (mpz_srcptr)))

(define-c-function mpz_fits_uint_p
  (int mpz_fits_uint_p (mpz_srcptr)))

(define-c-function mpz_fits_ulong_p
  (int mpz_fits_ulong_p (mpz_srcptr)))

(define-c-function mpz_fits_ushort_p
  (int mpz_fits_ushort_p (mpz_srcptr)))

(define-c-function mpz_gcd
  (void mpz_gcd (mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_gcd_ui
  (ulong mpz_gcd_ui (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_gcdext
  (void mpz_gcdext (mpz_ptr mpz_ptr mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_get_d
  (double mpz_get_d (mpz_srcptr)))

(define-c-function mpz_get_d_2exp
  (double mpz_get_d_2exp (signed long int * mpz_srcptr)))

(define-c-function mpz_get_si
  (long mpz_get_si (mpz_srcptr)))

(define-c-function mpz_get_str
  (char* mpz_get_str (char* int mpz_srcptr)))

(define-c-function mpz_get_ui
  (ulong mpz_get_ui (mpz_srcptr)))

(define-c-function mpz_getlimbn
  (mp_limb_t mpz_getlimbn (mpz_srcptr mp_size_t)))

(define-c-function mpz_hamdist
  (ulong mpz_hamdist (mpz_srcptr mpz_srcptr)))

(define-c-function mpz_import
  (void mpz_import (mpz_ptr size_t int size_t int size_t pointer)))

(define-c-function mpz_init
  (void mpz_init (mpz_ptr)))

(define-c-function mpz_init2
  (void mpz_init2 (mpz_ptr ulong)))

(define-c-function mpz_init_set
  (void mpz_init_set (mpz_ptr mpz_srcptr)))

(define-c-function mpz_init_set_d
  (void mpz_init_set_d (mpz_ptr double)))

(define-c-function mpz_init_set_si
  (void mpz_init_set_si (mpz_ptr long)))

(define-c-function mpz_init_set_str
  (int mpz_init_set_str (mpz_ptr char* int)))

(define-c-function mpz_init_set_ui
  (void mpz_init_set_ui (mpz_ptr ulong)))

(define-c-function mpz_inp_raw
  (size_t mpz_inp_raw (mpz_ptr FILE*)))

(define-c-function mpz_inp_str
  (size_t mpz_inp_str (mpz_ptr FILE* int)))

(define-c-function mpz_invert
  (int mpz_invert (mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_ior
  (void mpz_ior (mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_jacobi
  (int mpz_jacobi (mpz_srcptr mpz_srcptr)))

(define mpz_kronecker mpz_jacobi)

(define-c-function mpz_kronecker_si
  (int mpz_kronecker_si (mpz_srcptr long)))

(define-c-function mpz_kronecker_ui
  (int mpz_kronecker_ui (mpz_srcptr ulong)))

(define-c-function mpz_si_kronecker
  (int mpz_si_kronecker (long mpz_srcptr)))

(define-c-function mpz_ui_kronecker
  (int mpz_ui_kronecker (ulong mpz_srcptr)))

(define-c-function mpz_lcm
  (void mpz_lcm (mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_lcm_ui
  (void mpz_lcm_ui (mpz_ptr mpz_srcptr ulong)))

(define mpz_legendre mpz_jacobi)

(define-c-function mpz_lucnum_ui
  (void mpz_lucnum_ui (mpz_ptr ulong)))

(define-c-function mpz_lucnum2_ui
  (void mpz_lucnum2_ui (mpz_ptr mpz_ptr ulong)))

(define-c-function mpz_millerrabin
  (int mpz_millerrabin (mpz_srcptr int)))

(define-c-function mpz_mod
  (void mpz_mod (mpz_ptr mpz_srcptr mpz_srcptr)))

(define mpz_mod_ui mpz_fdiv_r_ui)

(define-c-function mpz_mul
  (void mpz_mul (mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_mul_2exp
  (void mpz_mul_2exp (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_mul_si
  (void mpz_mul_si (mpz_ptr mpz_srcptr long int)))

(define-c-function mpz_mul_ui
  (void mpz_mul_ui (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_neg
  (void mpz_neg (mpz_ptr mpz_srcptr)))

(define-c-function mpz_nextprime
  (void mpz_nextprime (mpz_ptr mpz_srcptr)))

(define-c-function mpz_out_raw
  (size_t mpz_out_raw (FILE* mpz_srcptr)))

(define-c-function mpz_out_str
  (size_t mpz_out_str (FILE* int mpz_srcptr)))

(define-c-function mpz_perfect_power_p
  (int mpz_perfect_power_p (mpz_srcptr)))

(define-c-function mpz_perfect_square_p
  (int mpz_perfect_square_p (mpz_srcptr)))

(define-c-function mpz_popcount
  (ulong mpz_popcount (mpz_srcptr)))

(define-c-function mpz_pow_ui
  (void mpz_pow_ui (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_powm
  (void mpz_powm (mpz_ptr mpz_srcptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_powm_ui
  (void mpz_powm_ui (mpz_ptr mpz_srcptr ulong mpz_srcptr)))

(define-c-function mpz_probab_prime_p
  (int mpz_probab_prime_p (mpz_srcptr int)))

(define-c-function mpz_random
  (void mpz_random (mpz_ptr mp_size_t)))

(define-c-function mpz_random2
  (void mpz_random2 (mpz_ptr mp_size_t)))

(define-c-function mpz_realloc2
  (void mpz_realloc2 (mpz_ptr ulong)))

(define-c-function mpz_remove
  (ulong mpz_remove (mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_root
  (int mpz_root (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_rootrem
  (void mpz_rootrem (mpz_ptrmpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_rrandomb
  (void mpz_rrandomb (mpz_ptr gmp_randstate_t ulong)))

(define-c-function mpz_scan0
  (ulong mpz_scan0 (mpz_srcptr ulong)))

(define-c-function mpz_scan1
  (ulong mpz_scan1 (mpz_srcptr ulong)))

(define-c-function mpz_set
  (void mpz_set (mpz_ptr mpz_srcptr)))

(define-c-function mpz_set_d
  (void mpz_set_d (mpz_ptr double)))

(define-c-function mpz_set_f
  (void mpz_set_f (mpz_ptr mpf_srcptr)))

(define-c-function mpz_set_q
  (void mpz_set_q (mpz_ptr mpq_srcptr)))

(define-c-function mpz_set_si
  (void mpz_set_si (mpz_ptr signed long int)))

(define-c-function mpz_set_str
  (int mpz_set_str (mpz_ptr char* int)))

(define-c-function mpz_set_ui
  (void mpz_set_ui (mpz_ptr ulong)))

(define-c-function mpz_setbit
  (void mpz_setbit (mpz_ptr ulong)))

(define-c-function mpz_size
  (size_t mpz_size (mpz_srcptr)))

(define-c-function mpz_sizeinbase
  (size_t mpz_sizeinbase (mpz_srcptr int)))

(define-c-function mpz_sqrt
  (void mpz_sqrt (mpz_ptr mpz_srcptr)))

(define-c-function mpz_sqrtrem
  (void mpz_sqrtrem (mpz_ptr mpz_ptr mpz_srcptr)))

(define-c-function mpz_sub
  (void mpz_sub (mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_sub_ui
  (void mpz_sub_ui (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_ui_sub
  (void mpz_ui_sub (mpz_ptr ulong mpz_srcptr)))

(define-c-function mpz_submul
  (void mpz_submul (mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_submul_ui
  (void mpz_submul_ui (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_swap
  (void mpz_swap (mpz_ptr mpz_ptr)))

(define-c-function mpz_tdiv_ui
  (ulong mpz_tdiv_ui (mpz_srcptr ulong)))

(define-c-function mpz_tdiv_q
  (void mpz_tdiv_q (mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_tdiv_q_2exp
  (void mpz_tdiv_q_2exp (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_tdiv_q_ui
  (ulong mpz_tdiv_q_ui (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_tdiv_qr
  (void mpz_tdiv_qr (mpz_ptr mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_tdiv_qr_ui
  (ulong mpz_tdiv_qr_ui (mpz_ptr mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_tdiv_r
  (void mpz_tdiv_r (mpz_ptr mpz_srcptr mpz_srcptr)))

(define-c-function mpz_tdiv_r_2exp
  (void mpz_tdiv_r_2exp (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_tdiv_r_ui
  (ulong mpz_tdiv_r_ui (mpz_ptr mpz_srcptr ulong)))

(define-c-function mpz_tstbit
  (int mpz_tstbit (mpz_srcptr ulong)))

(define-c-function mpz_ui_pow_ui
  (void mpz_ui_pow_ui (mpz_ptr ulong ulong)))

(define-c-function mpz_urandomb
  (void mpz_urandomb (mpz_ptr gmp_randstate_t ulong)))

(define-c-function mpz_urandomm
  (void mpz_urandomm (mpz_ptr gmp_randstate_t mpz_srcptr)))

(define-c-function mpz_xor
  (void mpz_xor (mpz_ptr mpz_srcptr mpz_srcptr)))

(define mpz_eor mpz_xor)



;;;; MPQ numbers


(define-c-function mpq_abs
  (void mpq_abs (mpq_ptr mpq_srcptr)))

(define-c-function mpq_add
  (void mpq_add (mpq_ptr mpq_srcptr mpq_srcptr)))

(define-c-function mpq_canonicalize
  (void mpq_canonicalize (mpq_ptr)))

(define-c-function mpq_clear
  (void mpq_clear (mpq_ptr)))

(define-c-function mpq_cmp
  (int mpq_cmp (mpq_srcptr mpq_srcptr)))

(define-c-function mpq_cmp_si
  (int mpq_cmp_si (mpq_srcptr long ulong)))

(define-c-function mpq_cmp_ui
  (int mpq_cmp_ui (mpq_srcptr ulong ulong)))

(define-c-function mpq_div
  (void mpq_div (mpq_ptr mpq_srcptr mpq_srcptr)))

(define-c-function mpq_div_2exp
  (void mpq_div_2exp (mpq_ptr mpq_srcptr ulong)))

(define-c-function mpq_equal
  (int mpq_equal (mpq_srcptr mpq_srcptr)))

(define-c-function mpq_get_num
  (void mpq_get_num (mpz_ptr mpq_srcptr)))

(define-c-function mpq_get_den
  (void mpq_get_den (mpz_ptr mpq_srcptr)))

(define-c-function mpq_get_d
  (double mpq_get_d (mpq_srcptr)))

(define-c-function mpq_get_str
  (char* mpq_get_str (char* int mpq_srcptr)))

(define-c-function mpq_init
  (void mpq_init (mpq_ptr)))

(define-c-function mpq_inp_str
  (size_t mpq_inp_str (mpq_ptr FILE* int)))

(define-c-function mpq_inv
  (void mpq_inv (mpq_ptr mpq_srcptr)))

(define-c-function mpq_mul
  (void mpq_mul (mpq_ptr mpq_srcptr mpq_srcptr)))

(define-c-function mpq_mul_2exp
  (void mpq_mul_2exp (mpq_ptr mpq_srcptr ulong)))

(define-c-function mpq_neg
  (void mpq_neg (mpq_ptr mpq_srcptr)))

(define-c-function mpq_out_str
  (size_t mpq_out_str (FILE* int mpq_srcptr)))

(define-c-function mpq_set
  (void mpq_set (mpq_ptr mpq_srcptr)))

(define-c-function mpq_set_d
  (void mpq_set_d (mpq_ptr double)))

(define-c-function mpq_set_den
  (void mpq_set_den (mpq_ptr mpz_srcptr)))

(define-c-function mpq_set_f
  (void mpq_set_f (mpq_ptr mpf_srcptr)))

(define-c-function mpq_set_num
  (void mpq_set_num (mpq_ptr mpz_srcptr)))

(define-c-function mpq_set_si
  (void mpq_set_si (mpq_ptr long ulong)))

(define-c-function mpq_set_str
  (int mpq_set_str (mpq_ptr char* int)))

(define-c-function mpq_set_ui
  (void mpq_set_ui (mpq_ptr ulong ulong)))

(define-c-function mpq_set_z
  (void mpq_set_z (mpq_ptr mpz_srcptr)))

(define-c-function mpq_sub
  (void mpq_sub (mpq_ptr mpq_srcptr mpq_srcptr)))

(define-c-function mpq_swap
  (void mpq_swap (mpq_ptr mpq_ptr)))



;;; MPF numbers

(define-c-function mpf_abs
  (void mpf_abs (mpf_ptr mpf_srcptr)))

(define-c-function mpf_add
  (void mpf_add (mpf_ptr mpf_srcptr mpf_srcptr)))

(define-c-function mpf_add_ui
  (void mpf_add_ui (mpf_ptr mpf_srcptr ulong)))

(define-c-function mpf_ceil
  (void mpf_ceil (mpf_ptr mpf_srcptr)))

(define-c-function mpf_clear
  (void mpf_clear (mpf_ptr)))

(define-c-function mpf_cmp
  (int mpf_cmp (mpf_srcptr mpf_srcptr)))

(define-c-function mpf_cmp_d
  (int mpf_cmp_d (mpf_srcptr double)))

(define-c-function mpf_cmp_si
  (int mpf_cmp_si (mpf_srcptr long)))

(define-c-function mpf_cmp_ui
  (int mpf_cmp_ui (mpf_srcptr ulong)))

(define-c-function mpf_div
  (void mpf_div (mpf_ptr mpf_srcptr mpf_srcptr)))

(define-c-function mpf_div_2exp
  (void mpf_div_2exp (mpf_ptr mpf_srcptr ulong)))

(define-c-function mpf_div_ui
  (void mpf_div_ui (mpf_ptr mpf_srcptr ulong)))

(define-c-function mpf_dump
  (void mpf_dump (mpf_srcptr)))

(define-c-function mpf_eq
  (int mpf_eq (mpf_srcptr mpf_srcptr ulong)))

(define-c-function mpf_fits_sint_p
  (int mpf_fits_sint_p (mpf_srcptr)))

(define-c-function mpf_fits_slong_p
  (int mpf_fits_slong_p (mpf_srcptr)))

(define-c-function mpf_fits_sshort_p
  (int mpf_fits_sshort_p (mpf_srcptr)))

(define-c-function mpf_fits_uint_p
  (int mpf_fits_uint_p (mpf_srcptr)))

(define-c-function mpf_fits_ulong_p
  (int mpf_fits_ulong_p (mpf_srcptr)))

(define-c-function mpf_fits_ushort_p
  (int mpf_fits_ushort_p (mpf_srcptr)))

(define-c-function mpf_floor
  (void mpf_floor (mpf_ptr mpf_srcptr)))

(define-c-function mpf_get_d
  (double mpf_get_d (mpf_srcptr)))

(define-c-function mpf_get_d_2exp
  (double mpf_get_d_2exp (long * mpf_srcptr)))

(define-c-function mpf_get_default_prec
  (ulong mpf_get_default_prec (void)))

(define-c-function mpf_get_prec
  (ulong mpf_get_prec (mpf_srcptr)))

(define-c-function mpf_get_si
  (long mpf_get_si (mpf_srcptr)))

(define-c-function mpf_get_str
  (char* mpf_get_str (char*  mp_exp_t* int size_t mpf_srcptr)))

(define-c-function mpf_get_ui
  (ulong mpf_get_ui (mpf_srcptr)))

(define-c-function mpf_init
  (void mpf_init (mpf_ptr)))

(define-c-function mpf_init2
  (void mpf_init2 (mpf_ptr ulong)))

(define-c-function mpf_init_set
  (void mpf_init_set (mpf_ptr mpf_srcptr)))

(define-c-function mpf_init_set_d
  (void mpf_init_set_d (mpf_ptr double)))

(define-c-function mpf_init_set_si
  (void mpf_init_set_si (mpf_ptr long)))

(define-c-function mpf_init_set_str
  (int mpf_init_set_str (mpf_ptr char* int)))

(define-c-function mpf_init_set_ui
  (void mpf_init_set_ui (mpf_ptr ulong)))

(define-c-function mpf_inp_str
  (size_t mpf_inp_str (mpf_ptr FILE* int)))

(define-c-function mpf_integer_p
  (int mpf_integer_p (mpf_srcptr)))

(define-c-function mpf_mul
  (void mpf_mul (mpf_ptr mpf_srcptr mpf_srcptr)))

(define-c-function mpf_mul_2exp
  (void mpf_mul_2exp (mpf_ptr mpf_srcptr ulong)))

(define-c-function mpf_mul_ui
  (void mpf_mul_ui (mpf_ptr mpf_srcptr ulong)))

(define-c-function mpf_neg
  (void mpf_neg (mpf_ptr mpf_srcptr)))

(define-c-function mpf_out_str
  (size_t mpf_out_str (FILE* int size_t mpf_srcptr)))

(define-c-function mpf_pow_ui
  (void mpf_pow_ui (mpf_ptr mpf_srcptr ulong)))

(define-c-function mpf_random2
  (void mpf_random2 (mpf_ptr mp_size_t mp_exp_t)))

(define-c-function mpf_reldiff
  (void mpf_reldiff (mpf_ptr mpf_srcptr mpf_srcptr)))

(define-c-function mpf_set
  (void mpf_set (mpf_ptr mpf_srcptr)))

(define-c-function mpf_set_d
  (void mpf_set_d (mpf_ptr double)))

(define-c-function mpf_set_default_prec
  (void mpf_set_default_prec (ulong)))

(define-c-function mpf_set_prec
  (void mpf_set_prec (mpf_ptr ulong)))

(define-c-function mpf_set_prec_raw
  (void mpf_set_prec_raw (mpf_ptr ulong)))

(define-c-function mpf_set_q
  (void mpf_set_q (mpf_ptr mpq_srcptr)))

(define-c-function mpf_set_si
  (void mpf_set_si (mpf_ptr long)))

(define-c-function mpf_set_str
  (int mpf_set_str (mpf_ptr char* int)))

(define-c-function mpf_set_ui
  (void mpf_set_ui (mpf_ptr ulong)))

(define-c-function mpf_set_z
  (void mpf_set_z (mpf_ptr mpz_srcptr)))

(define-c-function mpf_size
  (size_t mpf_size (mpf_srcptr)))

(define-c-function mpf_sqrt
  (void mpf_sqrt (mpf_ptr mpf_srcptr)))

(define-c-function mpf_sqrt_ui
  (void mpf_sqrt_ui (mpf_ptr ulong)))

(define-c-function mpf_sub
  (void mpf_sub (mpf_ptr mpf_srcptr mpf_srcptr)))

(define-c-function mpf_sub_ui
  (void mpf_sub_ui (mpf_ptr mpf_srcptr ulong)))

(define-c-function mpf_swap
  (void mpf_swap (mpf_ptr mpf_ptr)))

(define-c-function mpf_trunc
  (void mpf_trunc (mpf_ptr mpf_srcptr)))

(define-c-function mpf_ui_div
  (void mpf_ui_div (mpf_ptr ulong mpf_srcptr)))

(define-c-function mpf_ui_sub
  (void mpf_ui_sub (mpf_ptr ulong mpf_srcptr)))

(define-c-function mpf_urandomb
  (void mpf_urandomb (mpf_t gmp_randstate_t ulong)))


;;;; done

)

;;; end of file
