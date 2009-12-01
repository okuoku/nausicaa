;;;
;;;Part of: Nausicaa/MP
;;;Contents: interface to GMP, MPQ functions
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


(library (foreign math mp mpq)
  (export
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
    mpq_swap)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign math mp sizeof)
    (foreign math mp mpz))

  (define-shared-object gmp-shared-object
    GMP_SHARED_OBJECT)


(define-c-functions gmp-shared-object
  (mpq_abs
   (void __gmpq_abs (mpq_ptr mpq_srcptr)))

  (mpq_add
   (void __gmpq_add (mpq_ptr mpq_srcptr mpq_srcptr)))

  (mpq_canonicalize
   (void __gmpq_canonicalize (mpq_ptr)))

  (mpq_clear
   (void __gmpq_clear (mpq_ptr)))

  (mpq_cmp
   (int __gmpq_cmp (mpq_srcptr mpq_srcptr)))

  (mpq_cmp_si
   (int __gmpq_cmp_si (mpq_srcptr long ulong)))

  (mpq_cmp_ui
   (int __gmpq_cmp_ui (mpq_srcptr ulong ulong)))

  (mpq_div
   (void __gmpq_div (mpq_ptr mpq_srcptr mpq_srcptr)))

  (mpq_div_2exp
   (void __gmpq_div_2exp (mpq_ptr mpq_srcptr ulong)))

  (mpq_equal
   (int __gmpq_equal (mpq_srcptr mpq_srcptr)))

  (mpq_get_num
   (void __gmpq_get_num (mpz_ptr mpq_srcptr)))

  (mpq_get_den
   (void __gmpq_get_den (mpz_ptr mpq_srcptr)))

  (mpq_get_d
   (double __gmpq_get_d (mpq_srcptr)))

  (mpq_get_str
   (char* __gmpq_get_str (char* int mpq_srcptr)))

  (mpq_init
   (void __gmpq_init (mpq_ptr)))

  (mpq_inp_str
   (size_t __gmpq_inp_str (mpq_ptr FILE* int)))

  (mpq_inv
   (void __gmpq_inv (mpq_ptr mpq_srcptr)))

  (mpq_mul
   (void __gmpq_mul (mpq_ptr mpq_srcptr mpq_srcptr)))

  (mpq_mul_2exp
   (void __gmpq_mul_2exp (mpq_ptr mpq_srcptr ulong)))

  (mpq_neg
   (void __gmpq_neg (mpq_ptr mpq_srcptr)))

  (mpq_out_str
   (size_t __gmpq_out_str (FILE* int mpq_srcptr)))

  (mpq_set
   (void __gmpq_set (mpq_ptr mpq_srcptr)))

  (mpq_set_d
   (void __gmpq_set_d (mpq_ptr double)))

  (mpq_set_den
   (void __gmpq_set_den (mpq_ptr mpz_srcptr)))

  (mpq_set_f
   (void __gmpq_set_f (mpq_ptr mpf_srcptr)))

  (mpq_set_num
   (void __gmpq_set_num (mpq_ptr mpz_srcptr)))

  (mpq_set_si
   (void __gmpq_set_si (mpq_ptr long ulong)))

  (mpq_set_ui
   (void __gmpq_set_ui (mpq_ptr ulong ulong)))

  (mpq_set_str
   (int __gmpq_set_str (mpq_ptr char* int)))

  (mpq_set_z
   (void __gmpq_set_z (mpq_ptr mpz_srcptr)))

  (mpq_sub
   (void __gmpq_sub (mpq_ptr mpq_srcptr mpq_srcptr)))

  (mpq_swap
   (void __gmpq_swap (mpq_ptr mpq_ptr))))


;;;; done

)

;;; end of file
