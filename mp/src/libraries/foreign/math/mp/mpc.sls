;;;
;;;Part of: Nausicaa/MP
;;;Contents: interface to MPC
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


(library (foreign math mp mpc)
  (export
    mpc_abs
    mpc_add
    mpc_add_fr
    mpc_add_ui
    mpc_arg
    mpc_cmp
    mpc_cmp_si_si
    mpc_conj
    mpc_cos
    mpc_cosh
    mpc_div
    mpc_pow	;added in MPC 0.8
    mpc_pow_fr	;added in MPC 0.8
;;;The type "long  double" is not supported by  the underlying Scheme
;;;implementations.
;;;
;;;mpc_pow_ld  ;added in MPC 0.8
    mpc_pow_d	;added in MPC 0.8
    mpc_pow_si  ;added in MPC 0.8
    mpc_pow_ui  ;added in MPC 0.8
    mpc_pow_z	;added in MPC 0.8
    mpc_div_2exp
    mpc_div_fr
    mpc_div_ui
    mpc_exp
    mpc_fr_div
    mpc_fr_sub
    mpc_imag
    mpc_log
    mpc_mul
    mpc_mul_2exp
    mpc_mul_fr
    mpc_mul_i
    mpc_mul_si
    mpc_mul_ui
    mpc_neg
    mpc_norm
    mpc_proj
;;;    mpc_random
;;;    mpc_random2
    mpc_real
    mpc_set
    mpc_set_d	;added in MPC 0.8
    mpc_set_d_d	;added in MPC 0.8
;;;The  type "long  double" is  not supported  by the  underlying Scheme
;;;implementations.
;;;
;;;    mpc_set_ld		;added in MPC 0.8
;;;    mpc_set_ld_ld		;added in MPC 0.8
    mpc_set_f	;added in MPC 0.8
    mpc_set_f_f	;added in MPC 0.8
;;;    mpc_set_default_prec
    mpc_set_fr
    mpc_set_fr_fr
    mpc_set_q	;added in MPC 0.8
    mpc_set_q_q	;added in MPC 0.8
    mpc_set_si	;added in MPC 0.8
    mpc_set_si_si
;;;    mpc_set_ui_fr
    mpc_set_ui	;added in MPC 0.8
    mpc_set_ui_ui
    mpc_set_z	;added in MPC 0.8
    mpc_set_z_z	;added in MPC 0.8
    mpc_swap	;added in MPC 0.8
    mpc_set_nan	;added in MPC 0.8
    mpc_sin
    mpc_sinh
    mpc_sqr
    mpc_sqrt
    mpc_sub
    mpc_sub_fr
    mpc_sub_ui
    mpc_tan
    mpc_tanh
    mpc_asin	;added in MPC 0.8
    mpc_acos	;added in MPC 0.8
    mpc_atan	;added in MPC 0.8
    mpc_asinh	;added in MPC 0.8
    mpc_acosh	;added in MPC 0.8
    mpc_atanh	;added in MPC 0.8
    mpc_clear
    mpc_urandom
    mpc_init
    mpc_init2
    mpc_init3
;;;    mpc_get_default_prec
    mpc_get_prec
    mpc_get_prec2
    mpc_set_prec
    mpc_get_version

    mpc_strtoc	 ;added in MPC 0.8
    mpc_set_str	 ;added in MPC 0.8
    mpc_get_str	 ;added in MPC 0.8
    mpc_free_str ;added in MPC 0.8

    mpc_inp_str
    mpc_out_str

    mpc_ui_div
    mpc_ui_ui_sub

    MPC_RNDNN
    MPC_RNDNZ
    MPC_RNDNU
    MPC_RNDND

    MPC_RNDZN
    MPC_RNDZZ
    MPC_RNDZU
    MPC_RNDZD

    MPC_RNDUN
    MPC_RNDUZ
    MPC_RNDUU
    MPC_RNDUD

    MPC_RNDDN
    MPC_RNDDZ
    MPC_RNDDU
    MPC_RNDDD

    MPC_RND_RE MPC_RND_IM MPC_INEX_POS MPC_INEX_NEG MPC_INEX
    MPC_INEX_RE MPC_INEX_IM)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign math mp sizeof)
    (foreign math mp mpfr))

  (define-shared-object mpc-shared-object
    MPC_SHARED_OBJECT)


;;;; type aliases

(define char**		'pointer)


;;;; constants

(define (RNDC r1 r2)
  (+ r1 (bitwise-arithmetic-shift-left r2 4)))

(define (MPC_RND_RE o)
  (bitwise-and o #x0F))

(define (MPC_RND_IM o)
  (bitwise-arithmetic-shift-right o 4))


(define MPC_RNDNN (RNDC GMP_RNDN GMP_RNDN))
(define MPC_RNDNZ (RNDC GMP_RNDN GMP_RNDZ))
(define MPC_RNDNU (RNDC GMP_RNDN GMP_RNDU))
(define MPC_RNDND (RNDC GMP_RNDN GMP_RNDD))

(define MPC_RNDZN (RNDC GMP_RNDZ GMP_RNDN))
(define MPC_RNDZZ (RNDC GMP_RNDZ GMP_RNDZ))
(define MPC_RNDZU (RNDC GMP_RNDZ GMP_RNDU))
(define MPC_RNDZD (RNDC GMP_RNDZ GMP_RNDD))

(define MPC_RNDUN (RNDC GMP_RNDU GMP_RNDN))
(define MPC_RNDUZ (RNDC GMP_RNDU GMP_RNDZ))
(define MPC_RNDUU (RNDC GMP_RNDU GMP_RNDU))
(define MPC_RNDUD (RNDC GMP_RNDU GMP_RNDD))

(define MPC_RNDDN (RNDC GMP_RNDD GMP_RNDN))
(define MPC_RNDDZ (RNDC GMP_RNDD GMP_RNDZ))
(define MPC_RNDDU (RNDC GMP_RNDD GMP_RNDU))
(define MPC_RNDDD (RNDC GMP_RNDD GMP_RNDD))

(define (MPC_INEX_POS inex)
  (cond ((< inex 0) 2)
	((= inex 0) 0)
	(else 1)))

(define (MPC_INEX_NEG inex)
  (cond ((= inex 2) -1)
	((= inex 0) 0)
	(else 1)))

(define (MPC_INEX inex_re inex_im)
  (bitwise-ior (MPC_INEX_POS inex_re)
	       (bitwise-arithmetic-shift-left (MPC_INEX_POS inex_im) 2)))

(define (MPC_INEX_RE inex)
  (MPC_INEX_NEG (bitwise-and inex 3)))

(define (MPC_INEX_IM inex)
  (MPC_INEX_NEG (bitwise-arithmetic-shift-right inex 2)))


;;;; functions

(define-c-functions mpc-shared-object
  (mpc_add
   (int mpc_add (mpc_ptr mpc_srcptr mpc_srcptr mpc_rnd_t)))
  (mpc_add_fr
   (int mpc_add_fr (mpc_ptr mpc_srcptr mpfr_srcptr mpc_rnd_t)))
  (mpc_add_ui
   (int mpc_add_ui (mpc_ptr mpc_srcptr unsigned-long mpc_rnd_t)))
  (mpc_sub
   (int mpc_sub (mpc_ptr mpc_srcptr mpc_srcptr mpc_rnd_t)))
  (mpc_sub_fr
   (int mpc_sub_fr (mpc_ptr mpc_srcptr mpfr_srcptr mpc_rnd_t)))
  (mpc_fr_sub
   (int mpc_fr_sub (mpc_ptr mpfr_srcptr mpc_srcptr mpc_rnd_t)))
  (mpc_sub_ui
   (int mpc_sub_ui (mpc_ptr mpc_srcptr unsigned-long mpc_rnd_t)))
  (mpc_ui_ui_sub
   (int mpc_ui_ui_sub (mpc_ptr unsigned-long unsigned-long mpc_srcptr mpc_rnd_t)))
  (mpc_mul
   (int mpc_mul (mpc_ptr mpc_srcptr mpc_srcptr mpc_rnd_t)))
  (mpc_mul_fr
   (int mpc_mul_fr (mpc_ptr mpc_srcptr mpfr_srcptr mpc_rnd_t)))
  (mpc_mul_ui
   (int mpc_mul_ui (mpc_ptr mpc_srcptr unsigned-long mpc_rnd_t)))
  (mpc_mul_si
   (int mpc_mul_si (mpc_ptr mpc_srcptr long int mpc_rnd_t)))
  (mpc_mul_i
   (int mpc_mul_i (mpc_ptr mpc_srcptr int mpc_rnd_t)))
  (mpc_sqr
   (int mpc_sqr (mpc_ptr mpc_srcptr mpc_rnd_t)))
  (mpc_div
   (int mpc_div (mpc_ptr mpc_srcptr mpc_srcptr mpc_rnd_t)))
  (mpc_pow
   (int mpc_pow (mpc_ptr mpc_srcptr mpc_srcptr mpc_rnd_t)))
  (mpc_pow_fr
   (int mpc_pow_fr (mpc_ptr mpc_srcptr mpfr_srcptr mpc_rnd_t)))
;;;The type "long  double" is not supported by  the underlying Scheme
;;;implementations.
;;;
;;;   (mpc_pow_ld
;;;   (int mpc_pow_ld (mpc_ptr mpc_srcptr long-double mpc_rnd_t)))
;;;
  (mpc_pow_d
   (int mpc_pow_d (mpc_ptr mpc_srcptr double mpc_rnd_t)))
  (mpc_pow_si
   (int mpc_pow_si (mpc_ptr mpc_srcptr long mpc_rnd_t)))
  (mpc_pow_ui
   (int mpc_pow_ui (mpc_ptr mpc_srcptr unsigned-long mpc_rnd_t)))
  (mpc_pow_z
   (int mpc_pow_z (mpc_ptr mpc_srcptr mpz_srcptr mpc_rnd_t)))
  (mpc_div_fr
   (int mpc_div_fr (mpc_ptr mpc_srcptr mpfr_srcptr mpc_rnd_t)))
  (mpc_fr_div
   (int mpc_fr_div (mpc_ptr mpfr_srcptr mpc_srcptr mpc_rnd_t)))
  (mpc_div_ui
   (int mpc_div_ui (mpc_ptr mpc_srcptr unsigned-long mpc_rnd_t)))
  (mpc_ui_div
   (int mpc_ui_div (mpc_ptr unsigned-long mpc_srcptr mpc_rnd_t)))
  (mpc_div_2exp
   (int mpc_div_2exp (mpc_ptr mpc_srcptr unsigned-long mpc_rnd_t)))
  (mpc_mul_2exp
   (int mpc_mul_2exp (mpc_ptr mpc_srcptr unsigned-long mpc_rnd_t)))
  (mpc_conj
   (int mpc_conj (mpc_ptr mpc_srcptr mpc_rnd_t)))
  (mpc_neg
   (int mpc_neg (mpc_ptr mpc_srcptr mpc_rnd_t)))
  ( mpc_norm
    (int mpc_norm (mpfr_ptr mpc_srcptr mp_rnd_t)))
  (mpc_abs
   (int mpc_abs (mpfr_ptr mpc_srcptr mp_rnd_t)))
  ( mpc_sqrt
    (int mpc_sqrt (mpc_ptr mpc_srcptr mpc_rnd_t)))
  (mpc_set
   (int mpc_set (mpc_ptr mpc_srcptr mpc_rnd_t)))
  (mpc_set_d
   (int mpc_set_d (mpc_ptr double mpc_rnd_t)))
  (mpc_set_d_d
   (int mpc_set_d_d (mpc_ptr double double mpc_rnd_t)))
;;;The type "long  double" is not supported by  the underlying Scheme
;;;implementations.
;;;
;;;  (mpc_set_ld
;;;  (int mpc_set_ld (mpc_ptr long-double mpc_rnd_t)))
;;;  (mpc_set_ld_ld
;;;  (int mpc_set_ld_ld (mpc_ptr long-double long-double mpc_rnd_t)))
  (mpc_set_f
   (int mpc_set_f (mpc_ptr mpf_srcptr mpc_rnd_t)))
  (mpc_set_f_f
   (int mpc_set_f_f (mpc_ptr mpf_srcptr mpf_srcptr mpc_rnd_t)))
  (mpc_set_fr
   (int mpc_set_fr (mpc_ptr mpfr_srcptr mpc_rnd_t)))
  (mpc_set_fr_fr
   (int mpc_set_fr_fr (mpc_ptr mpfr_srcptr mpfr_srcptr mpc_rnd_t)))
  (mpc_set_q
   (int mpc_set_q (mpc_ptr mpq_srcptr mpc_rnd_t)))
  (mpc_set_q_q
   (int mpc_set_q_q (mpc_ptr mpq_srcptr mpq_srcptr mpc_rnd_t)))
  (mpc_set_si
   (int mpc_set_si (mpc_ptr long mpc_rnd_t)))
  (mpc_set_si_si
   (int mpc_set_si_si (mpc_ptr signed-long signed-long mpc_rnd_t)))
  (mpc_set_ui
   (int mpc_set_ui (mpc_ptr unsigned-long mpc_rnd_t)))
  (mpc_set_ui_ui
   (int mpc_set_ui_ui (mpc_ptr unsigned-long unsigned-long mpc_rnd_t)))
  (mpc_set_z
   (int mpc_set_z (mpc_ptr mpz_srcptr mpc_rnd_t)))
  (mpc_set_z_z
   (int mpc_set_z_z (mpc_ptr mpz_srcptr mpz_srcptr mpc_rnd_t)))
  (mpc_swap
   (void mpc_swap (mpc_ptr mpc_ptr)))
  (mpc_set_nan
   (void mpc_set_nan (mpc_ptr)))
;;;This is not exported in MPC 0.7.
;;;
;;;   (mpc_set_ui_fr
;;;   (int mpc_set_ui_fr (mpc_ptr unsigned-long mpfr_srcptr mpc_rnd_t)))

  (mpc_real
   (int mpc_real (mpfr_ptr mpc_srcptr mpfr_rnd_t)))
  (mpc_imag
   (int mpc_imag (mpfr_ptr mpc_srcptr mpfr_rnd_t)))
  (mpc_arg
   (int mpc_arg (mpfr_ptr mpc_srcptr mpfr_rnd_t)))
  (mpc_proj
   (int mpc_proj (mpc_ptr mpc_srcptr mpc_rnd_t)))
  (mpc_cmp
   (int mpc_cmp (mpc_srcptr mpc_srcptr)))
  (mpc_cmp_si_si
   (int mpc_cmp_si_si (mpc_srcptr long int long int)))
  (mpc_exp
   (void mpc_exp (mpc_ptr mpc_srcptr mpc_rnd_t)))
  (mpc_log
   (void mpc_log (mpc_ptr mpc_srcptr mpc_rnd_t)))
  (mpc_sin
   (void mpc_sin (mpc_ptr mpc_srcptr mpc_rnd_t)))
  (mpc_cos
   (void mpc_cos (mpc_ptr mpc_srcptr mpc_rnd_t)))
  (mpc_tan
   (void mpc_tan (mpc_ptr mpc_srcptr mpc_rnd_t)))
  (mpc_sinh
   (void mpc_sinh (mpc_ptr mpc_srcptr mpc_rnd_t)))
  (mpc_cosh
   (void mpc_cosh (mpc_ptr mpc_srcptr mpc_rnd_t)))
  (mpc_tanh
   (void mpc_tanh (mpc_ptr mpc_srcptr mpc_rnd_t)))
  (mpc_asin
   (int mpc_asin (mpc_ptr mpc_srcptr mpc_rnd_t)))
  (mpc_acos
   (int mpc_acos (mpc_ptr mpc_srcptr mpc_rnd_t)))
  (mpc_atan
   (int mpc_atan (mpc_ptr mpc_srcptr mpc_rnd_t)))
  (mpc_asinh
   (int mpc_asinh (mpc_ptr mpc_srcptr mpc_rnd_t)))
  (mpc_acosh
   (int mpc_acosh (mpc_ptr mpc_srcptr mpc_rnd_t)))
  (mpc_atanh
   (int mpc_atanh (mpc_ptr mpc_srcptr mpc_rnd_t)))
  (mpc_clear
   (void mpc_clear (mpc_ptr)))

;;; The foreign  function "mpc_init()" is not  present in MPC  0.7, so we
;;; define an alias.
;;;
;;;   (mpc_init
;;;   (void mpc_init (mpc_ptr)))
;;;

;;;These are not present in MPC 0.7.
;;;
;;;   (mpc_random
;;;   (void mpc_random (mpc_ptr)))
;;;   (mpc_random2
;;;   (void mpc_random2 (mpc_ptr mp_size_t mp_exp_t)))

  (mpc_urandom
   (int mpc_urandom (mpc_ptr gmp_randstate_t)))

  (mpc_init2
   (void mpc_init2 (mpc_ptr mp_prec_t)))
  (mpc_init3
   (void mpc_init3 (mpc_ptr mp_prec_t mp_prec_t)))
  (mpc_get_prec
   (mp_prec_t mpc_get_prec (mpc_ptr)))
  (mpc_get_prec2
   (void mpc_get_prec2 (mp_prec_t* mp_prec_t* mpc_ptr)))
  (mpc_set_prec
   (void mpc_set_prec (mpc_ptr mp_prec_t)))

;;;These are not present in MPC 0.7.
;;;
;;;   (mpc_set_default_prec
;;;   (void mpc_set_default_prec (mp_prec_t)))
;;;   (mpc_get_default_prec
;;;   (mp_prec_t mpc_get_default_prec (void)))

  (mpc_get_version
   (char* mpc_get_version (void)))

  (mpc_strtoc
   (int mpc_strtoc (mpc_ptr char* char** int mpc_rnd_t)))
  (mpc_set_str
   (int mpc_set_str (mpc_ptr char* int mpc_rnd_t)))
  (mpc_get_str
   (char* mpc_get_str (int size_t mpc_srcptr mpc_rnd_t)))
  (mpc_free_str
   (void mpc_free_str (char*)))


  (mpc_inp_str
   (size_t mpc_inp_str (mpc_ptr FILE* int mpc_rnd_t)))
  (mpc_out_str
   (size_t mpc_out_str (FILE* int size_t mpc_srcptr mpc_rnd_t))))

(define (mpc_init p)
  (mpc_init2 p 256))


;;;; done

)

;;; end of file
