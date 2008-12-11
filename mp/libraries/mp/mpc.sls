;;;
;;;Part of: Nausicaa/MP
;;;Contents: interface to MPC
;;;Date: Wed Dec 10, 2008
;;;Time-stamp: <2008-12-11 21:36:52 marco>
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

(library (mp mpc)
  (export
    mpc_abs
    mpc_add
    mpc_add_fr
    mpc_add_ui
    mpc_arg
    mpc_clear
    mpc_cmp
    mpc_cmp_si_si
    mpc_conj
    mpc_cos
    mpc_cosh
    mpc_div
    mpc_div_2exp
    mpc_div_fr
    mpc_div_ui
    mpc_exp
    mpc_fr_div
    mpc_fr_sub
    mpc_get_default_prec
    mpc_get_prec
    mpc_get_prec2
    mpc_get_version
    mpc_imag
    mpc_init
    mpc_init2
    mpc_init3
    mpc_inp_str
    mpc_log
    mpc_mul
    mpc_mul_2exp
    mpc_mul_fr
    mpc_mul_i
    mpc_mul_si
    mpc_mul_ui
    mpc_neg
    mpc_norm
    mpc_out_str
    mpc_proj
    mpc_random
    mpc_random2
    mpc_real
    mpc_set
    mpc_set_d_d
    mpc_set_default_prec
    mpc_set_fr
    mpc_set_fr_fr
    mpc_set_prec
    mpc_set_si_si
    mpc_set_ui_fr
    mpc_set_ui_ui
    mpc_sin
    mpc_sinh
    mpc_sqr
    mpc_sqrt
    mpc_sub
    mpc_sub_fr
    mpc_sub_ui
    mpc_tan
    mpc_tanh
    mpc_ui_div
    mpc_ui_ui_sub
    mpc_urandom

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
    (uriel lang)
    (uriel ffi)
    (uriel ffi sizeof)
    (mp sizeof)
    (mp mpfr))

  (define mpc-lib
    (let ((o (open-shared-object 'libmpc.so)))
      (shared-object o)
      o))



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

(define-c-function mpc_add
  (int mpc_add (mpc_ptr mpc_srcptr mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_add_fr
  (int mpc_add_fr (mpc_ptr mpc_srcptr mpfr_srcptr mpc_rnd_t)))
(define-c-function mpc_add_ui
  (int mpc_add_ui (mpc_ptr mpc_srcptr unsigned-long mpc_rnd_t)))
(define-c-function mpc_sub
  (int mpc_sub (mpc_ptr mpc_srcptr mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_sub_fr
  (int mpc_sub_fr (mpc_ptr mpc_srcptr mpfr_srcptr mpc_rnd_t)))
(define-c-function mpc_fr_sub
  (int mpc_fr_sub (mpc_ptr mpfr_srcptr mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_sub_ui
  (int mpc_sub_ui (mpc_ptr mpc_srcptr unsigned-long mpc_rnd_t)))
(define-c-function mpc_ui_ui_sub
  (int mpc_ui_ui_sub (mpc_ptr unsigned-long unsigned-long mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_mul
  (int mpc_mul (mpc_ptr mpc_srcptr mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_mul_fr
  (int mpc_mul_fr (mpc_ptr mpc_srcptr mpfr_srcptr mpc_rnd_t)))
(define-c-function mpc_mul_ui
  (int mpc_mul_ui (mpc_ptr mpc_srcptr unsigned-long mpc_rnd_t)))
(define-c-function mpc_mul_si
  (int mpc_mul_si (mpc_ptr mpc_srcptr long int mpc_rnd_t)))
(define-c-function mpc_mul_i
  (int mpc_mul_i (mpc_ptr mpc_srcptr int mpc_rnd_t)))
(define-c-function mpc_sqr
  (int mpc_sqr (mpc_ptr mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_div
  (int mpc_div (mpc_ptr mpc_srcptr mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_div_fr
  (int mpc_div_fr (mpc_ptr mpc_srcptr mpfr_srcptr mpc_rnd_t)))
(define-c-function mpc_fr_div
  (int mpc_fr_div (mpc_ptr mpfr_srcptr mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_div_ui
  (int mpc_div_ui (mpc_ptr mpc_srcptr unsigned-long mpc_rnd_t)))
(define-c-function mpc_ui_div
  (int mpc_ui_div (mpc_ptr unsigned-long mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_div_2exp
  (int mpc_div_2exp (mpc_ptr mpc_srcptr unsigned-long mpc_rnd_t)))
(define-c-function mpc_mul_2exp
  (int mpc_mul_2exp (mpc_ptr mpc_srcptr unsigned-long mpc_rnd_t)))
(define-c-function mpc_conj
  (int mpc_conj (mpc_ptr mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_neg
  (int mpc_neg (mpc_ptr mpc_srcptr mpc_rnd_t)))
(define-c-function  mpc_norm
  (int mpc_norm (mpfr_ptr mpc_srcptr mp_rnd_t)))
(define-c-function mpc_abs
  (int mpc_abs (mpfr_ptr mpc_srcptr mp_rnd_t)))
(define-c-function  mpc_sqrt
  (int mpc_sqrt (mpc_ptr mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_set
  (int mpc_set (mpc_ptr mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_set_d_d
  (int mpc_set_d_d (mpc_ptr double double mpc_rnd_t)))
(define-c-function mpc_set_fr
  (int mpc_set_fr (mpc_ptr mpfr_srcptr mpc_rnd_t)))
(define-c-function mpc_set_fr_fr
  (int mpc_set_fr_fr (mpc_ptr mpfr_srcptr mpfr_srcptr mpc_rnd_t)))
(define-c-function mpc_set_ui_fr
  (int mpc_set_ui_fr (mpc_ptr unsigned-long mpfr_srcptr mpc_rnd_t)))

;;FIXME (Thu  Dec 11, 2008) For no  reason I can figure  out now, Ikarus
;;fails if I use the MPC's own  functions: the real part is set, but the
;;imaginary part  is left to zero.   Ypsilon works fine.   It seems that
;;there is some  problem with the Ikarus FFI,  because the problem shows
;;itself also when I access the functions with:
;;
;; (define lib (dlopen "libmpc.so"))
;; (define f (make-c-callout 'signed-int
;;		'(pointer unsigned-long unsigned-long signed-int)))
;; (define g (f (dlsym lib "mpc_set_si_si")))
;;
;;I  have no will  to investicate  the problem  now, so  the replacement
;;functions below are exported in place of the MPC's originals.
;;
;; (define-c-function mpc_set_ui_ui
;;   (int mpc_set_ui_ui (mpc_ptr unsigned-long unsigned-long mpc_rnd_t)))
;; (define-c-function mpc_set_si_si
;;   (int mpc_set_si_si (mpc_ptr signed-long signed-long mpc_rnd_t)))
(define (mpc_set_ui_ui cplx real-int imag-int round)
  (MPC_INEX (mpfr_set_ui (struct-mpc-re-ref cplx) real-int (MPC_RND_RE round))
	    (mpfr_set_ui (struct-mpc-im-ref cplx) imag-int (MPC_RND_IM round))))
(define (mpc_set_si_si cplx real-int imag-int round)
  (MPC_INEX (mpfr_set_si (struct-mpc-re-ref cplx) real-int (MPC_RND_RE round))
	    (mpfr_set_si (struct-mpc-im-ref cplx) imag-int (MPC_RND_IM round))))


(define-c-function mpc_real
  (int mpc_real (mpfr_ptr mpc_srcptr mpfr_rnd_t)))
(define-c-function mpc_imag
  (int mpc_imag (mpfr_ptr mpc_srcptr mpfr_rnd_t)))
(define-c-function mpc_arg
  (int mpc_arg (mpfr_ptr mpc_srcptr mpfr_rnd_t)))
(define-c-function mpc_proj
  (int mpc_proj (mpc_ptr mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_cmp
  (int mpc_cmp (mpc_srcptr mpc_srcptr)))
(define-c-function mpc_cmp_si_si
  (int mpc_cmp_si_si (mpc_srcptr long int long int)))
(define-c-function mpc_exp
  (void mpc_exp (mpc_ptr mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_log
  (void mpc_log (mpc_ptr mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_sin
  (void mpc_sin (mpc_ptr mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_cos
  (void mpc_cos (mpc_ptr mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_tan
  (void mpc_tan (mpc_ptr mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_sinh
  (void mpc_sinh (mpc_ptr mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_cosh
  (void mpc_cosh (mpc_ptr mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_tanh
  (void mpc_tanh (mpc_ptr mpc_srcptr mpc_rnd_t)))
(define-c-function mpc_clear
  (void mpc_clear (mpc_ptr)))
(define-c-function mpc_init
  (void mpc_init (mpc_ptr)))
(define-c-function mpc_random
  (void mpc_random (mpc_ptr)))
(define-c-function mpc_random2
  (void mpc_random2 (mpc_ptr mp_size_t mp_exp_t)))
(define-c-function mpc_urandom
  (int mpc_urandom (mpc_ptr gmp_randstate_t)))
(define-c-function mpc_init2
  (void mpc_init2 (mpc_ptr mp_prec_t)))
(define-c-function mpc_init3
  (void mpc_init3 (mpc_ptr mp_prec_t mp_prec_t)))
(define-c-function mpc_get_prec
  (mp_prec_t mpc_get_prec (mpc_t)))
(define-c-function mpc_get_prec2
  (void mpc_get_prec2 (mp_prec_t* mp_prec_t* mpc_t)))
(define-c-function mpc_set_prec
  (void mpc_set_prec (mpc_ptr mp_prec_t)))
(define-c-function mpc_set_default_prec
  (void mpc_set_default_prec (mp_prec_t)))
(define-c-function mpc_get_default_prec
  (mp_prec_t mpc_get_default_prec (void)))
(define-c-function mpc_get_version
  (char* mpc_get_version (void)))
(define-c-function mpc_inp_str
  (size_t mpc_inp_str (mpc_ptr FILE* int mpc_rnd_t)))
(define-c-function mpc_out_str
  (size_t mpc_out_str (FILE* int size_t mpc_srcptr mpc_rnd_t)))



;;;; done

)

;;; end of file
