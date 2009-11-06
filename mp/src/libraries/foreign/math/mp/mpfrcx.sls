;;;
;;;Part of: Nausicaa/MP
;;;Contents: interface to MPFRCX
;;;Date: Fri Nov  6, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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


(library (foreign math mp mpfrcx)
  (export
    mpcx_init			mpfrx_init
    mpcx_set			mpfrx_set
    mpcx_init_set		mpfrx_init_set
    mpcx_clear			mpfrx_clear
    mpcx_realloc		mpfrx_realloc

    mpcx_cmp			mpcx_urandom
    mpfrx_cmp			mpfrx_urandom

    mpcx_add			mpfrx_add
    mpcx_sub			mpfrx_sub
    mpcx_si_sub			mpfrx_si_sub
    mpcx_neg			mpfrx_neg

    mpcx_mul			mpfrx_mul

    mpcx_rem			mpfrx_rem

    mpcx_reconstruct		mpfrx_reconstruct
    mpcx_multieval		mpfrx_multieval
    mpcx_root			mpfrx_root

    mpcx_out_str		mpfrx_out_str

    struct-mpfrx_t-coeff-ptr-ref	struct-mpcx_t-coeff-ptr-ref)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign math mp sizeof)
    (foreign math mp mpfr)
    (foreign math mp mpc))

  (define mpfrcx-lib
    (let ((o (open-shared-object* 'libmpfrcx.so)))
      (shared-object o)
      o))


;;;; type aliases

(define char**		'pointer)
(define mpfrx_t*	'pointer)
(define mpcx_t*		'pointer)
(define mpfr_t*		'pointer)
(define mpc_t*		'pointer)


;;;; constants



;;;; accessors

(define-syntax struct-mpfrx_t-coeff-ptr-ref
  (syntax-rules ()
    ((_ ?polynomial ?index)
     (array-ref-c-mpfrx_t (struct-mpcx_t-coeff-ref ?polynomial) ?index))))

(define-syntax struct-mpcx_t-coeff-ptr-ref
  (syntax-rules ()
    ((_ ?polynomial ?index)
     (array-ref-c-mpcx_t (struct-mpcx_t-coeff-ref ?polynomial) ?index))))




;;;; functions

(define-c-function mpcx_init
  (void mpcx_init (mpcx_ptr int mp_prec_t)))
(define-c-function mpcx_set
  (void mpcx_set (mpcx_ptr mpcx_srcptr)))
(define-c-function mpcx_init_set
  (void mpcx_init_set (mpcx_ptr mpcx_srcptr)))
(define-c-function mpcx_clear
  (void mpcx_clear (mpcx_ptr)))
(define-c-function mpcx_realloc
  (void mpcx_realloc (mpcx_ptr int)))

(define-c-function mpfrx_init
  (void mpfrx_init (mpfrx_ptr int mp_prec_t)))
(define-c-function mpfrx_set
  (void mpfrx_set (mpfrx_ptr mpfrx_srcptr)))
(define-c-function mpfrx_init_set
  (void mpfrx_init_set (mpfrx_ptr mpfrx_srcptr)))
(define-c-function mpfrx_clear
  (void mpfrx_clear (mpfrx_ptr)))
(define-c-function mpfrx_realloc
  (void mpfrx_realloc (mpfrx_ptr int)))

(define-c-function mpcx_cmp
  (int mpcx_cmp (mpcx_srcptr mpcx_srcptr)))
(define-c-function mpcx_urandom
  (void mpcx_urandom (mpcx_ptr int gmp_randstate_t)))
(define-c-function mpfrx_cmp
  (int mpfrx_cmp (mpfrx_srcptr mpfrx_srcptr)))
(define-c-function mpfrx_urandom
  (void mpfrx_urandom (mpfrx_ptr int gmp_randstate_t)))

(define-c-function mpcx_add
  (void mpcx_add (mpcx_ptr mpcx_srcptr mpcx_srcptr)))
(define-c-function mpcx_sub
  (void mpcx_sub (mpcx_ptr mpcx_srcptr mpcx_srcptr)))
(define-c-function mpcx_si_sub
  (void mpcx_si_sub (mpcx_ptr long mpcx_srcptr)))
(define-c-function mpcx_neg
  (void mpcx_neg (mpcx_ptr mpcx_srcptr)))

(define-c-function mpfrx_add
  (void mpfrx_add (mpfrx_ptr mpfrx_srcptr mpfrx_srcptr)))
(define-c-function mpfrx_sub
  (void mpfrx_sub (mpfrx_ptr mpfrx_srcptr mpfrx_srcptr)))
(define-c-function mpfrx_si_sub
  (void mpfrx_si_sub (mpfrx_ptr long mpfrx_srcptr)))
(define-c-function mpfrx_neg
  (void mpfrx_neg (mpfrx_ptr mpfrx_srcptr)))

(define-c-function mpcx_mul
  (void mpcx_mul (mpcx_ptr mpcx_srcptr mpcx_srcptr)))

(define-c-function mpfrx_mul
  (void mpfrx_mul (mpfrx_ptr mpfrx_srcptr mpfrx_srcptr)))


(define-c-function mpcx_rem
  (void mpcx_rem (mpcx_ptr mpcx_srcptr mpcx_srcptr)))

(define-c-function mpfrx_rem
  (void mpfrx_rem (mpfrx_ptr mpfrx_srcptr mpfrx_srcptr)))

(define-c-function mpcx_reconstruct
  (void mpcx_reconstruct (mpcx_t mpcx_t* int)))
(define-c-function mpcx_multieval
  (void mpcx_multieval (mpc_t* mpc_t* int mpcx_srcptr)))
(define-c-function mpcx_root
  (void mpcx_root (mpc_t mpcx_t)))

(define-c-function mpfrx_reconstruct
  (void mpfrx_reconstruct (mpfrx_t mpfrx_t* int)))
(define-c-function mpfrx_multieval
  (void mpfrx_multieval (mpfr_t* mpfr_t* int mpfrx_srcptr)))
(define-c-function mpfrx_root
  (void mpfrx_root (mpfr_t mpfrx_t)))

(define-c-function mpcx_out_str
  (size_t mpcx_out_str (FILE* int size_t mpcx_srcptr)))

(define-c-function mpfrx_out_str
  (size_t mpfrx_out_str (FILE* int size_t mpfrx_srcptr)))


;;;; done

)

;;; end of file
