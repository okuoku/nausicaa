;;;
;;;Part of: Nausicaa/MP libraries for R6RS Scheme
;;;Contents: size of libraries' C language types
;;;Date: Tue Nov 25, 2008
;;;Time-stamp: <2008-12-03 10:56:58 marco>
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

(library (mp sizeof)
  (export

    ;; Typedefs.
    mp_exp_t mp_exp_t* mp_size_t mp_limb_t gmp_randalg_t
    gmp_randstate_t
    mpz_srcptr mpz_ptr mpf_srcptr mpf_ptr mpq_srcptr mpq_ptr

    ;; Sizeofs.
    sizeof-mp-size sizeof-mp-exp sizeof-mp-limb
    sizeof-mpz sizeof-mpf sizeof-mpfr sizeof-mpfi sizeof-mpc
    sizeof-gmp-randstate

    ;; Constants.
    GMP_RAND_ALG_DEFAULT GMP_RAND_ALG_LC

    )
  (import (rnrs))

  (define mp_exp_t		(quote int))
  (define mp_size_t		(quote int))
  (define mp_limb_t		(quote int))
  (define gmp_randalg_t		(quote int))

  (define mp_exp_t*		'pointer)
  (define mpz_srcptr		'pointer)
  (define mpz_ptr		'pointer)
  (define mpf_srcptr		'pointer)
  (define mpf_ptr		'pointer)
  (define mpq_srcptr		'pointer)
  (define mpq_ptr		'pointer)
  (define gmp_randstate_t	'pointer)

  (define sizeof-mp-limb	4)
  (define sizeof-mp-size	4)
  (define sizeof-mp-exp		4)
  (define sizeof-gmp-randalg	4)

  (define sizeof-mpz		12)
  (define sizeof-mpq		24)
  (define sizeof-mpf		16)
  (define sizeof-mpfr		16)
  (define sizeof-mpfi		32)
  (define sizeof-mpc		32)
  (define sizeof-gmp-randstate	20)

  (define GMP_RAND_ALG_DEFAULT	0)
  (define GMP_RAND_ALG_LC	0)
  )

;;; end of file
