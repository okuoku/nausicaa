;;;
;;;Part of: Nausicaa/MP
;;;Contents: interface to GMP, random integers functions
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


(library (foreign math mp random)
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
    gmp_urandomm_ui)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign math mp sizeof))

  (define-shared-object gmp-shared-object
    GMP_SHARED_OBJECT)


(define-c-functions gmp-shared-object
  (gmp_randinit_default
   (void __gmp_randinit_default (gmp_randstate_t)))

  (gmp_randinit_lc_2exp
   (void __gmp_randinit_lc_2exp (gmp_randstate_t mpz_srcptr ulong ulong)))

  (gmp_randinit_lc_2exp_size
   (int __gmp_randinit_lc_2exp_size (gmp_randstate_t ulong)))

  (gmp_randinit_mt
   (void __gmp_randinit_mt (gmp_randstate_t)))

  (gmp_randinit_set
   (void __gmp_randinit_set (gmp_randstate_t gmp_randstate_t)))

  (gmp_randseed
   (void __gmp_randseed (gmp_randstate_t mpz_srcptr)))

  (gmp_randseed_ui
   (void __gmp_randseed_ui (gmp_randstate_t ulong)))

  (gmp_randclear
   (void __gmp_randclear (gmp_randstate_t)))

  (gmp_urandomb_ui
   (ulong __gmp_urandomb_ui (gmp_randstate_t ulong)))

  (gmp_urandomm_ui
   (ulong __gmp_urandomm_ui (gmp_randstate_t ulong))))


;;;; done

)

;;; end of file
