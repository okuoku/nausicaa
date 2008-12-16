;;;
;;;Part of: Nausicaa/MP
;;;Contents: interface to GMP, random integers functions
;;;Date: Fri Nov 28, 2008
;;;Time-stamp: <2008-12-16 10:03:57 marco>
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

(library (mp random)
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
  (import (r6rs)
    (uriel printing)
    (uriel ffi)
    (mp sizeof))

  (define gmp-lib
    (let ((o (open-shared-object 'libgmp.so)))
      (shared-object o)
      o))

  (define-c-function gmp_randinit_default
    (void __gmp_randinit_default (gmp_randstate_t)))

  (define-c-function gmp_randinit_lc_2exp
    (void __gmp_randinit_lc_2exp (gmp_randstate_t mpz_srcptr ulong ulong)))

  (define-c-function gmp_randinit_lc_2exp_size
    (int __gmp_randinit_lc_2exp_size (gmp_randstate_t ulong)))

  (define-c-function gmp_randinit_mt
    (void __gmp_randinit_mt (gmp_randstate_t)))

  (define-c-function gmp_randinit_set
    (void __gmp_randinit_set (gmp_randstate_t gmp_randstate_t)))

  (define-c-function gmp_randseed
    (void __gmp_randseed (gmp_randstate_t mpz_srcptr)))

  (define-c-function gmp_randseed_ui
    (void __gmp_randseed_ui (gmp_randstate_t ulong)))

  (define-c-function gmp_randclear
    (void __gmp_randclear (gmp_randstate_t)))

  (define-c-function gmp_urandomb_ui
    (ulong __gmp_urandomb_ui (gmp_randstate_t ulong)))

  (define-c-function gmp_urandomm_ui
    (ulong __gmp_urandomm_ui (gmp_randstate_t ulong))))

;;; end of file
