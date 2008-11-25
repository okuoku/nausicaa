;;;
;;;Part of: Nausicaa/MP libraries for R6RS Scheme
;;;Contents: size of libraries' C language types
;;;Date: Tue Nov 25, 2008
;;;Time-stamp: <2008-11-25 22:14:12 marco>
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
    sizeof-mpz sizeof-mpf sizeof-mpfr sizeof-mpfi sizeof-mpc)
  (import (rnrs))

  (define sizeof-mpz	12)
  (define sizeof-mpf	16)
  (define sizeof-mpfr	16)
  (define sizeof-mpfi	32)
  (define sizeof-mpc	32)
  )

;;; end of file
