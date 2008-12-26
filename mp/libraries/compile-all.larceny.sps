;;;
;;;Part of: Nausicaa/MP
;;;Contents: compile script
;;;Date: Fri Dec 26, 2008
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

(import (rnrs)
  (larceny compiler))

(compile-library "mp/sizeof.sls"	"mp/sizeof.larceny.slfasl")
(compile-library "mp/mpz.sls"		"mp/mpz.larceny.slfasl")
(compile-library "mp/mpq.sls"		"mp/mpq.larceny.slfasl")
(compile-library "mp/mpf.sls"		"mp/mpf.larceny.slfasl")
(compile-library "mp/random.sls"	"mp/random.larceny.slfasl")
(compile-library "mp/mpfr.sls"		"mp/mpfr.larceny.slfasl")
(compile-library "mp/mpfi.sls"		"mp/mpfi.larceny.slfasl")
(compile-library "mp/mpc.sls"		"mp/mpc.larceny.slfasl")

;;; end of file
