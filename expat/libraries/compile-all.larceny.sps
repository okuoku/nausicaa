;;;
;;;Part of: Nausicaa/Expat
;;;Contents: compile script
;;;Date: Sat Jan  3, 2009
;;;
;;;Abstract
;;;
;;;	Order  does matter!!!  Libraries  that are  imported into  other
;;;	libraries must be compiled first.
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

(import (rnrs)
  (larceny compiler))

(compile-library "expat/sizeof.sls"
		 "expat/sizeof.larceny.slfasl")

(compile-library "expat/platform.sls"
		 "expat/platform.larceny.slfasl")

(compile-library "expat.sls"
		 "expat.larceny.slfasl")


;;; end of file
