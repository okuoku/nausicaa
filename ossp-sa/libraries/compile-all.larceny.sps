;;;
;;;Part of: Nausicaa/OSSP/sa
;;;Contents: compile script
;;;Date: Mon Dec 29, 2008
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

(compile-library "ossp-sa/sizeof.sls"
		 "ossp-sa/sizeof.larceny.slfasl")

(compile-library "ossp-sa/stub.sls"
		 "ossp-sa/stub.larceny.slfasl")

(compile-library "ossp-sa.sls"
		 "ossp-sa.larceny.slfasl")

;;; end of file
