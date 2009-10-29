;;;
;;;Part of: Nausicaa/Template
;;;Contents: compile script for Larceny Scheme
;;;Date: Thu Dec 25, 2008
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

(compile-library "foreign/template/sizeof.sls"
		 "foreign/template/sizeof.larceny.slfasl")

(compile-library "foreign/template/shared-object.sls"
		 "foreign/template/shared-object.larceny.slfasl")

(compile-library "foreign/template/platform.sls"
		 "foreign/template/platform.larceny.slfasl")

(compile-library "foreign/template/primitives.sls"
		 "foreign/template/primitives.larceny.slfasl")

(compile-library "foreign/template.sls"
		 "foreign/template.larceny.slfasl")

;;; end of file
