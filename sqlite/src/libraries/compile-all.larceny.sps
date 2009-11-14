;;;
;;;Part of: Nausicaa/SQLite
;;;Contents: compile script for Larceny Scheme
;;;Date: Mon Nov  2, 2009
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

(compile-library "foreign/databases/sqlite/sizeof.sls"
		 "foreign/databases/sqlite/sizeof.larceny.slfasl")

(compile-library "foreign/databases/sqlite/shared-object.sls"
		 "foreign/databases/sqlite/shared-object.larceny.slfasl")

(compile-library "foreign/databases/sqlite/platform.sls"
		 "foreign/databases/sqlite/platform.larceny.slfasl")

(compile-library "foreign/databases/sqlite/conditions.sls"
		 "foreign/databases/sqlite/conditions.larceny.slfasl")

(compile-library "foreign/databases/sqlite/enumerations.sls"
		 "foreign/databases/sqlite/enumerations.larceny.slfasl")

(compile-library "foreign/databases/sqlite/primitives.sls"
		 "foreign/databases/sqlite/primitives.larceny.slfasl")

(compile-library "foreign/databases/sqlite.sls"
		 "foreign/databases/sqlite.larceny.slfasl")

(compile-library "foreign/databases/sqlite/compensated.sls"
		 "foreign/databases/sqlite/compensated.larceny.slfasl")

;;; end of file
