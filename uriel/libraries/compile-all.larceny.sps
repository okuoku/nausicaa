;;;
;;;Part of: Nausicaa/Uriel
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

(compile-library "uriel/lang.sls"
		 "uriel/lang.larceny.slfasl")

(compile-library "uriel/combinators.sls"
		 "uriel/combinators.larceny.slfasl")

(compile-library "uriel/define-macro.sls"
		 "uriel/define-macro.larceny.slfasl")

(compile-library "uriel/ffi/sizeof.sls"
		 "uriel/ffi/sizeof.larceny.slfasl")

(compile-library "uriel/memory/compat.larceny.sls"
		 "uriel/memory/compat.larceny.slfasl")

(compile-library "uriel/memory.sls"
		 "uriel/memory.larceny.slfasl")

(compile-library "uriel/ffi/compat.larceny.sls"
		 "uriel/ffi/compat.larceny.slfasl")

(compile-library "uriel/ffi.sls"
		 "uriel/ffi.larceny.slfasl")

(compile-library "uriel/errno.sls"
		 "uriel/errno.larceny.slfasl")

(compile-library "uriel/cstring.sls"
		 "uriel/cstring.larceny.slfasl")

(compile-library "uriel/foreign.sls"
		 "uriel/foreign.larceny.slfasl")

(compile-library "uriel/object-property.sls"
		 "uriel/object-property.larceny.slfasl")

(compile-library "uriel/test.sls"
		 "uriel/test.larceny.slfasl")

;;; end of file
