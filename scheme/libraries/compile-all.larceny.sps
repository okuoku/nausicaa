;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compile script for Larceny
;;;Date: Mon Jan 19, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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

(compile-library "scheme/unimplemented.sls"
		 "scheme/unimplemented.slfasl")

(compile-library "scheme/compat.larceny.sls"
		 "scheme/compat.larceny.slfasl")

(compile-library "scheme/registry.sls"
		 "scheme/registry.slfasl")

(compile-library "scheme.sls"
		 "scheme.slfasl")

(compile-library "format.sls"
		 "format.slfasl")

(compile-library "lists.sls"
		 "lists.slfasl")

(compile-library "char-sets.sls"
		 "char-sets.slfasl")

;; (compile-library "strings.sls"
;; 		 "strings.slfasl")

(compile-library "loops.sls"
		 "loops.slfasl")

(compile-library "checks.sls"
		 "checks.slfasl")

;;; end of file
