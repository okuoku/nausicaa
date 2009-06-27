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

(compile-library "nausicaa/registry.sls"
		 "nausicaa/registry.slfasl")

(compile-library "nausicaa/parameters.larceny.sls"
		 "nausicaa/parameters.larceny.slfasl")

(compile-library "nausicaa/common.sls"
		 "nausicaa/common.larceny.slfasl")

(compile-library "nausicaa.larceny.sls"
		 "nausicaa.larceny.slfasl")

(compile-library "format.sls"
		 "format.slfasl")

(compile-library "lists.sls"
		 "lists.slfasl")

(compile-library "one-dimension-co.sls"
		 "one-dimension-co.slfasl")

(compile-library "one-dimension-cc.sls"
		 "one-dimension-cc.slfasl")

(compile-library "char-sets.sls"
		 "char-sets.slfasl")

(compile-library "char-sets/blocks.sls"
		 "char-sets/blocks.slfasl")

(compile-library "char-sets/categories.sls"
		 "char-sets/categories.slfasl")

(compile-library "knuth-morris-pratt.sls"
 		 "knuth-morris-pratt.slfasl")

(compile-library "strings/low.sls"
 		 "strings/low.slfasl")

(compile-library "strings.sls"
 		 "strings.slfasl")

(compile-library "vectors/vectors-low.sls"
 		 "vectors/vectors-low.slfasl")

(compile-library "vectors.sls"
 		 "vectors.slfasl")

(compile-library "loops.sls"
		 "loops.slfasl")

(compile-library "checks.sls"
		 "checks.slfasl")

(compile-library "random.sls"
		 "random.slfasl")

(compile-library "compare.sls"
		 "compare.slfasl")

;;; --------------------------------------------------------------------

(compile-library "object-properties.sls"
		 "object-properties.slfasl")

(compile-library "cleanup-handlers.sls"
		 "cleanup-handlers.slfasl")

(compile-library "queues.sls"
		 "queues.slfasl")

(compile-library "combinators.sls"
		 "combinators.slfasl")

;;; --------------------------------------------------------------------

(compile-library "foreign/ffi/sizeof.sls"
		 "foreign/ffi/sizeof.slfasl")

(compile-library "foreign/memory/compat.larceny.sls"
		 "foreign/memory/compat.larceny.slfasl")

(compile-library "foreign/memory.sls"
		 "foreign/memory.slfasl")

(compile-library "foreign/ffi/compat.larceny.sls"
		 "foreign/ffi/compat.larceny.slfasl")

(compile-library "foreign/ffi.sls"
		 "foreign/ffi.slfasl")

(compile-library "foreign/cstring.sls"
		 "foreign/cstring.slfasl")

(compile-library "foreign/errno.sls"
		 "foreign/errno.slfasl")

;;; (compile-library "foreign/guarded-malloc.sls"
;;;  		 "foreign/guarded-malloc.slfasl")

(compile-library "foreign.sls"
		 "foreign.slfasl")

;;; end of file
