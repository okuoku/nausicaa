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

(compile-library "profiling/compat.larceny.sls"
		 "profiling/compat.larceny.slfasl")

(compile-library "profiling.sls"
		 "profiling.slfasl")

(compile-library "unimplemented.sls"
		 "unimplemented.slfasl")

(compile-library "conditions.sls"
		 "conditions.slfasl")

(compile-library "parameters.larceny.sls"
		 "parameters.larceny.slfasl")

(compile-library "pretty-print.larceny.sls"
		 "pretty-print.larceny.slfasl")

(compile-library "language-extensions.sls"
		 "language-extensions.slfasl")

(compile-library "nausicaa/common.sls"
		 "nausicaa/common.larceny.slfasl")

(compile-library "nausicaa.larceny.sls"
		 "nausicaa.larceny.slfasl")

(compile-library "format.sls"
		 "format.slfasl")

(compile-library "lists/low.sls"
		 "lists/low.slfasl")

(compile-library "lists/stx.sls"
		 "lists/stx.slfasl")

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

(compile-library "vectors/low.sls"
 		 "vectors/low.slfasl")

(compile-library "vectors.sls"
 		 "vectors.slfasl")

(compile-library "loops.sls"
		 "loops.slfasl")

(compile-library "debugging.sls"
		 "debugging.slfasl")

(compile-library "checks.sls"
		 "checks.slfasl")

(compile-library "random/low.sls"
		 "random/low.slfasl")

(compile-library "random.sls"
		 "random.slfasl")

(compile-library "random/vectors.sls"
		 "random/vectors.slfasl")

(compile-library "random/strings.sls"
		 "random/strings.slfasl")

(compile-library "random/distributions.sls"
		 "random/distributions.slfasl")

(compile-library "comparisons.sls"
		 "comparisons.slfasl")

(compile-library "arrays.sls"
		 "arrays.slfasl")

;;; --------------------------------------------------------------------

(compile-library "sentinel.sls"
		 "sentinel.slfasl")

(compile-library "scmobj/utils.sls"
		 "scmobj/utils.slfasl")

(compile-library "scmobj.sls"
		 "scmobj.slfasl")

(compile-library "deferred-exceptions.sls"
		 "deferred-exceptions.slfasl")

(compile-library "compensations.sls"
		 "compensations.slfasl")

(compile-library "object-properties.sls"
		 "object-properties.slfasl")

(compile-library "cleanup-handlers.sls"
		 "cleanup-handlers.slfasl")

(compile-library "queues.sls"
		 "queues.slfasl")

(compile-library "combinators.sls"
		 "combinators.slfasl")

(compile-library "variables.sls"
		 "variables.slfasl")

(compile-library "keywords.sls"
		 "keywords.slfasl")

(compile-library "irregex.sls"
		 "irregex.slfasl")

(compile-library "pregexp.sls"
		 "pregexp.slfasl")

;;; --------------------------------------------------------------------

(compile-library "silex/lexer.sls"
		 "silex/lexer.slfasl")

(compile-library "silex.sls"
		 "silex.slfasl")

;;; --------------------------------------------------------------------

(compile-library "lalr/common.sls"
 		 "lalr/common.slfasl")

(compile-library "lalr/lr-driver.sls"
 		 "lalr/lr-driver.slfasl")

(compile-library "lalr/glr-driver.sls"
 		 "lalr/glr-driver.slfasl")

(compile-library "lalr.sls"
 		 "lalr.slfasl")

;;; --------------------------------------------------------------------

(compile-library "csv/strings-lexer.sls"
 		 "csv/strings-lexer.slfasl")

(compile-library "csv/unquoted-data-lexer.sls"
 		 "csv/unquoted-data-lexer.slfasl")

(compile-library "csv/unquoted-data-comma-lexer.sls"
 		 "csv/unquoted-data-comma-lexer.slfasl")

(compile-library "csv.sls"
 		 "csv.slfasl")

;;; --------------------------------------------------------------------

(compile-library "infix/string-lexer.sls"
 		 "infix/string-lexer.slfasl")

(compile-library "infix/sexp-parser.sls"
 		 "infix/sexp-parser.slfasl")

(compile-library "infix/string-parser.sls"
 		 "infix/string-parser.slfasl")

(compile-library "infix.sls"
 		 "infix.slfasl")

(compile-library "infix/syntax.sls"
 		 "infix/syntax.slfasl")

;;; --------------------------------------------------------------------

(compile-library "email/addresses/quoted-text-lexer.sls"
		 "email/addresses/quoted-text-lexer.slfasl")

(compile-library "email/addresses/comments-lexer.sls"
		 "email/addresses/comments-lexer.slfasl")

(compile-library "email/addresses/domain-literals-lexer.sls"
		 "email/addresses/domain-literals-lexer.slfasl")

(compile-library "email/addresses/lexer.sls"
		 "email/addresses/lexer.slfasl")

;; (compile-library "email/addresses/parser.sls"
;; 		 "email/addresses/parser.slfasl")

(compile-library "email/addresses.sls"
		 "email/addresses.slfasl")

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

(compile-library "peekers-and-pokers.sls"
		 "peekers-and-pokers.slfasl")

;;; end of file
