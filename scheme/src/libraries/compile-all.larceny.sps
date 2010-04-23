;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compile script for Larceny Scheme
;;;Date: Mon Jan 19, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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

;;Core libraries

(compile-library "cond-expand/registry.sls"
		 "cond-expand/registry.slfasl")

(compile-library "cond-expand.larceny.sls"
		 "cond-expand.larceny.slfasl")

(compile-library "profiling/compat.larceny.sls"
		 "profiling/compat.larceny.slfasl")

(compile-library "profiling.sls"
		 "profiling.slfasl")

(compile-library "unimplemented.sls"
		 "unimplemented.slfasl")

(compile-library "conditions.sls"
		 "conditions.slfasl")

(compile-library "pretty-print.larceny.sls"
		 "pretty-print.larceny.slfasl")

(compile-library "language-extensions.sls"
		 "language-extensions.slfasl")

(compile-library "parameters.larceny.sls"
		 "parameters.larceny.slfasl")

(compile-library "shared-structures.sls"
		 "shared-structures.slfasl")

(compile-library "classes.sls"
		 "classes.slfasl")

(compile-library "deferred-exceptions.sls"
		 "deferred-exceptions.slfasl")

(compile-library "compensations.sls"
		 "compensations.slfasl")

(compile-library "nausicaa/common.sls"
		 "nausicaa/common.larceny.slfasl")

(compile-library "nausicaa.larceny.sls"
		 "nausicaa.larceny.slfasl")

;;Basic libraries

(compile-library "generics.sls"
		 "generics.slfasl")

(compile-library "enumerations.sls"
		 "enumerations.slfasl")


(compile-library "formations.sls"
		 "formations.slfasl")

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

(compile-library "streams.sls"
 		 "streams.slfasl")

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

(compile-library "randomisations/low.sls"
		 "randomisations/low.slfasl")

(compile-library "randomisations.sls"
		 "randomisations.slfasl")

(compile-library "randomisations/vectors.sls"
		 "randomisations/vectors.slfasl")

(compile-library "randomisations/strings.sls"
		 "randomisations/strings.slfasl")

(compile-library "randomisations/distributions.sls"
		 "randomisations/distributions.slfasl")

(compile-library "comparisons.sls"
		 "comparisons.slfasl")

(compile-library "arrays.sls"
		 "arrays.slfasl")

(compile-library "times-and-dates/compat.larceny.sls"
		 "times-and-dates/compat.larceny.slfasl")

(compile-library "times-and-dates.sls"
		 "times-and-dates.slfasl")

;;; --------------------------------------------------------------------

(compile-library "sentinel.sls"
		 "sentinel.slfasl")

(compile-library "variables.sls"
		 "variables.slfasl")

(compile-library "keywords.sls"
		 "keywords.slfasl")

;;; --------------------------------------------------------------------

(compile-library "object-properties.sls"
		 "object-properties.slfasl")

(compile-library "cleanup-handlers.sls"
		 "cleanup-handlers.slfasl")

(compile-library "queues.sls"
		 "queues.slfasl")

(compile-library "stacks.sls"
		 "stacks.slfasl")

;;; --------------------------------------------------------------------

;; High level other libraries

(compile-library "scmobj.sls"
		 "scmobj.slfasl")

(compile-library "scmobj/utils.sls"
		 "scmobj/utils.slfasl")

;;; --------------------------------------------------------------------

(compile-library "combinators.sls"
		 "combinators.slfasl")

(compile-library "irregex.sls"
		 "irregex.slfasl")

(compile-library "pregexp.sls"
		 "pregexp.slfasl")

(compile-library "matches.sls"
		 "matches.slfasl")

;;; --------------------------------------------------------------------

(compile-library "sexps.sls"
		 "sexps.slfasl")

;;; --------------------------------------------------------------------

(compile-library "parser-tools/source-location.sls"
 		 "parser-tools/source-location.slfasl")

(compile-library "parser-tools/lexical-token.sls"
 		 "parser-tools/lexical-token.slfasl")

;;; --------------------------------------------------------------------

(compile-library "silex/lexer.sls"
		 "silex/lexer.slfasl")

(compile-library "silex.sls"
		 "silex.slfasl")

;;; --------------------------------------------------------------------

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

(compile-library "packrat.sls"
 		 "packrat.slfasl")

;;; --------------------------------------------------------------------

(compile-library "email/addresses/common.sls"
 		 "email/addresses/common.slfasl")

(compile-library "email/addresses/quoted-text-lexer.sls"
		 "email/addresses/quoted-text-lexer.slfasl")

(compile-library "email/addresses/comments-lexer.sls"
		 "email/addresses/comments-lexer.slfasl")

(compile-library "email/addresses/domain-literals-lexer.sls"
		 "email/addresses/domain-literals-lexer.slfasl")

(compile-library "email/addresses/lexer.sls"
		 "email/addresses/lexer.slfasl")

(compile-library "email/addresses/parser.sls"
 		 "email/addresses/parser.slfasl")

(compile-library "email/addresses.sls"
		 "email/addresses.slfasl")

;;; --------------------------------------------------------------------

(compile-library "getopts.sls"
		 "getopts.slfasl")

;;; --------------------------------------------------------------------

(compile-library "libraries/rnrs-bindings.sls"
		 "libraries/rnrs-bindings.slfasl")

(compile-library "libraries/conditions.sls"
		 "libraries/conditions.slfasl")

(compile-library "libraries/helpers.sls"
		 "libraries/helpers.slfasl")

(compile-library "libraries/names.sls"
		 "libraries/names.slfasl")

(compile-library "libraries/references.sls"
		 "libraries/references.slfasl")

(compile-library "libraries/import-specs.sls"
		 "libraries/import-specs.slfasl")

(compile-library "libraries.sls"
		 "libraries.slfasl")

;;; --------------------------------------------------------------------

(compile-library "armor/conditions.sls"
		 "armor/conditions.slfasl")

(compile-library "armor/base16.sls"
		 "armor/base16.slfasl")

(compile-library "armor/base32.sls"
		 "armor/base32.slfasl")

(compile-library "armor/base64.sls"
		 "armor/base64.slfasl")

(compile-library "armor/base91.sls"
		 "armor/base91.slfasl")

(compile-library "armor/ascii85.sls"
		 "armor/ascii85.slfasl")

(compile-library "armor/newlines.sls"
		 "armor/newlines.slfasl")

(compile-library "armor/quoted-printable.sls"
		 "armor/quoted-printable.slfasl")

;;; --------------------------------------------------------------------

;; (compile-library "ffi/sizeof.sls"
;; 		 "ffi/sizeof.slfasl")

;; (compile-library "ffi/conditions.sls"
;; 		 "ffi/conditions.slfasl")

;; (compile-library "ffi/pointers/compat.larceny.sls"
;;  		 "ffi/pointers/compat.larceny.slfasl")

;; (compile-library "ffi/pointers.sls"
;;  		 "ffi/pointers.slfasl")

;; (compile-library "ffi/peekers-and-pokers/compat.larceny.sls"
;;  		 "ffi/peekers-and-pokers/compat.larceny.slfasl")

;; (compile-library "ffi/peekers-and-pokers.sls"
;;  		 "ffi/peekers-and-pokers.slfasl")

;; (compile-library "ffi/clang-data-types.sls"
;;  		 "ffi/clang-data-types.slfasl")

;; (compile-library "ffi/platform.larceny.sls"
;;  		 "ffi/platform.larceny.slfasl")

;; (compile-library "ffi/primitives.sls"
;;  		 "ffi/primitives.slfasl")

;; (compile-library "ffi.sls"
;;  		 "ffi.slfasl")

;;; --------------------------------------------------------------------

;; (compile-library "ffi/memory/conditions.sls"
;;  		 "ffi/memory/conditions.slfasl")

;; (compile-library "ffi/memory/operations/compat.larceny.sls"
;;  		 "ffi/memory/operations/compat.larceny.slfasl")

;; (compile-library "ffi/memory/operations.sls"
;;  		 "ffi/memory/operations.slfasl")

;; (compile-library "ffi/memory/memblocks.sls"
;;  		 "ffi/memory/memblocks.slfasl")

;; (compile-library "ffi/memory/alloc/compat.larceny.sls"
;;  		 "ffi/memory/alloc/compat.larceny.slfasl")

;; (compile-library "ffi/memory/alloc.sls"
;;  		 "ffi/memory/alloc.slfasl")

;; (compile-library "ffi/memory/bytevectors.sls"
;; 		 "ffi/memory/bytevectors.slfasl")

;; (compile-library "ffi/memory/caches.sls"
;; 		 "ffi/memory/caches.slfasl")

;; (compile-library "ffi/memory/compensated.sls"
;; 		 "ffi/memory/compensated.slfasl")

;; (compile-library "ffi/memory.sls"
;; 		 "ffi/memory.slfasl")

;; (compile-library "ffi/memory/refcount.sls"
;; 		 "ffi/memory/refcount.slfasl")

;; (compile-library "ffi/memory/membuffers.sls"
;; 		 "ffi/memory/membuffers.slfasl")

;; (compile-library "ffi/memory/mempool.sls"
;; 		 "ffi/memory/mempool.slfasl")

;;; --------------------------------------------------------------------

;; (compile-library "ffi/cstrings.sls"
;; 		 "ffi/cstrings.slfasl")

;; (compile-library "ffi/errno.sls"
;; 		 "ffi/errno.slfasl")

;;; end of file
