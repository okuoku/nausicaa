;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compile script for Larceny Scheme
;;;Date: Mon Jan 19, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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

(compile-library "enumerations.sls"
		 "enumerations.slfasl")

(compile-library "parameters.larceny.sls"
		 "parameters.larceny.slfasl")

(compile-library "pretty-print.larceny.sls"
		 "pretty-print.larceny.slfasl")

(compile-library "begin0.sls"
		 "begin0.slfasl")

(compile-library "set-cons.sls"
		 "set-cons.slfasl")

(compile-library "receive.sls"
		 "receive.slfasl")

(compile-library "language-extensions.sls"
		 "language-extensions.slfasl")

(compile-library "nausicaa/common.sls"
		 "nausicaa/common.larceny.slfasl")

(compile-library "nausicaa.larceny.sls"
		 "nausicaa.larceny.slfasl")

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

(compile-library "sentinel.sls"
		 "sentinel.slfasl")

(compile-library "times-and-dates/compat.larceny.sls"
		 "times-and-dates/compat.larceny.slfasl")

(compile-library "times-and-dates.sls"
		 "times-and-dates.slfasl")

;;; --------------------------------------------------------------------

(compile-library "records/builtins.sls"
		 "records/builtins.slfasl")

(compile-library "records/helpers.sls"
		 "records/helpers.slfasl")

(compile-library "records/extensions.sls"
		 "records/extensions.slfasl")

(compile-library "records.sls"
		 "records.slfasl")

(compile-library "scmobj.sls"
		 "scmobj.slfasl")

(compile-library "scmobj/utils.sls"
		 "scmobj/utils.slfasl")

;;; --------------------------------------------------------------------

(compile-library "deferred-exceptions.sls"
		 "deferred-exceptions.slfasl")

(compile-library "compensations.sls"
		 "compensations.slfasl")

(compile-library "object-properties.sls"
		 "object-properties.slfasl")

(compile-library "cleanup-handlers.sls"
		 "cleanup-handlers.slfasl")

(compile-library "queues/types.sls"
 		 "queues/types.slfasl")

(compile-library "queues/extensions.sls"
		 "queues/extensions.slfasl")

(compile-library "queues.sls"
		 "queues.slfasl")

(compile-library "stacks/types.sls"
 		 "stacks/types.slfasl")

(compile-library "stacks/extensions.sls"
		 "stacks/extensions.slfasl")

(compile-library "stacks.sls"
		 "stacks.slfasl")

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

(compile-library "matches.sls"
		 "matches.slfasl")

;;; --------------------------------------------------------------------

(compile-library "sexps.sls"
		 "sexps.slfasl")

(compile-library "sexps/syntax.sls"
		 "sexps/syntax.slfasl")

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

(compile-library "getopts/record-types.sls"
		 "getopts/record-types.slfasl")

(compile-library "getopts.sls"
		 "getopts.slfasl")

;;; --------------------------------------------------------------------

;; (compile-library "foreign/ffi/sizeof.sls"
;; 		 "foreign/ffi/sizeof.slfasl")

;; (compile-library "foreign/ffi/conditions.sls"
;; 		 "foreign/ffi/conditions.slfasl")

;; (compile-library "foreign/ffi/pointers/compat.larceny.sls"
;;  		 "foreign/ffi/pointers/compat.larceny.slfasl")

;; (compile-library "foreign/ffi/pointers.sls"
;;  		 "foreign/ffi/pointers.slfasl")

;; (compile-library "foreign/ffi/peekers-and-pokers/compat.larceny.sls"
;;  		 "foreign/ffi/peekers-and-pokers/compat.larceny.slfasl")

;; (compile-library "foreign/ffi/peekers-and-pokers.sls"
;;  		 "foreign/ffi/peekers-and-pokers.slfasl")

;; (compile-library "foreign/ffi/clang-data-types.sls"
;;  		 "foreign/ffi/clang-data-types.slfasl")

;; (compile-library "foreign/ffi/platform.larceny.sls"
;;  		 "foreign/ffi/platform.larceny.slfasl")

;; (compile-library "foreign/ffi/primitives.sls"
;;  		 "foreign/ffi/primitives.slfasl")

;; (compile-library "foreign/ffi.sls"
;;  		 "foreign/ffi.slfasl")

;;; --------------------------------------------------------------------

;; (compile-library "foreign/memory/conditions.sls"
;;  		 "foreign/memory/conditions.slfasl")

;; (compile-library "foreign/memory/operations/compat.larceny.sls"
;;  		 "foreign/memory/operations/compat.larceny.slfasl")

;; (compile-library "foreign/memory/operations.sls"
;;  		 "foreign/memory/operations.slfasl")

;; (compile-library "foreign/memory/memblocks.sls"
;;  		 "foreign/memory/memblocks.slfasl")

;; (compile-library "foreign/memory/alloc/compat.larceny.sls"
;;  		 "foreign/memory/alloc/compat.larceny.slfasl")

;; (compile-library "foreign/memory/alloc.sls"
;;  		 "foreign/memory/alloc.slfasl")

;; (compile-library "foreign/memory/bytevectors.sls"
;; 		 "foreign/memory/bytevectors.slfasl")

;; (compile-library "foreign/memory/caches.sls"
;; 		 "foreign/memory/caches.slfasl")

;; (compile-library "foreign/memory/compensated.sls"
;; 		 "foreign/memory/compensated.slfasl")

;; (compile-library "foreign/memory.sls"
;; 		 "foreign/memory.slfasl")

;; (compile-library "foreign/memory/refcount.sls"
;; 		 "foreign/memory/refcount.slfasl")

;; (compile-library "foreign/memory/membuffers/types.sls"
;;  		 "foreign/memory/membuffers/types.slfasl")

;; (compile-library "foreign/memory/membuffers/extensions.sls"
;; 		 "foreign/memory/membuffers/extensions.slfasl")

;; (compile-library "foreign/memory/membuffers.sls"
;; 		 "foreign/memory/membuffers.slfasl")

;; (compile-library "foreign/memory/mempool/types.sls"
;;  		 "foreign/memory/mempool/types.slfasl")

;; (compile-library "foreign/memory/mempool/extensions.sls"
;; 		 "foreign/memory/mempool/extensions.slfasl")

;; (compile-library "foreign/memory/mempool.sls"
;; 		 "foreign/memory/mempool.slfasl")

;;; --------------------------------------------------------------------

;; (compile-library "foreign/cstrings.sls"
;; 		 "foreign/cstrings.slfasl")

;; (compile-library "foreign/errno.sls"
;; 		 "foreign/errno.slfasl")

;;; end of file
