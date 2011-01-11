;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: proof for r6rs parser
;;;Date: Tue Jan 11, 2011
;;;
;;;Abstract
;;;
;;;	Read the Nausicaa/Scheme libraries under "./src/libraries"
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(import (nausicaa)
  (rnrs eval)
  (nausicaa formations)
  (nausicaa debugging)
  (nausicaa checks)
  (prefix (nausicaa silex lexer) lex.)
  (only (nausicaa parser-tools lexical-token) <lexical-token>)
  (only (nausicaa parser-tools source-location) <source-location>)
  (only (nausicaa r6rs datum-processing)
	<interlexeme-space>
	remove-interlexeme-space)
  (prefix (nausicaa r6rs lexer)  r6.)
  (prefix (nausicaa r6rs parser) r6.))


(define-constant rootdir
  "/home/marco/src/devel/scheme/nausicaa/scheme/src/libraries")

(define-constant library-files
  '("nausicaa.sls"
    "nausicaa/armor/ascii85.sls"
    "nausicaa/armor/base16.sls"
    "nausicaa/armor/base32.sls"
    "nausicaa/armor/base64.sls"
    "nausicaa/armor/base91.sls"
    "nausicaa/armor/conditions.sls"
    "nausicaa/armor/newlines.sls"
    "nausicaa/armor/quoted-printable.sls"
    "nausicaa/arrays.sls"
    "nausicaa/asciis.sls"
    "nausicaa/bytevectors/u8.sls"
    "nausicaa/bytevectors/u8low.sls"
    "nausicaa/char-sets.sls"
    "nausicaa/char-sets/blocks.sls"
    "nausicaa/char-sets/categories.sls"
    "nausicaa/checks.sls"
    "nausicaa/cleanup-handlers.sls"
    "nausicaa/combinators.sls"
    "nausicaa/comparisons.sls"
    "nausicaa/configuration.sls"
    "nausicaa/contracts.sls"
    "nausicaa/contracts/helpers.sls"
    "nausicaa/csv.sls"
    "nausicaa/csv/strings-lexer.sls"
    "nausicaa/csv/unquoted-data-comma-lexer.sls"
    "nausicaa/csv/unquoted-data-lexer.sls"
    "nausicaa/debugging.sls"
    "nausicaa/email/addresses.sls"
    "nausicaa/email/addresses/comments-lexer.sls"
    "nausicaa/email/addresses/common.sls"
    "nausicaa/email/addresses/domain-literals-lexer.sls"
    "nausicaa/email/addresses/lexer.sls"
    "nausicaa/email/addresses/parser.sls"
    "nausicaa/email/addresses/quoted-text-lexer.sls"
    "nausicaa/enumerations.sls"
    "nausicaa/evaluations.sls"
    "nausicaa/evaluations/wrapper.sls"
    "nausicaa/ffi.sls"
    "nausicaa/ffi/clang-data-types.sls"
    "nausicaa/ffi/clang-data-types/compat.mosh.sls"
    "nausicaa/ffi/clang-data-types/compat.petite.sls"
    "nausicaa/ffi/clang-data-types/compat.vicare.sls"
    "nausicaa/ffi/clang-data-types/compat.ypsilon.sls"
    "nausicaa/ffi/compat.mosh.sls"
    "nausicaa/ffi/compat.petite.sls"
    "nausicaa/ffi/compat.vicare.sls"
    "nausicaa/ffi/compat.ypsilon.sls"
    "nausicaa/ffi/conditions.sls"
    "nausicaa/ffi/cstrings.sls"
    "nausicaa/ffi/errno.sls"
    "nausicaa/ffi/extension-utilities.sls"
    "nausicaa/ffi/inspector-maker.sls"
    "nausicaa/ffi/memory.sls"
    "nausicaa/ffi/memory/alloc.sls"
    "nausicaa/ffi/memory/alloc/compat.mosh.sls"
    "nausicaa/ffi/memory/alloc/compat.petite.sls"
    "nausicaa/ffi/memory/alloc/compat.vicare.sls"
    "nausicaa/ffi/memory/alloc/compat.ypsilon.sls"
    "nausicaa/ffi/memory/bytevectors.sls"
    "nausicaa/ffi/memory/caches.sls"
    "nausicaa/ffi/memory/compensated.sls"
    "nausicaa/ffi/memory/conditions.sls"
    "nausicaa/ffi/memory/guarded-malloc.vicare.sls"
    "nausicaa/ffi/memory/memblocks.sls"
    "nausicaa/ffi/memory/membuffers.sls"
    "nausicaa/ffi/memory/mempool.sls"
    "nausicaa/ffi/memory/operations.sls"
    "nausicaa/ffi/memory/refcount.sls"
    "nausicaa/ffi/peekers-and-pokers.sls"
    "nausicaa/ffi/peekers-and-pokers/compat.mosh.sls"
    "nausicaa/ffi/peekers-and-pokers/compat.petite.sls"
    "nausicaa/ffi/peekers-and-pokers/compat.vicare.sls"
    "nausicaa/ffi/peekers-and-pokers/compat.ypsilon.sls"
    "nausicaa/ffi/pointers.sls"
    "nausicaa/ffi/pointers/compat.mosh.sls"
    "nausicaa/ffi/pointers/compat.petite.sls"
    "nausicaa/ffi/pointers/compat.vicare.sls"
    "nausicaa/ffi/pointers/compat.ypsilon.sls"
    "nausicaa/ffi/sizeof.sls"
    "nausicaa/ffi/syntax-helpers.sls"
    "nausicaa/formations.sls"
    "nausicaa/generics.sls"
    "nausicaa/generics/object-to-string.sls"
    "nausicaa/getopts.sls"
    "nausicaa/infix.sls"
    "nausicaa/infix/helpers.sls"
    "nausicaa/infix/sexp-parser.sls"
    "nausicaa/infix/string-lexer.sls"
    "nausicaa/infix/string-parser.sls"
    "nausicaa/interps.sls"
    "nausicaa/interps/variable-events.sls"
    "nausicaa/interps/variables.sls"
    "nausicaa/interps/wrapper.sls"
    "nausicaa/irregex.sls"
    "nausicaa/json.sls"
    "nausicaa/json/extended-lexer.sls"
    "nausicaa/json/rfc-lexer.sls"
    "nausicaa/json/sexp-parser.sls"
    "nausicaa/json/string-lexer.sls"
    "nausicaa/keywords.sls"
    "nausicaa/knuth-morris-pratt.sls"
    "nausicaa/lalr.sls"
    "nausicaa/lalr/glr-driver.sls"
    "nausicaa/lalr/lr-driver.sls"
    "nausicaa/language/assertions.sls"
    "nausicaa/language/auxiliary-syntaxes.sls"
    "nausicaa/language/classes.sls"
    "nausicaa/language/classes/binding-makers.sls"
    "nausicaa/language/classes/clause-parsers.sls"
    "nausicaa/language/classes/helpers.sls"
    "nausicaa/language/classes/internal-auxiliary-syntaxes.sls"
    "nausicaa/language/classes/properties.sls"
    "nausicaa/language/classes/top.sls"
    "nausicaa/language/common.sls"
    "nausicaa/language/compat.mosh.sls"
    "nausicaa/language/compat.petite.sls"
    "nausicaa/language/compat.vicare.sls"
    "nausicaa/language/compat.ypsilon.sls"
    "nausicaa/language/compensations.sls"
    "nausicaa/language/cond-expand.mosh.sls"
    "nausicaa/language/cond-expand.petite.sls"
    "nausicaa/language/cond-expand.vicare.sls"
    "nausicaa/language/cond-expand.ypsilon.sls"
    "nausicaa/language/cond-expand/registry.sls"
    "nausicaa/language/conditions.sls"
    "nausicaa/language/deferred-exceptions.sls"
    "nausicaa/language/extensions.sls"
    "nausicaa/language/gensym.mosh.sls"
    "nausicaa/language/gensym.petite.sls"
    "nausicaa/language/gensym.vicare.sls"
    "nausicaa/language/gensym.ypsilon.sls"
    "nausicaa/language/getenv.mosh.sls"
    "nausicaa/language/getenv.petite.sls"
    "nausicaa/language/getenv.vicare.sls"
    "nausicaa/language/getenv.ypsilon.sls"
    "nausicaa/language/identifier-alists.sls"
    "nausicaa/language/identifier-properties.sls"
    "nausicaa/language/identifier-properties/helpers.sls"
    "nausicaa/language/infix.sls"
    "nausicaa/language/makers.sls"
    "nausicaa/language/makers/helpers.sls"
    "nausicaa/language/parameters.mosh.sls"
    "nausicaa/language/parameters.petite.sls"
    "nausicaa/language/parameters.vicare.sls"
    "nausicaa/language/parameters.ypsilon.sls"
    "nausicaa/language/pretty-print.mosh.sls"
    "nausicaa/language/pretty-print.petite.sls"
    "nausicaa/language/pretty-print.vicare.sls"
    "nausicaa/language/pretty-print.ypsilon.sls"
    "nausicaa/language/sentinel.sls"
    "nausicaa/language/shared-structures.sls"
    "nausicaa/language/syntax-utilities.sls"
    "nausicaa/language/unimplemented.sls"
    "nausicaa/libraries.sls"
    "nausicaa/libraries/conditions.sls"
    "nausicaa/libraries/helpers.sls"
    "nausicaa/libraries/import-specs.sls"
    "nausicaa/libraries/names.sls"
    "nausicaa/libraries/references.sls"
    "nausicaa/libraries/rnrs-bindings.sls"
    "nausicaa/lists.sls"
    "nausicaa/lists/low.sls"
    "nausicaa/lists/stx.sls"
    "nausicaa/lists/xlists.sls"
    "nausicaa/loops.sls"
    "nausicaa/matches.sls"
    "nausicaa/msgcat.sls"
    "nausicaa/mutable-pairs.sls"
    "nausicaa/mutable-strings.sls"
    "nausicaa/net/helpers/ipv4-address-lexer.sls"
    "nausicaa/net/helpers/ipv4-address-parser.sls"
    "nausicaa/net/helpers/ipv6-address-lexer.sls"
    "nausicaa/net/helpers/ipv6-address-parser.sls"
    "nausicaa/net/ipv4-addresses.sls"
    "nausicaa/net/ipv6-addresses.sls"
    "nausicaa/object-properties.sls"
    "nausicaa/one-dimension-cc.sls"
    "nausicaa/one-dimension-co.sls"
    "nausicaa/packrat.sls"
    "nausicaa/parser-tools/lexical-token.sls"
    "nausicaa/parser-tools/source-location.sls"
    "nausicaa/pregexp.sls"
    "nausicaa/profiling.sls"
    "nausicaa/profiling/compat.mosh.sls"
    "nausicaa/profiling/compat.petite.sls"
    "nausicaa/profiling/compat.vicare.sls"
    "nausicaa/profiling/compat.ypsilon.sls"
    "nausicaa/queues.sls"
    "nausicaa/r6rs/character-lexer-table.sls"
    "nausicaa/r6rs/datum-processing.sls"
    "nausicaa/r6rs/fixed-strings.sls"
    "nausicaa/r6rs/identifier-lexer-table.sls"
    "nausicaa/r6rs/lexeme-processing.sls"
    "nausicaa/r6rs/lexer-table.sls"
    "nausicaa/r6rs/lexer.sls"
    "nausicaa/r6rs/line-comment-lexer-table.sls"
    "nausicaa/r6rs/nested-comment-lexer-table.sls"
    "nausicaa/r6rs/number-lexer-table.sls"
    "nausicaa/r6rs/parser-table.sls"
    "nausicaa/r6rs/parser.sls"
    "nausicaa/r6rs/string-lexer-table.sls"
    "nausicaa/randomisations.sls"
    "nausicaa/randomisations/blum-blum-shub.sls"
    "nausicaa/randomisations/borosh.sls"
    "nausicaa/randomisations/cmrg.sls"
    "nausicaa/randomisations/distributions.sls"
    "nausicaa/randomisations/lists.sls"
    "nausicaa/randomisations/low.sls"
    "nausicaa/randomisations/marsaglia.sls"
    "nausicaa/randomisations/mersenne.sls"
    "nausicaa/randomisations/strings.sls"
    "nausicaa/randomisations/vectors.sls"
    "nausicaa/silex.sls"
    "nausicaa/silex/default-error-handler.sls"
    "nausicaa/silex/lexer.sls"
    "nausicaa/silex/utilities.sls"
    "nausicaa/stacks.sls"
    "nausicaa/streams.sls"
    "nausicaa/strings.sls"
    "nausicaa/strings/low.sls"
    "nausicaa/strings/xstrings.sls"
    "nausicaa/submodules.sls"
    "nausicaa/symbols-tree.sls"
    "nausicaa/times-and-dates.sls"
    "nausicaa/times-and-dates/compat.mosh.sls"
    "nausicaa/times-and-dates/compat.petite.sls"
    "nausicaa/times-and-dates/compat.vicare.sls"
    "nausicaa/times-and-dates/compat.ypsilon.sls"
    "nausicaa/times-and-dates/gregorian.sls"
    "nausicaa/times-and-dates/julian-calendar.sls"
    "nausicaa/times-and-dates/leap-second-table.sls"
    "nausicaa/times-and-dates/seconds-and-subseconds.sls"
    "nausicaa/times-and-dates/types.sls"
    "nausicaa/type-utilities.sls"
    "nausicaa/uri.sls"
    "nausicaa/uri/generic-lexer.sls"
    "nausicaa/uri/generic-parser.sls"
    "nausicaa/uri/low.sls"
    "nausicaa/variables.sls"
    "nausicaa/vectors.sls"
    "nausicaa/vectors/low.sls"
    "nausicaa/vectors/xvectors.sls"
    ))


(define (same-error sexp1 sexp2)
  (error 'same? "wrong tokens" sexp1 sexp2))

(define (same? sexp1 sexp2)
  (cond ((boolean? sexp1)
	 (or (and (boolean? sexp2) (boolean=? sexp1 sexp2))
	     (same-error sexp1 sexp2)))
	((char? sexp1)
	 (or (and (char? sexp2) (char=? sexp1 sexp2))
	     (same-error sexp1 sexp2)))
	((string? sexp1)
	 (or (and (string? sexp2) (string=? sexp1 sexp2))
	     (same-error sexp1 sexp2)))
	((symbol? sexp1)
;; (write sexp1)(newline)
;; (write sexp2)(newline)
	 (or (and (symbol? sexp2) (eq? sexp1 sexp2))
	     (same-error sexp1 sexp2)))
	((number? sexp1)
	 (or (and (number? sexp2) (number=? sexp1 sexp2))
	     (same-error sexp1 sexp2)))
	((null? sexp1)
	 (if (null? sexp2)
	     #t
	   (same-error sexp1 sexp2)))
	((pair? sexp1)
;; (write sexp1)(newline)
;; (write sexp2)(newline)
	 (if (pair? sexp2)
	     (and (same? (car sexp1) (car sexp2))
		  (same? (cdr sexp1) (cdr sexp2)))
	   (same-error sexp1 sexp2)))
	((vector? sexp1)
	 (if (vector? sexp2)
	     (same? (vector->list sexp1)
		    (vector->list sexp2))
	   (same-error sexp1 sexp2)))
	((bytevector? sexp1)
	 (if (bytevector? sexp2)
	     (bytevector=? sexp1 sexp2)
	   (same-error sexp1 sexp2)))
	(else
	 (same-error sexp1 sexp2))))

(define (number=? x y)
  (define (== x y)
    (cond ((nan? x)	(nan? y))
	  ((zero? x)	(zero? y))
	  ((infinite? x)
	   (and (infinite? y)
		(or (and (positive? x) (positive? y))
		    (and (negative? x) (negative? y)))))
	  (else
	   (and (or (and (exact? x) (exact? y))
		    (and (inexact? x) (inexact? y)))
		(= x y)
		;;(< (abs (- x y)) 1e-6)
		))))
  (cond ((and (number? x) (number? y))
	 (and (== (real-part x) (real-part y))
	      (== (imag-part x) (imag-part y))))
	(else (equal? x y))))


(define (error-handler message (T <lexical-token>))
  (raise
   (condition (make-lexical-violation)
	      (make-message-condition
	       (string-append message
			      " line " (number->string T.location.line)
			      " column " (number->string T.location.column)))
	      (make-irritants-condition `(,T.value)))))

(define (parse port)
  (let* ((IS		(lex.lexer-make-IS
			 (lex.port: port)
			 (lex.counters: 'all)))
	 (true-lexer	(r6.make-token-lexer IS))
	 (lexer	(lambda ()
		  (let (((T <lexical-token>) (true-lexer)))
		    (debug "c: ~s, v: ~s" T.category T.value)
		    T)))
	 (parser	(r6.make-r6rs-parser)))
    (parser lexer error-handler #f)))

(define (compare pathname)
  (with-compensations
    (letrec* ((port (compensate
			(open-input-file pathname)
		      (with
		       (close-port port))))
	      (sexp1 (read port)))
      (format #t "parsing: ~a ... " pathname)
      (flush-output-port (current-output-port))
      (letrec* ((port (compensate
			  (open-input-file pathname)
			(with
			 (close-port port))))
		(sexp2 (parse port)))
	(let ((sexp2 (remove-interlexeme-space sexp2)))
	  (when #f
	    (pretty-print sexp1)
	    (pretty-print sexp2))
	  (same? sexp1 (car sexp2))
	  (format #t "ok\n")
	  )))))

(parametrise ((debugging #f))
  (let ((cl (cdr (command-line))))
    (if (null? cl)
	(for-each compare
	  (map (lambda (pathname)
		 (string-append rootdir "/" pathname))
	    library-files))
      (compare (car cl)))))

;;; end of file
