;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: SILex and LALR tables for R6RS lexer and parser
;;;Date: Wed Dec 22, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (prefix (nausicaa silex) lex.)
  (prefix (nausicaa lalr)  lalr.))

(lex.lex (lex.input-file:	"lexer-table.l")
	 (lex.output-file:	"lexer-table.sls")
	 (lex.library-spec:	"(nausicaa r6rs lexer-table)")
	 (lex.library-imports:	'((nausicaa silex default-error-handler)
				  (nausicaa parser-tools lexical-token)
				  (nausicaa parser-tools source-location)
				  (nausicaa r6rs lexeme-processing)))
	 (lex.table-name:	'r6rs-lexer-table)
;;;	 (lex.lexer-format:	'code) ;this format makes the library 1.2MB in size
	 (lex.lexer-format:	'decision-tree) ;this format makes the library 123KB in size
	 (lex.counters:		'all))

(lex.lex (lex.input-file:	"nested-comment-lexer-table.l")
	 (lex.output-file:	"nested-comment-lexer-table.sls")
	 (lex.library-spec:	"(nausicaa r6rs nested-comment-lexer-table)")
	 (lex.library-imports:	'((nausicaa silex default-error-handler)
				  (nausicaa parser-tools lexical-token)
				  (nausicaa parser-tools source-location)))
	 (lex.table-name:	'r6rs-nested-comment-lexer-table)
	 (lex.lexer-format:	'code)
	 (lex.counters:		'all))

(lex.lex (lex.input-file:	"line-comment-lexer-table.l")
	 (lex.output-file:	"line-comment-lexer-table.sls")
	 (lex.library-spec:	"(nausicaa r6rs line-comment-lexer-table)")
	 (lex.library-imports:	'((nausicaa silex default-error-handler)
				  (nausicaa parser-tools lexical-token)
				  (nausicaa parser-tools source-location)))
	 (lex.table-name:	'r6rs-line-comment-lexer-table)
	 (lex.lexer-format:	'code)
	 (lex.counters:		'all))

(lex.lex (lex.input-file:	"string-lexer-table.l")
	 (lex.output-file:	"string-lexer-table.sls")
	 (lex.library-spec:	"(nausicaa r6rs string-lexer-table)")
	 (lex.library-imports:	'((nausicaa silex default-error-handler)
				  (nausicaa parser-tools lexical-token)
				  (nausicaa parser-tools source-location)))
	 (lex.table-name:	'r6rs-string-lexer-table)
	 (lex.lexer-format:	'code)
	 (lex.counters:		'all))

(lex.lex (lex.input-file:	"character-lexer-table.l")
	 (lex.output-file:	"character-lexer-table.sls")
	 (lex.library-spec:	"(nausicaa r6rs character-lexer-table)")
	 (lex.library-imports:	'((nausicaa silex default-error-handler)
				  (nausicaa parser-tools lexical-token)
				  (nausicaa parser-tools source-location)))
	 (lex.table-name:	'r6rs-character-lexer-table)
	 (lex.lexer-format:	'code)
	 (lex.counters:		'all))

(lex.lex (lex.input-file:	"identifier-lexer-table.l")
	 (lex.output-file:	"identifier-lexer-table.sls")
	 (lex.library-spec:	"(nausicaa r6rs identifier-lexer-table)")
	 (lex.library-imports:	'((nausicaa silex default-error-handler)
				  (nausicaa parser-tools lexical-token)
				  (nausicaa parser-tools source-location)))
	 (lex.table-name:	'r6rs-identifier-lexer-table)
	 (lex.lexer-format:	'code)
	 (lex.counters:		'all))

(lex.lex (lex.input-file:	"number-lexer-table.l")
	 (lex.output-file:	"number-lexer-table.sls")
	 (lex.library-spec:	"(nausicaa r6rs number-lexer-table)")
	 (lex.library-imports:	'((nausicaa silex default-error-handler)
				  (nausicaa parser-tools lexical-token)
				  (nausicaa parser-tools source-location)))
	 (lex.table-name:	'r6rs-number-lexer-table)
	 (lex.lexer-format:	'code)
	 (lex.counters:		'all))

;;; --------------------------------------------------------------------

(lalr.lalr-parser

 (lalr.output-file:	"parser-table.sls")
 (lalr.parser-name:	'make-r6rs-parser-parser)
 (lalr.library-spec:	'(nausicaa r6rs parser-table))

 (lalr.terminals:	'( ;;
			  OPAREN		CPAREN
			  OBRACKET		CBRACKET
			  SHARPPAREN		SHARPVU8PAREN
			  TICK			BACKTICK
			  COMMA			COMMAAT
			  DOT
			  SHARPTICK		SHARPBACKTICK
			  SHARPCOMMA		SHARPCOMMAAT
			  DOUBLECOLON		SEMICOLON
			  SHARP
			  WHITESPACE		LINEENDING
			  SHARPSEMICOLON

			  IDENTIFIER		BOOLEAN
			  NUMBER		CHARACTER
			  STRING

			  LINECOMMENT		NESTEDCOMMENT
			  SHARPBANGR6RS		SHARPBANG))

 (lalr.rules:
  '((lexeme		(IDENTIFIER)		: $1
			(BOOLEAN)		: $1
			(NUMBER)		: $1
			(CHARACTER)		: $1
			(STRING)		: $1
			(OPAREN)		: $1
			(CPAREN)		: $1
			(OBRACKET)		: $1
			(CBRACKET)		: $1
			(SHARPPAREN)		: $1
			(SHARPVU8PAREN)		: $1
			(TICK)			: $1
			(BACKTICK)		: $1
			(COMMA)			: $1
			(COMMAAT)		: $1
			(DOT)			: $1
			(SHARPTICK)		: $1
			(SHARPBACKTICK)		: $1
			(SHARPCOMMA)		: $1
			(SHARPCOMMAAT)		: $1)

    (interlexeme-space	(atmosphere atmosphere-tail)		: $1)
    (atmosphere		(WHITESPACE)				: $1
			(comment)				: #f)
    (atmosphere-tail	(atmosphere atmosphere-tail)		: $1)

    (comment		(LINECOMMENT)				: #f
			(NESTEDCOMMENT)				: #f
			(SHARPBANGR6RS)				: #f
			;;FIXME the following must end with a DATUM token
			(SHARPBANG interlexeme-space )		: #f)

    )))

;;; end of file
