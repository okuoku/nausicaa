;;; -*- coding: utf-8-unix -*-
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
	 (lex.library-imports:	'((nausicaa parser-tools lexical-token)
				  (nausicaa parser-tools source-location)
				  (nausicaa r6rs lexeme-processing)))
	 (lex.table-name:	'r6rs-lexer-table)
;;;	 (lex.lexer-format:	'code) ;this format makes the library 1.2MB in size
	 (lex.lexer-format:	'decision-tree) ;this format makes the library 123KB in size
	 (lex.counters:		'all))

(lex.lex (lex.input-file:	"nested-comment-lexer-table.l")
	 (lex.output-file:	"nested-comment-lexer-table.sls")
	 (lex.library-spec:	"(nausicaa r6rs nested-comment-lexer-table)")
	 (lex.library-imports:	'((nausicaa r6rs lexeme-processing)
				  (nausicaa parser-tools lexical-token)
				  (nausicaa parser-tools source-location)))
	 (lex.table-name:	'r6rs-nested-comment-lexer-table)
	 (lex.lexer-format:	'code)
	 (lex.counters:		'all))

(lex.lex (lex.input-file:	"line-comment-lexer-table.l")
	 (lex.output-file:	"line-comment-lexer-table.sls")
	 (lex.library-spec:	"(nausicaa r6rs line-comment-lexer-table)")
	 (lex.library-imports:	'((nausicaa r6rs lexeme-processing)
				  (nausicaa parser-tools lexical-token)
				  (nausicaa parser-tools source-location)))
	 (lex.table-name:	'r6rs-line-comment-lexer-table)
	 (lex.lexer-format:	'code)
	 (lex.counters:		'all))

(lex.lex (lex.input-file:	"string-lexer-table.l")
	 (lex.output-file:	"string-lexer-table.sls")
	 (lex.library-spec:	"(nausicaa r6rs string-lexer-table)")
	 (lex.library-imports:	'((nausicaa r6rs lexeme-processing)
				  (nausicaa parser-tools lexical-token)
				  (nausicaa parser-tools source-location)))
	 (lex.table-name:	'r6rs-string-lexer-table)
	 (lex.lexer-format:	'code)
	 (lex.counters:		'all))

(lex.lex (lex.input-file:	"character-lexer-table.l")
	 (lex.output-file:	"character-lexer-table.sls")
	 (lex.library-spec:	"(nausicaa r6rs character-lexer-table)")
	 (lex.library-imports:	'((nausicaa r6rs lexeme-processing)
				  (nausicaa parser-tools lexical-token)
				  (nausicaa parser-tools source-location)))
	 (lex.table-name:	'r6rs-character-lexer-table)
	 (lex.lexer-format:	'code)
	 (lex.counters:		'all))

(lex.lex (lex.input-file:	"identifier-lexer-table.l")
	 (lex.output-file:	"identifier-lexer-table.sls")
	 (lex.library-spec:	"(nausicaa r6rs identifier-lexer-table)")
	 (lex.library-imports:	'((nausicaa r6rs lexeme-processing)
				  (nausicaa parser-tools lexical-token)
				  (nausicaa parser-tools source-location)))
	 (lex.table-name:	'r6rs-identifier-lexer-table)
	 (lex.lexer-format:	'code)
	 (lex.counters:		'all))

(lex.lex (lex.input-file:	"number-lexer-table.l")
	 (lex.output-file:	"number-lexer-table.sls")
	 (lex.library-spec:	"(nausicaa r6rs number-lexer-table)")
	 (lex.library-imports:	'((nausicaa r6rs lexeme-processing)
				  (nausicaa parser-tools lexical-token)
				  (nausicaa parser-tools source-location)))
	 (lex.table-name:	'r6rs-number-lexer-table)
	 (lex.lexer-format:	'code)
	 (lex.counters:		'all))


(lalr.lalr-parser

 (lalr.output-file:	"parser-table.sls")
 (lalr.parser-name:	'make-r6rs-parser)
 (lalr.library-spec:	'(nausicaa r6rs parser-table))
 (lalr.library-imports:	'((nausicaa r6rs datum-processing)))

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
  '((datum
     (identifier)		: $1
     (boolean)			: $1
     (number)			: $1
     (string)			: $1
     (character)		: $1
     (vector)			: $1
     (bytevector)		: $1
     (pair)			: $1
     (list)			: $1)

    (identifier
     (IDENTIFIER)	: ((identifier-datum-maker)	yypushback yycustom $1))

    (boolean
     (BOOLEAN)		: ((boolean-datum-maker)	yypushback yycustom $1))

    (number
     (NUMBER)		: ((number-datum-maker)		yypushback yycustom $1))

    (character
     (CHARACTER)	: ((character-datum-maker)	yypushback yycustom $1))

    (string
     (STRING)		: ((string-datum-maker)		yypushback yycustom $1))

    (pair
     (OPAREN datum DOT datum CPAREN)	: ((pair-datum-maker) yypushback yycustom $2 $4))

    (list
     (OPAREN   datum paren-list-tail)	: ((list-datum-maker) yypushback yycustom (cons $2 $3))
     (OBRACKET datum bracket-list-tail)	: ((list-datum-maker) yypushback yycustom (cons $2 $3)))
    (paren-list-tail
     (CPAREN)				: '()
     (datum paren-list-tail)		: (cons $1 $2))
    (bracket-list-tail
     (CBRACKET)				: '()
     (datum bracket-list-tail)		: (cons $1 $2))

    (vector
     (SHARPPAREN datum vector-tail)	: ((vector-datum-maker) yypushback yycustom (cons $2 $3)))
    (vector-tail
     (CPAREN)				: '()
     (datum vector-tail)		: (cons $1 $2))

    (bytevector
     (SHARPVU8PAREN datum bvector-tail)	: ((bytevector-datum-maker) yypushback yycustom (cons $2 $3)))
    (bvector-tail
     (CPAREN)				: '()
     (datum bvector-tail)		: (cons $1 $2))

    (quoted-datum
     (TICK datum)			: ((quoted-datum-maker) yypushback yycustom $2))
    (quasiquoted-datum
     (BACKTICK datum)			: ((quasiquoted-datum-maker) yypushback yycustom $2))
    (unquoted-datum
     (COMMA datum)			: ((unquoted-datum-maker) yypushback yycustom $2))
    (unquoted-splicing-datum
     (COMMAAT datum)			: ((unquoted-splicing-datum-maker) yypushback yycustom $2))

    (syntax-datum
     (SHARPTICK datum)			: ((syntax-datum-maker) yypushback yycustom $2))
    (quasisyntax-datum
     (SHARPBACKTICK datum)		: ((quasisyntax-datum-maker) yypushback yycustom $2))
    (unsyntax-datum
     (SHARPCOMMA datum)			: ((unsyntax-datum-maker) yypushback yycustom $2))
    (unsyntax-splicing-datum
     (SHARPCOMMAAT datum)		: ((unsyntax-splicing-datum-maker) yypushback yycustom $2))

    (interlexeme-space
     (atmosphere atmosphere-tail)	: $1)
    (atmosphere
     (WHITESPACE)			: $1
     (comment)				: #f)
    (atmosphere-tail
     (atmosphere atmosphere-tail)	: $1)

    (comment
     (LINECOMMENT)				: #f
     (NESTEDCOMMENT)				: #f
     (SHARPBANGR6RS)				: ((sharp-bang-r6rs-datum-maker) yypushback yycustom $1)
     (SHARPBANG interlexeme-space identifier)	: ((sharp-bang-datum-maker) yypushback yycustom $3))
    )))

;;; end of file
