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
  '((datums
     (*eoi*)			: '()
     (datum datums)		: (cons $1 $2))

    (datum
     (identifier)		: $1
     (boolean)			: $1
     (number)			: $1
     (string)			: $1
     (character)		: $1
     (vector)			: $1
     (bytevector)		: $1
     (pair)			: $1
     (list)			: $1
     (quoted-datum)		: $1
     (quasiquoted-datum)	: $1
     (unquoted-datum)		: $1
     (unquoted-splicing-datum)	: $1
     (syntax-datum)		: $1
     (quasisyntax-datum)	: $1
     (unsyntax-datum)		: $1
     (unsyntax-splicing-datum)	: $1
     (interlexeme-space)	: $1)

;;; --------------------------------------------------------------------

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
     (OPAREN   datum DOT datum CPAREN)	 : ((pair-datum-maker) yypushback yycustom $2 $4)
     (OBRACKET datum DOT datum CBRACKET) : ((pair-datum-maker) yypushback yycustom $2 $4))

    (list
     (OPAREN   paren-list-tail)		: ((list-datum-maker) yypushback yycustom $2)
     (OBRACKET bracket-list-tail)	: ((list-datum-maker) yypushback yycustom $2))
    (paren-list-tail
     (CPAREN)				: '()
     (datum DOT datum CPAREN)		: (cons $1 $3)
     (datum paren-list-tail)		: (cons $1 $2))
    (bracket-list-tail
     (CBRACKET)				: '()
     (datum DOT datum CBRACKET)		: (cons $1 $3)
     (datum bracket-list-tail)		: (cons $1 $2))

    (vector
     (SHARPPAREN vector-tail)		: ((vector-datum-maker) yypushback yycustom $2))
    (vector-tail
     (CPAREN)				: '()
     (datum vector-tail)		: (cons $1 $2))

    (bytevector
     (SHARPVU8PAREN bvector-tail)	: ((bytevector-datum-maker) yypushback yycustom $2))
    (bvector-tail
     (CPAREN)				: '()
     ;;Let the parametrised function detect  the problem if DATUM is not
     ;;a u8 integer.
     (datum bvector-tail)		: (cons $1 $2))

;;; --------------------------------------------------------------------

    (quoted-datum
     (TICK datum)			: ((quoted-datum-maker) yypushback yycustom $2))
    (quasiquoted-datum
     (BACKTICK datum)			: ((quasiquoted-datum-maker) yypushback yycustom $2))
    (unquoted-datum
     (COMMA datum)			: ((unquoted-datum-maker) yypushback yycustom $2))
    (unquoted-splicing-datum
     (COMMAAT datum)			: ((unquoted-splicing-datum-maker) yypushback yycustom $2))

;;; --------------------------------------------------------------------

    (syntax-datum
     (SHARPTICK datum)			: ((syntax-datum-maker) yypushback yycustom $2))
    (quasisyntax-datum
     (SHARPBACKTICK datum)		: ((quasisyntax-datum-maker) yypushback yycustom $2))
    (unsyntax-datum
     (SHARPCOMMA datum)			: ((unsyntax-datum-maker) yypushback yycustom $2))
    (unsyntax-splicing-datum
     (SHARPCOMMAAT datum)		: ((unsyntax-splicing-datum-maker) yypushback yycustom $2))

;;; --------------------------------------------------------------------

    (interlexeme-space
     (atmosphere)		: ((interlexeme-space-datum-maker) yypushback yycustom (list $1))
     (atmosphere
      atmosphere-tail)		: ((interlexeme-space-datum-maker) yypushback yycustom (cons $1 $2)))
    (atmosphere
     (WHITESPACE)		: ((whitespace-datum-maker) yypushback yycustom $1)
     (LINEENDING)		: ((whitespace-datum-maker) yypushback yycustom $1)
     (comment)			: $1)
    (atmosphere-tail
     (atmosphere)		: (list $1)
     (atmosphere
      atmosphere-tail)		: (cons $1 $2))

    (comment
     (LINECOMMENT)		: ((line-comment-datum-maker)    yypushback yycustom $1)
     (NESTEDCOMMENT)		: ((nested-comment-datum-maker)  yypushback yycustom $1)
     (SHARPBANGR6RS)		: ((sharp-bang-r6rs-datum-maker) yypushback yycustom $1)
     (SHARPBANG)		: ((sharp-bang-datum-maker)      yypushback yycustom $1)
     (SHARPSEMICOLON
      interlexeme-space datum)	: ((sharp-semicolon-datum-maker) yypushback yycustom $2 $3)
     (SHARPSEMICOLON datum)	: ((sharp-semicolon-datum-maker) yypushback yycustom #f $2))

;;; --------------------------------------------------------------------

    (paren-mismatch
     (OPAREN paren-mismatch-tail)		#;empty)
    (paren-mismatch-tail
     (error CBRACKET)				;empty
     (datum paren-mismatch-tail)		: (cons $1 $2))

    (bracket-mismatch
     (OBRACKET bracket-mismatch-tail)		#;empty)
    (bracket-mismatch-tail
     (error CPAREN)				;empty
     (datum bracket-mismatch-tail)		: (cons $1 $2))

    )))

;;; end of file
