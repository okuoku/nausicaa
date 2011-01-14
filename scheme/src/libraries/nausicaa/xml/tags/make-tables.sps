;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: build tables for XML processing
;;;Date: Fri Jan 14, 2011
;;;
;;;Abstract
;;;
;;;
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
  (prefix (nausicaa silex) lex.)
  (prefix (nausicaa lalr)  lalr.))

(lex.lex (lex.input-file:	"lexer-table.l")
	 (lex.output-file:	"lexer-table.sls")
	 (lex.library-spec:	"(nausicaa xml tags lexer-table)")
	 (lex.library-imports:	'((nausicaa parser-tools lexical-token)
				  (nausicaa parser-tools source-location)
				  (nausicaa xml tags lexeme-processing)))
	 (lex.table-name:	'xml-lexer-table)
	 (lex.lexer-format:	'decision-tree) ;this format makes the library 123KB in size
	 (lex.counters:		'all))


(lalr.lalr-parser

 (lalr.output-file:	"parser-table.sls")
 (lalr.parser-name:	'make-xml-parser)
 (lalr.library-spec:	'(nausicaa xml tags parser-table))
 (lalr.library-imports:	'((nausicaa xml tags datum-processing)))

 (lalr.terminals:	'( ;;
			  OTAG		CTAG))

 (lalr.rules:
  '((document
     (prolog element Misc)	: ((document-datum-maker) yypushback yycustom $1 $2 $3))

    (prolog
     (OTAG)			: $1)

    (element
     (OTAG)			: $1)

    (Misc
     (Misc-tail)		: ((misc-datum-maker)	yypushback yycustom $1))
    (Misc-tail
     (OTAG)			: $1
     (OTAG Misc-tail)		: (cons $1 $2))

    )))

;;; end of file
