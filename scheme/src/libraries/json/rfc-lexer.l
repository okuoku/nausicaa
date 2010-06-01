;;; -*- mode: scheme; coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexer for json text
;;;Date: Sun May 30, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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

;;; structural characters

;; space, horizontal tab, line feed, carriage return
WS			[ \9\10\13]*

;; [ left square bracket
BEGIN_ARRAY		{WS}\[{WS}

;; { left curly bracket
BEGIN_OBJECT		{WS}\{{WS}

;; ] right square bracket
END_ARRAY		{WS}\]{WS}

;; } right curly bracket
END_OBJECT		{WS}\}{WS}

;; : colon
NAME_SEPARATOR		{WS}:{WS}

;; , comma
VALUE_SEPARATOR		{WS},{WS}

START_QUOTED_TEXT	\"

;;; literal names

THE_FALSE		"false"
THE_NULL		"null"
THE_TRUE		"true"


;;; numbers

decint			[0-9]
integer			{decint}+
exponent		([eE][+\-]?[0-9]+)
real			{decint}+\.|{decint}*\.{decint}+{exponent}?|{decint}+{exponent}
number			{real}|{integer}

;;This is just in case we decide to support binary, octal and hex
;;numbers
;;
;;binint          #[bB][01]+
;;octint          #[oO][0-7]+
;;hexint          #[xX][0-9A-Fa-f]+
;;integer	{decint}|{binint}|{octint}|{hexint}

%%

{BEGIN_ARRAY}		(make-<lexical-token> 'BEGIN_ARRAY
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\[ 1)

{BEGIN_OBJECT}		(make-<lexical-token> 'BEGIN_OBJECT
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\{ 1)

{END_ARRAY}		(make-<lexical-token> 'END_ARRAY
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\] 1)

{END_OBJECT}		(make-<lexical-token> 'END_OBJECT
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\} 1)

{NAME_SEPARATOR}	(make-<lexical-token> 'NAME_SEPARATOR
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\: 1)

{VALUE_SEPARATOR}	(make-<lexical-token> 'VALUE_SEPARATOR
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\, 1)

{THE_FALSE}		(make-<lexical-token> 'FALSE
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #f 5)

{THE_TRUE}		(make-<lexical-token> 'TRUE
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #t 4)

{THE_NULL}		(make-<lexical-token> 'NULL
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 '() 4)

{NUMBER}		(make-<lexical-token> 'NUMBER
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (string->number yytext) (string-length yytext))

{START_QUOTED_TEXT}	(make-<lexical-token> 'QUOTED-TEXT-OPEN
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\" 1)

<<EOF>>			(make-<lexical-token> '*eoi*
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (eof-object) 0)

<<ERROR>>		(silex-default-error-handler)

;;; end of file
