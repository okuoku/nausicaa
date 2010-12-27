;;; -*- mode: scheme; coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexer for IPv6 address textual representation
;;;Date: Wed Jun  9, 2010
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
;;WS			[ \9\10\13]*

decdig			[0-9]
hexdig			[0-9A-Fa-f]

decint5			25[0-5]
decint4			2[0-4]{decdig}
decint3			[01]{decdig}{decdig}
decint2			{decdig}{decdig}
decint1			{decdig}
decint			{decint1}|{decint2}|{decint3}|{decint4}|{decint5}

hexint4			{hexdig}{hexdig}{hexdig}{hexdig}
hexint3			{hexdig}{hexdig}{hexdig}
hexint2			{hexdig}{hexdig}
hexint1			{hexdig}
hexint			{hexint1}|{hexint2}|{hexint3}|{hexint4}

number			{decint}|{hexint}

COLON			:
DOT			\.
SLASH			/

%%

{COLON}			(make-<lexical-token> 'COLON
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\: 1)

{DOT}			(make-<lexical-token> 'DOT
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\. 1)

{SLASH}			(make-<lexical-token> 'SLASH
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\/ 1)

{number}		(make-<lexical-token> 'NUMBER
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 yytext (string-length yytext))

<<EOF>>			(make-<lexical-token> '*eoi*
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (eof-object) 0)

<<ERROR>>		(silex-default-error-handler)

;;; end of file
