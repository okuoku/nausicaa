;;; -*- mode: scheme; coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexer for Ipv4 address textual representation
;;;Date: Fri Jun 11, 2010
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

decdig			[0-9]
;; hexdig			[0-9A-Fa-f]
;; octdig			[0-7]

decint5			25[0-5]
decint4			2[0-4]{decdig}
decint3			1{decdig}{decdig}
decint2			[1-9]{decdig}
decint1			{decdig}
decint			{decint1}|{decint2}|{decint3}|{decint4}|{decint5}

lenint4			3[0-2]
lenint3			2[0-9]
lenint2			1[0-9]
lenint1			[0-9]
lenint			{lenint1}|{lenint2}|{lenint3}|{lenint4}

;; hexint			0x{hexdig}{hexdig}?

;; octint			0{octdig}({octdig}({octdig})?)?

prefix-length		/{lenint}

DOT			\.

%%

{DOT}			(make-<lexical-token> 'DOT
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\. 1)

{decint}		(make-<lexical-token> 'NUMBER
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (string->number yytext) (string-length yytext))

;; {hexint}		(make-<lexical-token> 'NUMBER
;; 			 (make-<source-location> #f yyline yycolumn yyoffset)
;; 			 (string->number (substring yytext 2 (string-length yytext)) 16)
;; 			 (string-length yytext))

;; {octint}		(make-<lexical-token> 'NUMBER
;; 			 (make-<source-location> #f yyline yycolumn yyoffset)
;; 			 (string->number (substring yytext 1 (string-length yytext)) 8)
;; 			 (string-length yytext))

{prefix-length}		(make-<lexical-token> 'PREFIX-LENGTH
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (string->number (substring yytext 1 (string-length yytext)))
			 (string-length yytext))

<<EOF>>			(make-<lexical-token> '*eoi*
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (eof-object) 0)

<<ERROR>>		(silex-default-error-handler)

;;; end of file
