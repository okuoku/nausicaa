;;; -*- mode: scheme; coding: utf-8; page-delimiter: "^;;page" -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: R6RS string lexer table
;;;Date: Tue Dec 28, 2010
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

;;page
%[string.macros.l]

STRING-ELEMENT		[^\"\\]+

%%

{DOUBLEQUOTE}		(quote STRING)

{ESCAPED-A}		#\x7
{ESCAPED-B}		#\x8
{ESCAPED-T}		#\x9
{ESCAPED-N}		#\xA
{ESCAPED-V}		#\xB
{ESCAPED-F}		#\xC
{ESCAPED-R}		#\xD
{ESCAPED-DOUBLEQUOTE}	#\x22
{ESCAPED-BACKSLASH}	#\x5C


{LINE-WRAP-TO-IGNORE}	;;ignored

{INLINE-HEX-ESCAPE}	(let* ((len (string-length yytext))
			       (num (string->number (substring yytext 2 (- len 1)) 16)))
			  (if (or (<= 0 num #xD7FF) (<= #xE000 num #x10FFFF))
			      (integer->char num)
			    ((lexical-error-token-maker)
			     yygetc yyungetc yytext yyline yycolumn yyoffset)))

{STRING-ELEMENT}	yytext

<<EOF>>			((eoi-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
<<ERROR>>		((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;; end of file
