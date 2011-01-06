;;; -*- mode: scheme; coding: utf-8 -*-
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

DOUBLEQUOTE		\"

ESCAPED-A		\\a
ESCAPED-B		\\b
ESCAPED-T		\\t
ESCAPED-N		\\n
ESCAPED-V		\\v
ESCAPED-F		\\f
ESCAPED-R		\\r
ESCAPED-DOUBLEQUOTE	\\\"
ESCAPED-BACKSLASH	\\\\

ESCAPED-STRING-ELEMENT	{ESCAPED-A}|{ESCAPED-B}|{ESCAPED-T}|{ESCAPED-N}|{ESCAPED-V}|{ESCAPED-F}|{ESCAPED-R}|{ESCAPED-DOUBLEQUOTE}|{ESCAPED-BACKSLASH}

;;<line ending> is:
;;
;;		\10		line feed
;;		\13		carriage return
;;		\133		next line
;;		\10\13		carriage return, line feed
;;		\13\133		carriage return, next line
;;		\8232		line separator
;;
LINEENDING		[\10\13\133\8232]|(\13\10)|(\13\133)

;;<intraline whitespace> is: char tabulation \9, any char whose Unicode
;;category is Zs.
;;
;;Zs		#x0020		\32
;;		#x00A0		\160
;;		#x1680		\5760
;;		#x180E		\6158
;;		#x2000		\8192
;;		#x2001		\8193
;;		#x2002		\8194
;;		#x2003		\8195
;;		#x2004		\8196
;;		#x2005		\8197
;;		#x2006		\8198
;;		#x2007		\8199
;;		#x2008		\8200
;;		#x2009		\8201
;;		#x200A		\8202
;;		#x202F		\8239
;;		#x205F		\8287
;;		#x3000		\12288
;;
INTRALINE-WHITESPACE	[\9\32\160\5760\6158\8192\8193\8194\8195\8196\8197\8198\8199\8200\8201\8202\8239\8287\12288]

LINE-WRAP-TO-IGNORE	\\{INTRALINE-WHITESPACE}*{LINEENDING}{INTRALINE-WHITESPACE}*


DIGIT			[0-9]
HEXDIGIT		{DIGIT}|[a-fA-F]
HEX-SCALAR-VALUE	{HEXDIGIT}+
INLINE-HEX-ESCAPE	\\x{HEX-SCALAR-VALUE}\;

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
