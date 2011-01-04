;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: number lexer
;;;Date: Tue Jan  4, 2011
;;;
;;;Abstract
;;;
;;;	This table defines a lexer for Scheme numbers defined by R6RS.
;;;
;;;Copyright (c) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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

;;;Notice that where the R6RS syntax specifies the <empty> lexeme, we
;;;use the R? regular expression operator.

NANINF			"nan.0"|"inf.0"
DIGIT			[0-9]
HEXDIGIT		{DIGIT}|[a-fA-F]
DIGIT2			[01]
DIGIT8			[01234567]
DIGIT10			{DIGIT}
DIGIT16			{HEXDIGIT}
RADIX2			(#b)|(#B)
RADIX8			(#o)|(#O)
RADIX10			((#d)|(#D))?
RADIX16			(#x)|(#X)
EXACTNESS		((#i)|(#I)|(#e)|(#E))?
EXPONENT-MARKER		e|E|s|S|f|F|d|D|l|L
SIGN			(\+|\-)?
SUFFIX			({EXPONENT-MARKER}{SIGN}{DIGIT10}+)?
MANTISSA-WIDTH		(\|{DIGIT10}+)?

;;; --------------------------------------------------------------------

UINTEGER2		{DIGIT2}+
UREAL2			{UINTEGER2}|({UINTEGER2}/{UINTEGER2})
REAL2			({SIGN}{UREAL2})|(\+{NANINF})|(\-{NANINF})

PREFIX2			({RADIX2}{EXACTNESS})|({EXACTNESS}{RADIX2})
COMPLEX2		{REAL2}|{REAL2}@{REAL2}|({REAL2}\+{UREAL2}i)|({REAL2}\-{UREAL2}i)|({REAL2}\+{NANINF}i)|({REAL2}\-{NANINF}i)|({REAL2}\+i)|({REAL2}\-i)|(\+{UREAL2}i)|(\-{UREAL2}i)|(\+{NANINF}i)|(\-{NANINF}i)|(\+i)|(\-i)
NUMBER2			{PREFIX2}{COMPLEX2}

;;; --------------------------------------------------------------------

UINTEGER8		{DIGIT8}+
UREAL8			{UINTEGER8}|({UINTEGER8}/{UINTEGER8})
REAL8			({SIGN}{UREAL8})|(\+{NANINF})|(\-{NANINF})

PREFIX8			({RADIX8}{EXACTNESS})|({EXACTNESS}{RADIX8})
COMPLEX8		{REAL8}|{REAL8}@{REAL8}|({REAL8}\+{UREAL8}i)|({REAL8}\-{UREAL8}i)|({REAL8}\+{NANINF}i)|({REAL8}\-{NANINF}i)|({REAL8}\+i)|({REAL8}\-i)|(\+{UREAL8}i)|(\-{UREAL8}i)|(\+{NANINF}i)|(\-{NANINF}i)|(\+i)|(\-i)
NUMBER8			{PREFIX8}{COMPLEX8}

;;; --------------------------------------------------------------------

UINTEGER10		{DIGIT10}+
;;This rule is unique for base 10.
DECIMAL10		({UINTEGER10}{SUFFIX})|(\.{DIGIT10}+{SUFFIX})|({DIGIT10}+\.{DIGIT10}*{SUFFIX})|({DIGIT10}+\.{SUFFIX})
;;This rule is unique for base 10 in the last alternative.
UREAL10			({UINTEGER10})|({UINTEGER10}/{UINTEGER10})|({DECIMAL10}{MANTISSA-WIDTH})
REAL10			({SIGN}{UREAL10})|(\+{NANINF})|(\-{NANINF})

PREFIX10		({RADIX10}{EXACTNESS})|({EXACTNESS}{RADIX10})
COMPLEX10		{REAL10}|{REAL10}@{REAL10}|({REAL10}\+{UREAL10}i)|({REAL10}\-{UREAL10}i)|({REAL10}\+{NANINF}i)|({REAL10}\-{NANINF}i)|({REAL10}\+i)|({REAL10}\-i)|(\+{UREAL10}i)|(\-{UREAL10}i)|(\+{NANINF}i)|(\-{NANINF}i)|(\+i)|(\-i)
NUMBER10		{PREFIX10}{COMPLEX10}

;;; --------------------------------------------------------------------

UINTEGER16		{DIGIT16}+
UREAL16			{UINTEGER16}|({UINTEGER16}/{UINTEGER16})
REAL16			({SIGN}{UREAL16})|(\+{NANINF})|(\-{NANINF})

PREFIX16		({RADIX16}{EXACTNESS})|({EXACTNESS}{RADIX16})
COMPLEX16		{REAL16}|{REAL16}@{REAL16}|({REAL16}\+{UREAL16}i)|({REAL16}\-{UREAL16}i)|({REAL16}\+{NANINF}i)|({REAL16}\-{NANINF}i)|({REAL16}\+i)|({REAL16}\-i)|(\+{UREAL16}i)|(\-{UREAL16}i)|(\+{NANINF}i)|(\-{NANINF}i)|(\+i)|(\-i)
NUMBER16		{PREFIX16}{COMPLEX16}

;;; --------------------------------------------------------------------

;;<whitespace> is one among:
;;
;;		\9	char tabulation
;;		\10	linefeed
;;		\11	line tabulation
;;		\12	form feed
;;		\13	carriage return
;;		\133	next line
;;
;;and  any char  whose  Unicode category  is  Zs, Zl  or  Zp; these  are
;;(http://www.fileformat.info/info/unicode/category/index.htm):
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
;;Zl		#x2028		\8232
;;Zp		#x2029		\8233
;;
WHITESPACE		\9\10\11\12\13\32\160\5760\6158\8192\8193\8194\8195\8196\8197\8198\8199\8200\8201\8202\8239\8287\12288\8232\8233

COMMENT			#;

;;; --------------------------------------------------------------------

NUMBER			{NUMBER2}|{NUMBER8}|{NUMBER10}|{NUMBER16}
WRONG_NUMBER		{NUMBER}[^{WHITESPACE}{COMMENT}]+

%%

{NUMBER}		(let ((n (string->number yytext)))
			  (or n (silex-default-error-handler)))

{WRONG_NUMBER}		(silex-default-error-handler yytext)

<<EOF>>			(silex-default-eof-handler)
<<ERROR>>		(silex-default-error-handler)

;;; end of file
