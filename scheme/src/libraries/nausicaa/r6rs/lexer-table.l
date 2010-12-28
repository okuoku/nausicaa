;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexer for R6RS source code
;;;Date: Wed Dec 22, 2010
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

OPAREN			\(
CPAREN			\)
OBRACKET		\[
CBRACKET		\]
TICK			'
BACKTICK		`
COMMA			,
COMMAAT			,@
DOT			.
DOUBLEQUOTE		\"
SEMICOLON		\;
SHARPPAREN		#\(
SHARPVU8PAREN		#vu8\(
SHARPTICK		#'
SHARPBACKTICK		#`
SHARPCOMMA		#,
SHARPCOMMAAT		#,@
SHARPSEMICOLON		#\;

;;FIXME Is this actually useful for something?
SHARP			#

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
WHITESPACE		[\9\10\11\12\13\32\160\5760\6158\8192\8193\8194\8195\8196\8197\8198\8199\8200\8201\8202\8239\8287\12288\8232\8233]+

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

;;A simple comment is everything between a semicolon and a LINEENDING or
;;a paragraph separator (#x2029 = \8233).
;;
LINECOMMENT		\;.*({LINEENDING}|\8233)

;;Multiline comments can contain nested multiline comments, so we need a
;;sublexer.
;;
ONESTEDCOMMENT		#\|

SHARPBANGR6RS		#!r6rs
SHARPBANG		#!

;;Identifiers.

LETTER			[a-zA-Z]
CONSTITUENT		{LETTER}|[\127-\55295]|[\57344-\1114111]
SPECIAL_INITIAL		[!\$%&\*/:<=>\?\^_~]
;;FIXME add below: any char whose category is Nd, Mc, Me
DIGIT			[0-9]
HEXDIGIT		{DIGIT}|[a-fA-F]
SPECIAL_SUBSEQUENT	[\+\-\.@]
HEX_SCALAR_VALUE	{HEXDIGIT}+
INLINE_HEX_ESCAPE	\\x{HEX_SCALAR_VALUE}\;
INITIAL			{CONSTITUENT}|{SPECIAL_INITIAL}|{INLINE_HEX_ESCAPE}
SUBSEQUENT		{INITIAL}|{DIGIT}|{SPECIAL_SUBSEQUENT}
IDENTIFIER		{INITIAL}{SUBSEQUENT}*

PECULIAR_IDENTIFIER	[\+\-]|"..."|\->{SUBSEQUENT}*

BOOLEAN			#t|#f|#T|#F

CHARACTER_NAME		nul|alarm|backspace|tab|linefeed|newline|vtab|page|return|esc|space|delete
SHARPBACKSLASH		#\\
CHARACTER		{SHARPBACKSLASH}((x{HEX_SCALAR_VALUE})|{CHARACTER_NAME}|.)

%%

{OPAREN}		(make-<lexical-token> 'OPAREN
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      #\( 1)

{CPAREN}		(make-<lexical-token> 'CPAREN
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      #\) 1)

{OBRACKET}		(make-<lexical-token> 'OBRACKET
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      #\[ 1)

{CBRACKET}		(make-<lexical-token> 'CBRACKET
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      #\] 1)

{TICK}			(make-<lexical-token> 'TICK
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      #\' 1)

{BACKTICK}		(make-<lexical-token> 'BACKTICK
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      #\` 1)

{COMMAAT}		(make-<lexical-token> 'COMMAAT
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      ",@" 2)

{COMMA}			(make-<lexical-token> 'COMMA
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      #\, 1)

{DOT}			(make-<lexical-token> 'DOT
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      #\. 1)

{DOUBLEQUOTE}		(make-<lexical-token> 'DOUBLEQUOTE
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      #\" 1)

{SEMICOLON}		(make-<lexical-token> 'SEMICOLON
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      #\; 1)

{SHARPPAREN}		(make-<lexical-token> 'SHARPPAREN
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      "#(" 2)

{SHARPVU8PAREN}		(make-<lexical-token> 'SHARPVU8PAREN
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      "#vu8(" 4)

{SHARPTICK}		(make-<lexical-token> 'SHARPTICK
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      "#'" 2)

{SHARPBACKTICK}		(make-<lexical-token> 'SHARPBACKTICK
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      "#`" 2)


{SHARPCOMMAAT}		(make-<lexical-token> 'SHARPCOMMAAT
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      "#,@" 3)

{SHARPCOMMA}		(make-<lexical-token> 'SHARPCOMMA
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      #\( 1)

{SHARPSEMICOLON}	(make-<lexical-token> 'SHARPSEMICOLON
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      "#;" 2)

{LINECOMMENT}		(make-<lexical-token> 'LINECOMMENT
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      yytext (string-length yytext))

{ONESTEDCOMMENT}	(make-<lexical-token> 'ONESTEDCOMMENT
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      "#|" 2)

{SHARPBANGR6RS}		(make-<lexical-token> 'SHARPBANGR6RS
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      yytext 6)

{SHARPBANG}		(make-<lexical-token> 'SHARPBANG
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      "#!" 2)

{WHITESPACE}		(make-<lexical-token> 'WHITESPACE
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      yytext (string-length yytext))

{LINEENDING}		(make-<lexical-token> 'LINEENDING
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      yytext (string-length yytext))

{IDENTIFIER}		(make-<lexical-token> 'IDENTIFIER
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      (string->symbol yytext) (string-length yytext))

{PECULIAR_IDENTIFIER}	(make-<lexical-token> 'IDENTIFIER
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      (string->symbol yytext) (string-length yytext))

{BOOLEAN}		(make-<lexical-token> 'BOOLEAN
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      (case yytext
						(("#t" "#T")	#t)
						(("#f" "#F"	#f)))
					      2)

{CHARACTER}		(make-<lexical-token> 'character
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      yytext (string-length yytext))

<<EOF>>			(make-<lexical-token> '*eoi*
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (eof-object) 0)

<<ERROR>>		(silex-default-error-handler)

;;; end of file
