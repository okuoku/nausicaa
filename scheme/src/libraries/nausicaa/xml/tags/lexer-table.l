;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexer for XML documents
;;;Date: Fri Jan 14, 2011
;;;
;;;Abstract
;;;
;;;	A lexer  for XML  aiming to be  compliant with  the ``Extensible
;;;	Markup Language (XML)  1.0 (Fifth Edition)'', W3C Recommendation
;;;	26 November 2008:
;;;
;;;		http://www.w3.org/TR/REC-xml/
;;;		http://www.w3.org/XML/xml-V10-5e-errata
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

;;page

Char			#x9|#xA|#xD|[#x20-#xD7FF]|[#xE000-#xFFFD]|[#x10000-#x10FFFF]


;;page
;;;; comments


;;page
;;;; identifiers

LETTER			[a-zA-Z]
CONSTITUENT		{LETTER}|[\127-\55295]|[\57344-\1114111]
SPECIAL_INITIAL		[!\$%&\*/:<=>\?\^_~]
DIGIT			[0-9]
HEXDIGIT		{DIGIT}|[a-fA-F]
SPECIAL_SUBSEQUENT	[\+\-\.@]
HEX_SCALAR_VALUE	{HEXDIGIT}+
INLINE_HEX_ESCAPE	\\x{HEX_SCALAR_VALUE}\;
INITIAL			{CONSTITUENT}|{SPECIAL_INITIAL}|{INLINE_HEX_ESCAPE}
SUBSEQUENT		{INITIAL}|{DIGIT}|{CHARS-ND-MC-ME}|{SPECIAL_SUBSEQUENT}
IDENTIFIER		{INITIAL}{SUBSEQUENT}*
PECULIAR_IDENTIFIER	[\+\-]|"..."|(\->{SUBSEQUENT}*)

WRONG_IDENTIFIER		{IDENTIFIER}{TAIL_OF_WRONG}

;;We do not include the peculiar  identifiers "+" and "-" here, else the
;;numbers  starting  with  +  and   -  would  be  interpreted  as  wrong
;;identifiers.
;;
WRONG_PECULIAR_IDENTIFIER	("..."|(\->{SUBSEQUENT}*)){TAIL_OF_WRONG}

;;page
;;;; characters

SHARPBACKSLASH		#\\
CHARACTER_NAME		nul|alarm|backspace|tab|linefeed|newline|vtab|page|return|esc|space|delete
NAMED_CHARACTER		{SHARPBACKSLASH}{CHARACTER_NAME}
HEX_CHARACTER		{SHARPBACKSLASH}x{HEX_SCALAR_VALUE}
LITERAL_CHARACTER	{SHARPBACKSLASH}.

WRONG_NAMED_CHARACTER	{NAMED_CHARACTER}{TAIL_OF_WRONG}
WRONG_HEX_CHARACTER	{SHARPBACKSLASH}x{HEX_SCALAR_VALUE}{TAIL_OF_WRONG}
WRONG_LITERAL_CHARACTER	{SHARPBACKSLASH}.{TAIL_OF_WRONG}

;;page
;;;; numbers

NANINF			"nan.0"|"inf.0"
IMAG			i
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
COMPLEX2		{REAL2}|{REAL2}@{REAL2}|({REAL2}\+{UREAL2}{IMAG})|({REAL2}\-{UREAL2}{IMAG})|({REAL2}\+{NANINF}{IMAG})|({REAL2}\-{NANINF}{IMAG})|({REAL2}\+{IMAG})|({REAL2}\-{IMAG})|(\+{UREAL2}{IMAG})|(\-{UREAL2}{IMAG})
NUMBER2			{PREFIX2}{COMPLEX2}

;;; --------------------------------------------------------------------

UINTEGER8		{DIGIT8}+
UREAL8			{UINTEGER8}|({UINTEGER8}/{UINTEGER8})
REAL8			({SIGN}{UREAL8})|(\+{NANINF})|(\-{NANINF})

PREFIX8			({RADIX8}{EXACTNESS})|({EXACTNESS}{RADIX8})
COMPLEX8		{REAL8}|{REAL8}@{REAL8}|({REAL8}\+{UREAL8}{IMAG})|({REAL8}\-{UREAL8}{IMAG})|({REAL8}\+{NANINF}{IMAG})|({REAL8}\-{NANINF}{IMAG})|({REAL8}\+{IMAG})|({REAL8}\-{IMAG})|(\+{UREAL8}{IMAG})|(\-{UREAL8}{IMAG})
NUMBER8			{PREFIX8}{COMPLEX8}

;;; --------------------------------------------------------------------

UINTEGER10		{DIGIT10}+
;;This rule is unique for base 10.
DECIMAL10		({UINTEGER10}{SUFFIX})|(\.{DIGIT10}+{SUFFIX})|({DIGIT10}+\.{DIGIT10}*{SUFFIX})|({DIGIT10}+\.{SUFFIX})
;;This rule is unique for base 10 in the last alternative.
UREAL10			({UINTEGER10})|({UINTEGER10}/{UINTEGER10})|({DECIMAL10}{MANTISSA-WIDTH})
REAL10			({SIGN}{UREAL10})|(\+{NANINF})|(\-{NANINF})

PREFIX10		({RADIX10}{EXACTNESS})|({EXACTNESS}{RADIX10})
COMPLEX10		{REAL10}|{REAL10}@{REAL10}|({REAL10}\+{UREAL10}{IMAG})|({REAL10}\-{UREAL10}{IMAG})|({REAL10}\+{NANINF}{IMAG})|({REAL10}\-{NANINF}{IMAG})|({REAL10}\+{IMAG})|({REAL10}\-{IMAG})|(\+{UREAL10}{IMAG})|(\-{UREAL10}{IMAG})|(\+{NANINF}{IMAG})|(\-{NANINF}{IMAG})|(\+{IMAG})|(\-{IMAG})
NUMBER10		{PREFIX10}{COMPLEX10}

;;; --------------------------------------------------------------------

UINTEGER16		{DIGIT16}+
UREAL16			{UINTEGER16}|({UINTEGER16}/{UINTEGER16})
REAL16			({SIGN}{UREAL16})|(\+{NANINF})|(\-{NANINF})

PREFIX16		({RADIX16}{EXACTNESS})|({EXACTNESS}{RADIX16})
COMPLEX16		{REAL16}|{REAL16}@{REAL16}|({REAL16}\+{UREAL16}{IMAG})|({REAL16}\-{UREAL16}{IMAG})|({REAL16}\+{NANINF}{IMAG})|({REAL16}\-{NANINF}{IMAG})|({REAL16}\+{IMAG})|({REAL16}\-{IMAG})|(\+{UREAL16}{IMAG})|(\-{UREAL16}{IMAG})|(\+{NANINF}{IMAG})
NUMBER16		{PREFIX16}{COMPLEX16}

;;; --------------------------------------------------------------------

NUMBER			{NUMBER2}|{NUMBER8}|{NUMBER10}|{NUMBER16}
WRONG_NUMBER		{NUMBER}{TAIL_OF_WRONG}

;;page
;;;; misc stuff

SHARPBANGR6RS		#!r6rs
SHARPBANGIDENTIFIER	#!({IDENTIFIER}|{PECULIAR_IDENTIFIER})

BOOLEAN			#t|#f|#T|#F
WRONG_BOOLEAN		{BOOLEAN}{TAIL_OF_WRONG}

;;page
%%

{OPAREN}		((open-paren-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{CPAREN}		((close-paren-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{OBRACKET}		((open-bracket-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{CBRACKET}		((close-bracket-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{TICK}			((tick-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
{BACKTICK}		((back-tick-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{COMMAAT}		((comma-at-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
{COMMA}			((comma-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
{DOT}			((dot-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
{DOUBLEQUOTE}		((double-quote-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{SHARPPAREN}		((sharp-paren-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{SHARPVU8PAREN}		((sharp-vu8-paren-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{SHARPTICK}		((sharp-tick-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{SHARPBACKTICK}		((sharp-back-tick-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{SHARPCOMMAAT}		((sharp-comma-at-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{SHARPCOMMA}		((sharp-comma-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{SHARPSEMICOLON}	((sharp-semicolon-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;page
{LINECOMMENT}		((line-comment-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{LINECOMMENT-NOEND}	((line-comment-noend-token-maker)
			 yygetc yyungetc yytext yyline yycolumn yyoffset)
{ONESTEDCOMMENT}	((open-nested-comment-token-maker)
			 yygetc yyungetc yytext yyline yycolumn yyoffset)
{SHARPBANGR6RS}		((sharp-bang-r6rs-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{SHARPBANGIDENTIFIER}	((sharp-bang-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{WHITESPACE}		((white-space-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{LINEENDING}		((line-ending-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;page
;;; identifiers

{IDENTIFIER}		((identifier-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{PECULIAR_IDENTIFIER}	((identifier-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{WRONG_IDENTIFIER}	((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{WRONG_PECULIAR_IDENTIFIER} ((lexical-error-token-maker) yygetc yyungetc yytext yyline yycolumn yyoffset)

;;; booleans

{BOOLEAN}		((boolean-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
{WRONG_BOOLEAN}		((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;; characters

{NAMED_CHARACTER}	((named-character-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{HEX_CHARACTER}		((hex-character-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{LITERAL_CHARACTER}	((literal-character-token-maker)yygetc yyungetc yytext yyline yycolumn yyoffset)
{WRONG_NAMED_CHARACTER}	((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{WRONG_HEX_CHARACTER}	((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{WRONG_LITERAL_CHARACTER} ((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;; numbers

{NUMBER}		((number-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
{WRONG_NUMBER}		((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;page
;;;; done

<<EOF>>			((eoi-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
<<ERROR>>		((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;; end of file
;; Local Variables:
;; page-delimiter: "^;;page"
;; End:
