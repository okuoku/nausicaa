;;; -*- mode: scheme; coding: utf-8; page-delimiter: "^;;page" -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexer for R6RS source code
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

;;page
;;;; delimiters and datum opening

OPAREN			\(
CPAREN			\)
OBRACKET		\[
CBRACKET		\]
TICK			'
BACKTICK		`
COMMA			,
COMMAAT			,@
DOT			\.
DOUBLEQUOTE		\"
SEMICOLON		\;
SHARP			#
SHARPPAREN		#\(
SHARPVU8PAREN		#vu8\(
SHARPTICK		#'
SHARPBACKTICK		#`
SHARPCOMMA		#,
SHARPCOMMAAT		#,@
SHARPSEMICOLON		#\;

;;page
;;;; whitespace and comments

%[whitespaces-and-comments.macros.l]

;; this is defined only for reference, it is not used
DELIMITER		{OPAREN}|{CPAREN}|{OBRACKET}|{CBRACKET}|{DOUBLEQUOTE}|{SEMICOLON}|{SHARP}|{WHITESPACE-CHARS}

;;page
;;;; identifiers

%[identifier.macros.l]

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

%[number.macros.l]

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
