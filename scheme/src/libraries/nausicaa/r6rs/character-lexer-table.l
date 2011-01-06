;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: character lexer
;;;Date: Tue Jan  4, 2011
;;;
;;;Abstract
;;;
;;;	This  table defines  a lexer  for Scheme  characters  defined by
;;;	R6RS.
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

;;When a valid lexeme is followed by this tail it is wrong.  This is the
;;negation of the <delimiter> element.
TAIL_OF_WRONG		[^\9\10\11\12\13\32\160\5760\6158\8192\8193\8194\8195\8196\8197\8198\8199\8200\8201\8202\8239\8287\12288\8232\8233\(\)\[\]\"\;#]+

SHARPBACKSLASH		#\\
DIGIT			[0-9]
HEXDIGIT		{DIGIT}|[a-fA-F]
HEX_SCALAR_VALUE	{HEXDIGIT}+
CHARACTER_NAME		nul|alarm|backspace|tab|linefeed|newline|vtab|page|return|esc|space|delete

NAMED_CHARACTER		{SHARPBACKSLASH}{CHARACTER_NAME}
HEX_CHARACTER		{SHARPBACKSLASH}x{HEX_SCALAR_VALUE}
LITERAL_CHARACTER	{SHARPBACKSLASH}.

WRONG_NAMED_CHARACTER	{NAMED_CHARACTER}{TAIL_OF_WRONG}
WRONG_HEX_CHARACTER	{SHARPBACKSLASH}x{HEX_SCALAR_VALUE}{TAIL_OF_WRONG}
WRONG_LITERAL_CHARACTER	{SHARPBACKSLASH}.{TAIL_OF_WRONG}

%%

;;Notice that we cannot use a CASE with strings; refer to the definition
;;of EQV? in the R6RS document.
{NAMED_CHARACTER}	(cond
			  ((string=? yytext "#\\nul")		#\nul)
			  ((string=? yytext "#\\alarm")		#\alarm)
			  ((string=? yytext "#\\backspace")	#\backspace)
			  ((string=? yytext "#\\tab")		#\tab)
			  ((string=? yytext "#\\linefeed")	#\linefeed)
			  ((string=? yytext "#\\newline")	#\newline)
			  ((string=? yytext "#\\vtab")		#\vtab)
			  ((string=? yytext "#\\page")		#\page)
			  ((string=? yytext "#\\return")	#\return)
			  ((string=? yytext "#\\esc")		#\esc)
			  ((string=? yytext "#\\space")		#\space)
			  ((string=? yytext "#\\delete")	#\delete)
			  (else
			   ;;Notice that this should never happen.
			   (assertion-violation 'r6rs-character-lexer-table
			     "internal error, invalid named character" yytext)))

{HEX_CHARACTER}		(let* ((len (string-length yytext))
			       (num (string->number (substring yytext 3 len) 16)))
			  (if (or (<= 0 num #xD7FF) (<= #xE000 num #x10FFFF))
			      (integer->char num)
			    (make-<lexical-token> '*lexer-error*
						  (make-<source-location> #f yyline yycolumn yyoffset)
						  yytext len)))

{LITERAL_CHARACTER}	(string-ref yytext 2)

{WRONG_NAMED_CHARACTER}	  ((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{WRONG_HEX_CHARACTER}	  ((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)
{WRONG_LITERAL_CHARACTER} ((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

<<EOF>>			((eof-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
<<ERROR>>		((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;; end of file
