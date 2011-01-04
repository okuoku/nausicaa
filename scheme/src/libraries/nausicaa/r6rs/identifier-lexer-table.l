;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: character lexer
;;;Date: Tue Jan  4, 2011
;;;
;;;Abstract
;;;
;;;	This  table defines a  lexer for  Scheme identifiers  defined by
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

%%

{IDENTIFIER}		(string->symbol yytext)
{PECULIAR_IDENTIFIER}	(string->symbol yytext)

<<EOF>>			(silex-default-eof-handler)
<<ERROR>>		(silex-default-error-handler)

;;; end of file
