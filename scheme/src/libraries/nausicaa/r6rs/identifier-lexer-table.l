;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: R6RS identifier lexer table
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

%[identifier.macros.l]

;;When a valid lexeme is followed by this tail it is wrong.  This is the
;;negation of the <delimiter> element.
TAIL_OF_WRONG		[^\9\10\11\12\13\32\160\5760\6158\8192\8193\8194\8195\8196\8197\8198\8199\8200\8201\8202\8239\8287\12288\8232\8233\(\)\[\]\"\;#]+

WRONG_IDENTIFIER	{IDENTIFIER}{TAIL_OF_WRONG}

;;We do not include the peculiar  identifiers "+" and "-" here, else the
;;numbers  starting  with  +  and   -  would  be  interpreted  as  wrong
;;identifiers.
;;
WRONG_PECULIAR_IDENTIFIER	("..."|(\->{SUBSEQUENT}*)){TAIL_OF_WRONG}


%%

{IDENTIFIER}		yytext
{PECULIAR_IDENTIFIER}	yytext

{WRONG_IDENTIFIER}	    ((lexical-error-token-maker) yygetc yyungetc yytext yyline yycolumn yyoffset)
{WRONG_PECULIAR_IDENTIFIER} ((lexical-error-token-maker) yygetc yyungetc yytext yyline yycolumn yyoffset)

<<EOF>>			((eoi-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
<<ERROR>>		((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;; end of file
