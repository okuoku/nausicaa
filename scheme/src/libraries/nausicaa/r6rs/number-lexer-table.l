;;; -*- mode: scheme; coding: utf-8; page-delimiter: "^;;page" -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: number lexer table
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

;;page
;;Notice that where the R6RS syntax specifies the <empty> lexeme, we use
;;the R? regular expression operator.

%[../silex/unicode.macros.l]
%[whitespaces-and-comments.macros.l]
%[number.macros.l]

WRONG_NUMBER		{NUMBER}{TAIL_OF_WRONG}

%%

{NUMBER}		(let ((n (string->number yytext)))
			  (or n ((lexical-error-token-maker)
				 yygetc yyungetc yytext yyline yycolumn yyoffset)))

{WRONG_NUMBER}		((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

<<EOF>>			((eoi-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
<<ERROR>>		((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;; end of file
