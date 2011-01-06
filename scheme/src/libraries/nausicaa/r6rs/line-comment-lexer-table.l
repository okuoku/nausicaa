;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: line comment lexer
;;;Date: Tue Jan  4, 2011
;;;
;;;Abstract
;;;
;;;	This table defines a exer for line comments "; ...".
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
LINECOMMENT-NOEND	\;[^\10\13\133\8232\8233]*

%%

{LINECOMMENT}		yytext
{LINECOMMENT-NOEND}	(let ((ch (dynamic-wind
				      (lambda () #f)
				      (lambda ()
					(yygetc))
				      (lambda () (yyungetc)))))
			  (if (eof-object? ch)
			      yytext
			    ((lexical-error-token-maker)
			     yygetc yyungetc yytext yyline yycolumn yyoffset)))

<<EOF>>			((eoi-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
<<ERROR>>		((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;; end of file
