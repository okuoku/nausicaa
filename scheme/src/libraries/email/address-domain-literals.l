;;; -*- mode: fundamental; coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: string lexer for domain literals in email addresses
;;;Date: Fri Jul 31, 2009
;;;
;;;Abstract
;;;
;;;	This file  defines a lexer table  to be used  while tokenising a
;;;	domain literal in an RFC  822 email address.  Another lexer must
;;;	detect the  opening square parenthesis, then this  table is used
;;;	until the closing square parenthesis is found.
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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

end-of-dtext		\]
integer			[0-9]+
dot			\.

%%

{end-of-dtext}		(make-lexical-token
			 'DOMAIN-LITERAL-CLOSE
			 (make-source-location #f yyline yycolumn yyoffset
			                       (string-length yytext))
			 yytext)

{integer}		(let ((num (string->number yytext)))
			  (make-lexical-token
			   (if (< num 256) 'DOMAIN-LITERAL-INTEGER 'INVALID-VALUE)
			   (make-source-location #f yyline yycolumn yyoffset
						 (string-length yytext))
			   yytext))

{dot}			(make-lexical-token
			 'DOT
			 (make-source-location #f yyline yycolumn yyoffset 1)
			 yytext)

<<EOF>>			(make-lexical-token
			 '*eoi*
			 (make-source-location #f yyline yycolumn yyoffset 0)
			 (eof-object))

<<ERROR>>		(make-lexical-token
			 'INVALID-VALUE
			 (make-source-location #f yyline yycolumn yyoffset
					       (string-length yytext))
			 yytext)

;;; end of file
