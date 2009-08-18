;;; -*- mode: fundamental; coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: string lexer for strings in email addresses
;;;Date: Thu Jul 30, 2009
;;;
;;;Abstract
;;;
;;;	This file  defines a lexer table  to be used  while tokenising a
;;;	string in an  RFC 822 email address.  Another  lexer must detect
;;;	the  opening double-quote,  then this  table is  used  until the
;;;	closing double-quote is found.
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

double-quote		\"
quoted-string		([^\"]+|\\\")+

%%

{double-quote}		'QUOTED-STRING-CLOSE

{quoted-string}		yytext

<<EOF>>			(make-lexical-token
			 '*eoi*
			 (make-source-location #f yyline yycolumn yyoffset 0)
			 (eof-object))

<<ERROR>>		(make-lexical-token
			 'INVALID-CHARACTER
			 (make-source-location #f yyline yycolumn yyoffset
					       (string-length yytext))
			 yytext)

;;; end of file
