;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: string lexer table for CSV files
;;;Date: Tue Jul 21, 2009
;;;
;;;Abstract
;;;
;;;	This file  defines a lexer table  to be used  while tokenising a
;;;	string  in CSV a file.  Another lexer  must  detect the  opening
;;;	double-quote,  then  this  table   is  used  until  the  closing
;;;	double-quote is found.
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

%%

\"\"		(begin #\")	;a double double-quote is a
				;nested double-quote

\"		(begin #f)	;a single double-quote is the
				;end of string

.		(string-ref yytext 0)	;any character which is not a
					;double-quote

<<EOF>>		(assertion-violation #f
                  "while parsing string, found end of input before closing double-quote")

<<ERROR>>	(assertion-violation #f
                  (string-append "invalid token at line "
				 (number->string yyline)
				 " column "
				 (number->string yycolumn))
		  yytext)


;;; end of file
;; Local Variables:
;; mode: fundamental
;; End:
