;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: unquoted data lexer table for CSV files
;;;Date: Tue Jul 21, 2009
;;;
;;;Abstract
;;;
;;;	This file defines a lexer table to be used while tokenising
;;;	unquoted data in a CSV file.
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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

;; Every sequence of #\newline and #\return is collapsed into
;; a single end of line.
end-of-line	[\10\13]+

;; A single double-quote is the start of string.
string-start	\"

;; Any character which is not a double-quote or end-of-line.
any-char	.

%%

{end-of-line}	(begin 'eol)
{string-start}	(begin 'string)
{any-char}	(string-ref yytext 0)
<<EOF>>		(begin #f)
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
