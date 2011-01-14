;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexer for XML documents
;;;Date: Fri Jan 14, 2011
;;;
;;;Abstract
;;;
;;;	A lexer  for XML  aiming to be  compliant with  the ``Extensible
;;;	Markup Language (XML)  1.0 (Fifth Edition)'', W3C Recommendation
;;;	26 November 2008:
;;;
;;;		http://www.w3.org/TR/REC-xml/
;;;		http://www.w3.org/XML/xml-V10-5e-errata
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

Char			#x9|#xA|#xD|[#x20-#xD7FF]|[#xE000-#xFFFD]|[#x10000-#x10FFFF]


;;page
%%

{Char}		((open-paren-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;page
;;;; done

<<EOF>>			((eoi-token-maker)		yygetc yyungetc yytext yyline yycolumn yyoffset)
<<ERROR>>		((lexical-error-token-maker)	yygetc yyungetc yytext yyline yycolumn yyoffset)

;;; end of file
;; Local Variables:
;; page-delimiter: "^;;page"
;; End:
