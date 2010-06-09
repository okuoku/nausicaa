;;; -*- mode: scheme; coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexer for IPv6 address textual representation
;;;Date: Wed Jun  9, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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

;;; structural characters

;; space, horizontal tab, line feed, carriage return
;;WS			[ \9\10\13]*

decdig			[0-9]
hexdig			[0-9A-Fa-f]

hexint			{decdig}({decdig}({decdig}({decdig})?)?)?

COLON			:

%%

{COLON}			(make-<lexical-token> 'COLON
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\: 1)

{hexint}		(make-<lexical-token> 'HEXINT
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 yytext (string-length yytext))

<<EOF>>			(make-<lexical-token> '*eoi*
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (eof-object) 0)

<<ERROR>>		(silex-default-error-handler)

;;; end of file
