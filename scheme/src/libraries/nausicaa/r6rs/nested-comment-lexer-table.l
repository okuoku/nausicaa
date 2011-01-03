;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: nested comment lexer
;;;Date: Tue Dec 28, 2010
;;;
;;;Abstract
;;;
;;;	This table defines a sublexer  for nested comments: "#| ... |#";
;;;	nested comments  can be nested.   We must call this  lexer after
;;;	having consumed a "#|" token  from the input stream and continue
;;;	until we consume the matching "|#" token.  This sublexer must be
;;;	called recursively to consume nested comments.
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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

OCOMMENT		#\|
CCOMMENT		\|#

%%

{CCOMMENT}		(make-<lexical-token> 'CCOMMENT
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      yytext 2)

{OCOMMENT}		(make-<lexical-token> 'OCOMMENT
					      (make-<source-location> #f yyline yycolumn yyoffset)
					      yytext 2)

<<EOF>>			(make-<lexical-token> '*eoi*
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (eof-object) 0)

<<ERROR>>		(silex-default-error-handler)

;;; end of file
