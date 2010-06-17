;;; -*- mode: scheme; coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexer for generic URI grammar
;;;Date: Thu Jun 17, 2010
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

COLON			:
SLASH			/
QUESTION		\?
NUMBER-SIGN		#
COMPONENT		[^{COLON}{SLASH}{QUESTION}{NUMBER-SIGN}]+

%%

{COLON}			(make-<lexical-token> 'COLON
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\: 1)

{SLASH}			(make-<lexical-token> 'SLASH
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\/ 1)

{QUESTION}		(make-<lexical-token> 'QUESTION
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\? 1)

{NUMBER-SIGN}		(make-<lexical-token> 'NUMBER-SIGN
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\# 1)

{COMPONENT}		(make-<lexical-token> 'COMPONENT
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 yystring (string-length yystring))

<<EOF>>			(make-<lexical-token> '*eoi*
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (eof-object) 0)

<<ERROR>>		(silex-default-error-handler)

;;; end of file
