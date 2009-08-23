;;; -*- mode: scheme; coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexer for email addresses
;;;Date: Fri Jul 31, 2009
;;;
;;;Abstract
;;;
;;;	This file defines a lexer table to be used while tokenising an
;;;	RFC 822 email address.
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

atom			[^\(\"\[<>@,;:\.\9\10\13\32]+
character		[<>@,;:\.]

;;We accept  any sequence  of CR and  LF char  as a valid  newline, even
;;tough  the RFC wants  CR+LF.  After  the newline  it is  mandatory the
;;presence of a space.
proper-newline		[\13\10]+[\9\32]+

space			[\9\32]+

start-comment		\(
start-quoted-text	\"
start-domain-literal	\[

%%

{proper-newline}	;ignore it
{space}			;ignore it

{atom}			(make-lexical-token
			 'ATOM
			 (make-source-location #f yyline yycolumn yyoffset
					       (string-length yytext))
			 yytext)
{character}		(let ((ch (string-ref yytext 0)))
			  (make-lexical-token
			   (case ch
			     ((#\@) 'AT)
			     ((#\.) 'DOT)
			     ((#\,) 'COMMA)
			     ((#\:) 'COLON)
			     ((#\;) 'SEMICOLON)
			     ((#\<) 'ANGLE-OPEN)
			     ((#\>) 'ANGLE-CLOSE))
			   (make-source-location #f yyline yycolumn yyoffset ch)
			   ch))

{start-comment}		(make-lexical-token
			 'COMMENT-OPEN
			 (make-source-location #f yyline yycolumn yyoffset 1)
			 yytext)

{start-quoted-text}	(make-lexical-token
			 'QUOTED-TEXT-OPEN
			 (make-source-location #f yyline yycolumn yyoffset 1)
			 yytext)

{start-domain-literal}	(make-lexical-token
			 'DOMAIN-LITERAL-OPEN
			 (make-source-location #f yyline yycolumn yyoffset 1)
			 yytext)

<<EOF>>			(make-lexical-token
			 '*eoi*
			 (make-source-location #f yyline yycolumn yyoffset 0)
			 (eof-object))

<<ERROR>>		(make-lexical-token
			 '*lexer-error*
			 (make-source-location #f yyline yycolumn yyoffset (string-length yytext))
			 yytext)

;;; end of file
