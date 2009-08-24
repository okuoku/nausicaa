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

;;US-ASCII control  characters that do not include  the carriage return,
;;line feed, and white space characters.
NO-WS-CTL		[\1-\8\11\12\14-\31\127]

;;See RFC 2822 section "3.2.3. Folding white space and comments".
FWS			\13\10[\9\32]+

WS			[\9\32]+

;;Characters excluding CR and LF.
text-part		[\1-\9\11\12\14-\127]
obs-text		.

;;See RFC 2822 section "3.2.4. Atom".
atom			[0-9A-Za-z!#\$%&'\*\+\-/=\?\^_`\{\|\}~]+

character		[<>@,;:\.]

start-comment		\(
start-quoted-text	\"
start-domain-literal	\[

%%

{FWS}			;ignore it
{WS}			;ignore it

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
