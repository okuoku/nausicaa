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

atom			[^\(\"\[<>@,;:\.\9\32\10\13]+
character		[<>@,;:\.]

;;We accept  any sequence  of CR and  LF char  as a valid  newline, even
;;tough  the RFC wants  CR+LF.  After  the newline  it is  mandatory the
;;presence of a space.
proper-newline		[\13\10]+[\9\32]+

space			[\9\32]+

open-comment		\(
open-quoted-text	\"
open-domain-literal	\[
;;;domain-literal		\[.+\]

%%

{proper-newline}	;ignore it
{space}			;ignore it

{atom}			(cons 'atom yytext)
{character}		(cons 'character (string-ref yytext 0))

{open-comment}		(begin 'comment)
{open-quoted-text}	(begin 'quoted-text)
{open-domain-literal}	(begin 'domain-literal)

;; {domain-literal}	(cons 'domain-literal
;; 			      ;;Strip the square brackets.
;; 			      (let ((len (string-length yytext)))
;; 				(substring yytext 1 (- len 1))))

<<EOF>>			(begin #f)

<<ERROR>>		(assertion-violation #f
			  (string-append "invalid token in email address text at line "
                                         (number->string yyline)
					 " column "
					 (number->string yycolumn)
					 " offset "
					 (number->string yyoffset)))

