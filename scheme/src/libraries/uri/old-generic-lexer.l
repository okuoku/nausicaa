;;; -*- mode: scheme; coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexer for generic URI grammar
;;;Date: Wed Jun  2, 2010
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

ALPHA			[a-zA-Z]
DIGIT			[0-9]
HEXDIG			[0-9a-fA-F]
LF			\10
CR			\13
SP			\32
DQUOTE			\"

AT			@
COLON			:
QUESTION		\?
SHARP			#
SLASH			/
DOUBLE_SLASH		{SLASH}{SLASH}
OPEN_BRACKET		\[
CLOSE_BRACKET		\]

pct-encoded		%{HEXDIG}{HEXDIG}

gen-delims		[{COLON}{QUESTION}{SHARP}{SLASH}\[\]@]
sub-delims		[\!\$&\'()\*\+,\;=]

reserved		{gen-delims}|{sub-delims}
unreserved		{ALPHA}|{DIGIT}|[\-\._~]

SCHEME_STRING		{ALPHA}({ALPHA}|{DIGIT}|[+\-/.])*
USERINFO_STRING		({unreserved}|{pct-encoded}|{sub-delims}|:)*
IPvFuture		[vV]{HEXDIG}\.({unreserved}|{sub-delims}|:)*

%%

{AT}			(make-<lexical-token> 'AT
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\@ 1)

{COLON}			(make-<lexical-token> 'COLON
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\: 1)

{QUESTION}		(make-<lexical-token> 'QUESTION
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\? 1)

{SHARP}			(make-<lexical-token> 'SHARP
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\# 1)

{SLASH}			(make-<lexical-token> 'SLASH
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\/ 1)

{DOUBLE_SLASH}		(make-<lexical-token> 'DOUBLE_SLASH
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 "//" 2)

{OPEN_BRACKET}		(make-<lexical-token> 'OPEN_BRACKE
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\] 1)

{CLOSE_BRACKET}		(make-<lexical-token> 'CLOSE_BRACKET
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 #\[ 1)


{SCHEME_STRING}		(make-<lexical-token> 'SCHEME_STRING
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 yytext (string-length yytext))

{USERINFO_STRING}	(make-<lexical-token> 'USERINFO_STRING
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 yytext (string-length yytext))

{IPvFuture}		(make-<lexical-token> 'IPVFUTURE_STRING
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 yytext (string-length yytext))

;; {NUMBER}		(make-<lexical-token> 'NUMBER
;; 			 (make-<source-location> #f yyline yycolumn yyoffset)
;; 			 (string->number yytext) (string-length yytext))


<<EOF>>			(make-<lexical-token> '*eoi*
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (eof-object) 0)

<<ERROR>>		(silex-default-error-handler)

;;; end of file
