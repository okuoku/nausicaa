;;; -*- mode: fundamental; coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: string lexer for strings in json chunks
;;;Date: Sun May 30, 2010
;;;
;;;Abstract
;;;
;;;	This file  defines a lexer table  to be used  while tokenising a
;;;	string in an  RFC 4627 JSON text.
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

hexdigit		[0-9a-fA-F]

escaped-quote		\\\"
escaped-backslash	\\\\
escaped-slash		\\/
escaped-backspace	\\b
escaped-formfeed	\\f
escaped-linefeed	\\n
escaped-return		\\r
escaped-tab		\\t
escaped-unicode		\\u{hexdigit}{hexdigit}{hexdigit}{hexdigit}

double-quote		\"

text			[^\"\\]+

%%

{double-quote}		'QUOTED-TEXT-CLOSE

{escaped-quote}		"\""
{escaped-backslash}	"\\"
{escaped-slash}		"/"
{escaped-backspace}	"\b"
{escaped-formfeed}	"\f"
{escaped-linefeed}	"\n"
{escaped-return}	"\r"
{escaped-tab}		"\t"
{escaped-unicode}	(begin
                         (string (integer->char (string->number (substring yytext 2 6) 16))))

{text}			(begin yytext)

<<EOF>>			(make-<lexical-token> '*eoi*
			 (make-<source-location> #f yyline yycolumn yyoffset)
			 (eof-object) 0)

<<ERROR>>		(silex-default-error-handler)

;;; end of file
