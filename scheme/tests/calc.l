;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: calculator lexer specification for SILex
;;;Date: Tue Jul  3, 2007
;;;
;;;Abstract
;;;
;;;	This is a  test lexer specification for a  calculator.  It seems
;;;	to  work  well.  To  produce  it we  can  process  it using  the
;;;	following program:
;;;
;;;		(import (rnrs) (silex))
;;;		(lex "calc-test.l" "calc-test.l.sls")
;;;
;;;	this will produce a new file "calc-test.l.sls" that we can embed
;;;	directly in the source code of a program.  The following:
;;;
;;;		(import (rnrs) (silex))
;;;		(lex "calc-test.l" "calc-test.l.sls"
;;;		     'library-spec "(calc-test)")
;;;
;;;	will  produce a  new file  "calc-test.l.sls" holding  a properly
;;;	defined R6RS library to be loaded into a program.
;;;
;;;Notes
;;;
;;; 	The definition of the  "symbol" regular expression must not hold
;;; 	any of the  chars in [+-*\/^%,] or it  will result in tokenising
;;; 	errors when splitting "1*2", "1/2", "a-b" and such.
;;;
;;;Copyright (C) 2007, 2009 Marco Maggi <marcomaggi@gna.org>
;;;Copyright (C) 2000 The Free Software Foundation
;;;
;;;Originally from Guile-Arith was written by Ian Grant <iang@pobox.com>
;;;This file was modified by Marco Maggi <marcomaggi@gna.org>
;;;
;;;This program is free software: you can redistribute it and/or modify
;;;it under the terms of the GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is distributed in the hope that it will be useful, but
;;;WITHOUT ANY WARRANTY; without even the implied warranty of
;;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;General Public License for more details.
;;;
;;;You should have received a copy of the GNU General Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

blanks		[ \10\13]+

decint          [0-9]+
binint          #[bB][01]+
octint          #[oO][0-7]+
hexint          #[xX][0-9A-Fa-f]+
integer		{decint}|{binint}|{octint}|{hexint}

exponent        ([eE][+\-]?[0-9]+)
truereal	[0-9]+\.|[0-9]*\.[0-9]+{exponent}?|[0-9]+{exponent}
real		{truereal}|{integer}

imag		({decint}|{real})i

nan             \-nan\.0|\+nan\.0|nan\.0
inf             \-inf\.0|\+inf\.0|inf\.0

initial         [a-zA-Z!$&:<=>?_~]
subsequent      {initial}|[0-9.@]
symbol          {initial}{subsequent}*

cmpoperator	(<=|>=)
operator	[\+\-*/%\^\\<>=]

comma		,

oparen		\(
cparen		\)

%%
{blanks}	;; skip blanks, tabs and newlines
{imag}		(string->number (string-append "+" yytext))
{real}		(string->number yytext)
{nan}		(string->number yytext)
{inf}		(string->number yytext)
{operator}	(case (string-ref yytext 0)
		  ((#\+) +)
		  ((#\-) -)
		  ((#\*) *)
		  ((#\/) /)
		  ((#\%) mod)
		  ((#\^) expt)
		  ((#\\) div)
		  ((#\=) =)
		  ((#\<) <)
		  ((#\>) >))
{cmpoperator}	(cond
                  ((string=? yytext "<=") <=)
                  ((string=? yytext ">=") >=))
{symbol}	(string->symbol yytext)
{comma}		(begin cons)

{oparen}	(begin #\()
{cparen}	(begin #\))

<<EOF>>		(begin #f)
<<ERROR>>	(assertion-violation #f
                  "invalid lexer token" yytext)

