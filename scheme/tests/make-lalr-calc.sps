;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for lalr
;;;Date: Thu Jul 16, 2009
;;;
;;;Abstract
;;;
;;;	Simple calculator in Scheme
;;;
;;;	  This  program  illustrates  the  use of  the  lalr-scm  parser
;;;	generator for Scheme. It is NOT robust, since calling a function
;;;	with the  wrong number of  arguments may generate an  error that
;;;	will cause the calculator to crash.
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;Copyright (c) 2004 Dominique Boucher
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


#!r6rs
(import (rnrs)
  (lalr)
  (silex))


;;;; lexer

(lex :output-file "calc-parser-lexer.sls"
     :lexer-format 'code
     :library-spec "(calc-parser-lexer)"
     :table-name 'calc-parser-lexer-table
     :input-string "
blanks		[ \\9\\10\\13]+

decint          [0-9]+
binint          #[bB][01]+
octint          #[oO][0-7]+
hexint          #[xX][0-9A-Fa-f]+
integer		{decint}|{binint}|{octint}|{hexint}

exponent        ([eE][+\\-]?[0-9]+)
truereal	[0-9]+\\.|[0-9]*\\.[0-9]+{exponent}?|[0-9]+{exponent}
real		{truereal}|{integer}

imag		({decint}|{real})i

nan             \\-nan\\.0|\\+nan\\.0|nan\\.0
inf             \\-inf\\.0|\\+inf\\.0|inf\\.0

initial         [a-zA-Z!$&:<=>?_~]
subsequent      {initial}|[0-9.@]
symbol          {initial}{subsequent}*

cmpoperator	(<=|>=)
operator	[\\+\\-*/%\\^\\\\<>=]

comma		,

oparen		\\(
cparen		\\)

%%
{blanks}	;; skip blanks, tabs and newlines
{imag}		(string->number (string-append \"+\" yytext))
{real}		(string->number yytext)
{nan}		(string->number yytext)
{inf}		(string->number yytext)
{operator}	(case (string-ref yytext 0)
		  ((#\\+) +)
		  ((#\\-) -)
		  ((#\\*) *)
		  ((#\\/) /)
		  ((#\\%) mod)
		  ((#\\^) expt)
		  ((#\\\\) div)
		  ((#\\=) =)
		  ((#\\<) <)
		  ((#\\>) >))
{cmpoperator}	(cond
                  ((string=? yytext \"<=\") <=)
                  ((string=? yytext \">=\") >=))
{symbol}	(string->symbol yytext)
{comma}		(begin cons)

{oparen}	(begin #\\()
{cparen}	(begin #\\))

<<EOF>>		(begin #f)
<<ERROR>>	(assertion-violation #f
                  \"invalid lexer token\")
")



;;;; parser

(lalr-parser

 :output-file		"calc-parser.sls"
		;output a parser, called calc-parser, in a separate file

 :table-name		'calc-parser
 :library-spec		'(calc-parser)
 :library-imports	'((calc-parser-helper))

 :output-table		"calc-parser-tables.txt"
		;output to a file the human readable LALR table

 :expect		5
		;there should be no conflicts

 :tokens	'(ID NUM = LPAREN RPAREN NEWLINE COMMA
		  (left: + -)
		  (left: * /)
		  (nonassoc: uminus))

 :rules	'((lines    (lines line)	: (write $2)
		    (line)		: (write $1))

	  (line     (assign NEWLINE)	: $1
		    (expr   NEWLINE)	: $1
		    (error  NEWLINE)	: #f)

	  (assign   (ID = expr)		: (hashtable-set! (table-of-variables) $1 $3))

	  (expr     (expr + expr)	: (+ $1 $3)
		    (expr - expr)	: (- $1 $3)
		    (expr * expr)	: (* $1 $3)
		    (expr / expr)	: (/ $1 $3)
		    (- expr (prec: uminus))
					: (- $2)
		    (ID)		: (hashtable-ref (table-of-variables) $1 #f)
		    (ID LPAREN args RPAREN)
					: (apply $1 $3)
		    (NUM)		: $1
		    (LPAREN expr RPAREN)
					: $2)

	  (args     ()			: '()
		    (expr arg-rest)	: (cons $1 $2))

	  (arg-rest (COMMA expr arg-rest)
					: (cons $2 $3)
		    ()			: '())))

;;; end of file
