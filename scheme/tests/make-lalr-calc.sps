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
  (prefix (silex) silex:))


;;;; lexer

(silex:lex silex::output-file "calc-parser-lexer.sls"
	   silex::counters 'all
	   silex::library-spec "(calc-parser-lexer)"
	   silex::library-imports '((lalr common))
	   silex::table-name 'calc-parser-lexer-table
	   silex::input-string "
blanks		[ \\9]+
newline		[\\10\\13]+

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

initial         [a-zA-Z_]
subsequent      {initial}|[0-9.@]
symbol          {initial}{subsequent}*

cmpoperator	(<=|>=|==)
operator	[\\+\\-*/%\\^\\\\<>]
assign		=

comma		,

oparen		\\(
cparen		\\)

%%
{blanks}	;; skip spaced and tabs
{imag}		(make-lexical-token 'NUM
				    (make-source-location \"<input>\" yyline yycolumn yyoffset -1)
				    (string->number (string-append \"+\" yytext)))
{real}		(make-lexical-token 'NUM
				    (make-source-location \"<input>\" yyline yycolumn yyoffset -1)
				    (string->number yytext))
{nan}		(make-lexical-token 'NUM
				    (make-source-location \"<input>\" yyline yycolumn yyoffset -1)
				    +nan.0)
{inf}		(make-lexical-token 'NUM
				    (make-source-location \"<input>\" yyline yycolumn yyoffset -1)
				    +inf.0)
{operator}	(let ((position (make-source-location \"<input>\" yyline yycolumn yyoffset -1)))
		  (case (string-ref yytext 0)
		    ((#\\+)	'+)
		    ((#\\-)	'-)
		    ((#\\*)	'*)
		    ((#\\/)	'/)
		    ((#\\%)	(make-lexical-token 'FUN position mod))
		    ((#\\^)	(make-lexical-token 'FUN position expt))
		    ((#\\\\)	(make-lexical-token 'FUN position div))
		    ((#\\<)	(make-lexical-token 'FUN position <))
		    ((#\\>)	(make-lexical-token 'FUN position >))))
{cmpoperator}	(let ((position (make-source-location \"<input>\" yyline yycolumn yyoffset -1)))
		  (cond
		   ((string=? yytext \"==\") (make-lexical-token 'FUN position =))
		   ((string=? yytext \"<=\") (make-lexical-token 'FUN position <=))
		   ((string=? yytext \">=\") (make-lexical-token 'FUN position >=))))
{symbol}	(make-lexical-token 'ID
				    (make-source-location \"<input>\" yyline yycolumn yyoffset -1)
				    (string->symbol yytext))
{assign}	(begin 'ASSIGN)

{comma}		(begin 'COMMA)
{newline}	(begin 'NEWLINE)
{oparen}	(begin 'LPAREN)
{cparen}	(begin 'RPAREN)

<<EOF>>		(begin '*eoi*)
<<ERROR>>	(assertion-violation #f \"invalid lexer token\")
")


;;;; parser

(lalr-parser

 :output-file		"calc-parser.sls"
		;output a parser, called calc-parser, in a separate file

 :parser-name		'make-calc-parser
 :library-spec		'(calc-parser)
 :library-imports	'((calc-parser-helper) (rnrs eval))

 :dump-table		"calc-parser-tables.txt"
		;output to a file the human readable LALR table

;;; :expect		5
		;there should be no conflicts

 :tokens	'(ID NUM ASSIGN LPAREN RPAREN NEWLINE COMMA
		  (left: + -)
		  (left: * /)
		  (nonassoc: uminus))

 :rules	'((lines
	   (lines line)		: (let ((result $2))
				    (when result
				      (evaluated-expressions
				       (cons result (evaluated-expressions)))))
		;this reports the result of all the lines but the first
	   (line)		: (let ((result $1))
				    (when result
				      (evaluated-expressions
				       (cons result (evaluated-expressions))))))
		;this reports the result of the first line only

	  (line     (assign NEWLINE)	: $1
		    (expr   NEWLINE)	: $1
		    (error  NEWLINE)	: #f)

	  (assign   (ID ASSIGN expr)	: (begin
					    (hashtable-set! (table-of-variables) $1 $3)
					    #f))

	  (expr     (expr + expr)	: (+ $1 $3)
		    (expr - expr)	: (- $1 $3)
		    (expr * expr)	: (* $1 $3)
		    (expr / expr)	: (/ $1 $3)
		    (- expr (prec: uminus))
					: (- $2)
		    (ID)		: (hashtable-ref (table-of-variables) $1 #f)
		    (ID LPAREN args RPAREN)
					: (apply (eval $1 (environment '(rnrs))) $3)
		    (NUM)		: $1
		    (LPAREN expr RPAREN)
					: $2)

	  (args     ()			: '()
		    (expr arg-rest)	: (cons $1 $2))

	  (arg-rest (COMMA expr arg-rest)
					: (cons $2 $3)
		    ()			: '())))

;;; end of file
