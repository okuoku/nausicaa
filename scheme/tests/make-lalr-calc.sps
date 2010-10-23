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
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (prefix (silex) lex.))


;;;; lexer

(lex.lex (lex.output-file: "calc-parser-lexer.sls")
	 (lex.counters: 'all)
	 (lex.library-spec: "(calc-parser-lexer)")
	 (lex.library-imports: '((parser-tools lexical-token)
				 (parser-tools source-location)))
	 (lex.table-name: 'calc-parser-lexer-table)
	 (lex.input-string: "
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
inf             \\inf\\.0

initial         [a-zA-Z_]
subsequent      {initial}|[0-9.@]
symbol          {initial}{subsequent}*

operator	(<=|>=|==|[\\+*/%\\^\\\\<>\\-])
assign		=

comma		,

oparen		\\(
cparen		\\)

%%
{blanks}	;; skip spaced and tabs
{imag}		(make-<lexical-token> 'NUM
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      (string->number (string-append \"+\" yytext))
				      (string-length yytext))

{real}		(make-<lexical-token> 'NUM
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      (string->number yytext)
				      (string-length yytext))

{nan}		(make-<lexical-token> 'NUM
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      +nan.0
				      (string-length yytext))

{inf}		(make-<lexical-token> 'NUM
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      +inf.0
				      (string-length yytext))

{operator}	(let ((position (make-<source-location> #f yyline yycolumn yyoffset))
		      (len	(string-length yytext)))
		  (case (string->symbol yytext)
		    ((+)	(make-<lexical-token> '+ position '+ len))
		    ((-)	(make-<lexical-token> '- position '- len))
		    ((*)	(make-<lexical-token> '* position '* len))
		    ((/)	(make-<lexical-token> '/ position '/ len))
		    ((%)	(make-<lexical-token> 'MOD position mod len))
		    ((^)	(make-<lexical-token> 'EXPT position expt len))
		    ((\\x5C;)	(make-<lexical-token> 'DIV position div len))
		    ((<)	(make-<lexical-token> 'LESS position < len))
		    ((>)	(make-<lexical-token> 'GREAT position > len))
		    ((<=)	(make-<lexical-token> 'LESSEQ position <= len))
		    ((>=)	(make-<lexical-token> 'GREATEQ position >= len))
		    ((==)	(make-<lexical-token> 'EQUAL position = len))
		    (else       (error #f \"unknown operator\" yytext len))))

{symbol}	(make-<lexical-token> 'ID
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      (string->symbol yytext)
				      (string-length yytext))

{assign}	(make-<lexical-token> 'ASSIGN
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      'ASSIGN
				      (string-length yytext))

{comma}		(make-<lexical-token>
		 'COMMA
		 (make-<source-location> #f yyline yycolumn yyoffset)
		 'COMMA
		 (string-length yytext))
{newline}	(make-<lexical-token>
		 'NEWLINE
		 (make-<source-location> #f yyline yycolumn yyoffset)
		 'NEWLINE
		  (string-length yytext))
{oparen}	(make-<lexical-token>
		 'LPAREN
		 (make-<source-location> #f yyline yycolumn yyoffset)
		 'LPAREN
		 (string-length yytext))
{cparen}	(make-<lexical-token>
		 'RPAREN
		 (make-<source-location> #f yyline yycolumn yyoffset)
		 'RPAREN
		 (string-length yytext))

<<EOF>>		(make-<lexical-token>
		 '*eoi*
		 (make-<source-location> #f yyline yycolumn yyoffset)
		 (eof-object)
		 0)
<<ERROR>>	(assertion-violation #f \"invalid lexer token\")
"))


;;;; parser

(lalr-parser

 (output-file:		"calc-parser.sls")
		;output a parser, called calc-parser, in a separate file

 (parser-name:		'make-calc-parser)
 (library-spec:		'(calc-parser))
 (library-imports:	'((calc-parser-helper) (rnrs eval)
			  (parser-tools lexical-token)
			  (parser-tools source-location)))

 (dump-table:		"calc-parser-tables.txt")
		;output to a file the human readable LALR table

;;; (expect:		5)
		;there should be no conflicts

 (terminals:	'(ID NUM ASSIGN LPAREN RPAREN NEWLINE COMMA
		  (left: + -)
		  (left: * / DIV MOD EXPT LESS GREAT LESSEQ GREATEQ EQUAL)
		  (nonassoc: uminus)
		  (nonassoc: uplus)))

 (rules:
  '((script	(lines)			: $1)

    (lines	(lines line)		: (let ((result $2))
					    (when result
					      (evaluated-expressions
					       (cons result (evaluated-expressions))))
					    result)
		;this reports the result of the last line
		(line)		: (let ((result $1))
				    (when result
				      (evaluated-expressions
				       (cons result (evaluated-expressions))))
				    result))
		;this reports the result of all the lines but the last

    (line	(assign NEWLINE)	: $1
		(expr   NEWLINE)	: $1

		(error  NEWLINE)	: #f)
		;either a line starts  with an expression or assignment,
		;or it  is an  error; in case  of error discard  all the
		;tokens up until the first newline

    (assign   (ID ASSIGN expr)	: (begin
				    (hashtable-set! (table-of-variables) $1 $3)
				    #f))

    (expr     (expr + expr)	: (+ $1 $3)
	      (expr - expr)	: (- $1 $3)
	      (expr * expr)	: (* $1 $3)
	      (expr / expr)	: (/ $1 $3)
	      (+ expr (prec: uplus))
	      : $2
	      (- expr (prec: uminus))
	      : (- $2)
	      (expr DIV expr)	: (div $1 $3)
	      (expr MOD expr)	: (mod $1 $3)
	      (expr EXPT expr)	: (expt $1 $3)
	      (expr LESS expr)	: (< $1 $3)
	      (expr GREAT expr)	: (> $1 $3)
	      (expr LESSEQ expr)	: (<= $1 $3)
	      (expr GREATEQ expr)	: (>= $1 $3)
	      (expr EQUAL expr)	: (= $1 $3)
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
	      ()			: '()))))

;;; end of file
