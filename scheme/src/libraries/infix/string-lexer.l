;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: calculator lexer
;;;Date: Sat Aug 15, 2009
;;;
;;;Abstract
;;;
;;;	The lexer table is derived  from the lexer in Guile-Arith by Ian
;;;	Grant <iang@pobox.com>.
;;;
;;;Copyright (c) 2007, 2009-2010 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2000 The Free Software Foundation
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

blanks		[ \9\10\13]+

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
pinf            \+inf\.0|inf\.0
minf            \-inf\.0

initial         [a-zA-Z_]
subsequent      {initial}|[0-9\.@!$&:<=>\?~\-]
symbol          {initial}{subsequent}*

operator	(<=|>=|//)|[\+\-*/%\^<>=]

question	\?
colon		:
comma		,

oparen		\(
cparen		\)

%%
{blanks}	;; skip blanks, tabs and newlines
{imag}		(make-<lexical-token> 'NUM
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      (string->number (string-append "+" yytext))
				      (string-length yytext))
{real}		(make-<lexical-token> 'NUM
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      (string->number yytext)
				      (string-length yytext))
{nan}		(make-<lexical-token> 'NUM
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      +nan.0
				      (string-length yytext))
{pinf}		(make-<lexical-token> 'NUM
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      +inf.0
				      (string-length yytext))
{minf}		(make-<lexical-token> 'NUM
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      -inf.0
				      (string-length yytext))

{operator}	(let ((position (make-<source-location> #f yyline yycolumn yyoffset))
		      (len	(string-length yytext))
                      (symbol	(string->symbol yytext)))
                  ;;These must be different categories to let us specify
                  ;;their precedence in the grammar.
		  (case symbol
		    ((+)	(make-<lexical-token> 'ADD	position '+ len))
		    ((-)	(make-<lexical-token> 'SUB	position '- len))
		    ((*)	(make-<lexical-token> 'MUL	position '* len))
		    ((/)	(make-<lexical-token> 'DIV	position '/ len))
		    ((%)	(make-<lexical-token> 'MOD	position 'mod len))
		    ((^)	(make-<lexical-token> 'EXPT	position 'expt len))
		    ((//)	(make-<lexical-token> 'DIV0	position 'div len))
		    ((<)	(make-<lexical-token> 'LT	position '< len))
		    ((>)	(make-<lexical-token> 'GT	position '> len))
		    ((<=)	(make-<lexical-token> 'LE	position '<= len))
		    ((>=)	(make-<lexical-token> 'GE	position '>= len))
		    ((=)	(make-<lexical-token> 'EQ	position '= len))))

{symbol}	(make-<lexical-token> 'ID
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      (string->symbol yytext)
				      (string-length yytext))

{question}	(make-<lexical-token> 'QUESTION-ID
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      '?
				      (string-length yytext))

{colon}		(make-<lexical-token> 'COLON-ID
				      (make-<source-location> #f yyline yycolumn yyoffset)
				      ':
				      (string-length yytext))

{comma}		(make-<lexical-token>
		 'COMMA
		 (make-<source-location> #f yyline yycolumn yyoffset)
		 'COMMA
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

<<ERROR>>	(assertion-violation #f
                  "invalid lexer token")

;;; end of file
