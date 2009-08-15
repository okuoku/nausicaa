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
;;;Copyright (c) 2007, 2009 Marco Maggi <marcomaggi@gna.org>
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
subsequent      {initial}|[0-9\.@!$&:<=>?~\-]
symbol          {initial}{subsequent}*

operator	(<=|>=|//)|[\+\-*/%\^<>=]

comma		,

oparen		\(
cparen		\)

%%
{blanks}	;; skip blanks, tabs and newlines
{imag}		(make-lexical-token 'NUM
				    (make-source-location #f
							  yyline yycolumn yyoffset
							  (string-length yytext))
				    (string->number (string-append "+" yytext)))
{real}		(make-lexical-token 'NUM
				    (make-source-location #f
							  yyline yycolumn yyoffset
							  (string-length yytext))
				    (string->number yytext))
{nan}		(make-lexical-token 'NUM
				    (make-source-location #f yyline yycolumn yyoffset
							  (string-length yytext))
				    +nan.0)
{pinf}		(make-lexical-token 'NUM
				    (make-source-location #f yyline yycolumn yyoffset
							  (string-length yytext))
				    +inf.0)
{minf}		(make-lexical-token 'NUM
				    (make-source-location #f yyline yycolumn yyoffset
							  (string-length yytext))
				    -inf.0)

{operator}	(let ((position (make-source-location #f yyline yycolumn yyoffset
						      (string-length yytext)))
                      (symbol	(string->symbol yytext)))
                  ;;These must be different categories to let us specify
                  ;;their precedence in the grammar.
		  (case symbol
		    ((+)	(make-lexical-token 'ADD	position '+))
		    ((-)	(make-lexical-token 'SUB	position '-))
		    ((*)	(make-lexical-token 'MUL	position '*))
		    ((/)	(make-lexical-token 'DIV	position '/))
		    ((%)	(make-lexical-token 'MOD	position 'mod))
		    ((^)	(make-lexical-token 'EXPT	position 'expt))
		    ((//)	(make-lexical-token 'DIV0	position 'div))
		    ((<)	(make-lexical-token 'LT		position '<))
		    ((>)	(make-lexical-token 'GT		position '>))
		    ((<=)	(make-lexical-token 'LE		position '<=))
		    ((>=)	(make-lexical-token 'GE		position '>=))
		    ((=)	(make-lexical-token 'EQ		position '=))))

{symbol}	(make-lexical-token 'ID
				    (make-source-location #f yyline yycolumn yyoffset
							  (string-length yytext))
				    (string->symbol yytext))

{comma}		(make-lexical-token
		 'COMMA
		 (make-source-location #f yyline yycolumn yyoffset (string-length yytext))
		 'COMMA)

{oparen}	(make-lexical-token
		 'LPAREN
		 (make-source-location #f yyline yycolumn yyoffset (string-length yytext))
		 'LPAREN)

{cparen}	(make-lexical-token
		 'RPAREN
		 (make-source-location #f yyline yycolumn yyoffset (string-length yytext))
		 'RPAREN)

<<EOF>>		(make-lexical-token
		 '*eoi*
		 (make-source-location #f yyline yycolumn yyoffset 0)
		 (eof-object))

<<ERROR>>	(assertion-violation #f
                  "invalid lexer token")

;;; end of file
