;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: infix string lexer specification for (silex)
;;;Date: Sat Aug 15, 2009
;;;
;;;Abstract
;;;
;;;
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


(import (nausicaa)
  (prefix (silex) sil:)
  (lalr))

(sil:lex sil::input-file	"string-lexer.l"
	 sil::output-file	"string-lexer.sls"
	 sil::library-spec	"(infix string-lexer)"
	 sil::library-imports	'((lalr lr-driver)
				  (parser-tools lexical-token)
				  (parser-tools source-location))
	 sil::table-name	'infix-string-lexer-table
	 sil::lexer-format	'code
	 sil::counters		'all)

(lalr-parser

 :output-file		"string-parser.sls"
 :parser-name		'make-infix-string-parser
 :library-spec		'(infix string-parser)
;;; :library-imports	'((rnrs eval))

 :terminals	'(ID NUM LPAREN RPAREN COMMA
		     (left: ADD SUB)
		     (left: MUL DIV DIV0)
		     (left: MOD)
		     (left: EXPT)
		     (left: LT GT LE GE EQ)
		     (nonassoc: UADD)
		     (nonassoc: USUB))

 :rules	'((expr		(expr ADD expr)		: (list $2 $1 $3)
			(expr SUB expr)		: (list $2 $1 $3)
			(expr DIV expr)		: (list $2 $1 $3)
			(expr MUL expr)		: (list $2 $1 $3)

			(expr DIV0 expr)		: (list $2 $1 $3)
			(expr MOD expr)		: (list $2 $1 $3)
			(expr EXPT expr)		: (list $2 $1 $3)
			(expr LT expr)		: (list $2 $1 $3)
			(expr GT expr)		: (list $2 $1 $3)
			(expr LE expr)		: (list $2 $1 $3)
			(expr GE expr)		: (list $2 $1 $3)
			(expr EQ expr)		: (list $2 $1 $3)
			(ADD expr (prec: UADD))	: $2
			(SUB expr (prec: USUB))	: (- $2)
			(ID)			: $1
			(ID LPAREN args RPAREN)	: (cons $1 $3)
			(NUM)			: $1
			(LPAREN expr RPAREN)	: $2)

	  (args		()			: '()
			(expr arg-rest)		: (cons $1 $2))

	  (arg-rest	(COMMA expr arg-rest)	: (cons $2 $3)
			()			: '())))

(lalr-parser

 :output-file		"sexp-parser.sls"
 :parser-name		'make-infix-sexp-parser
 :library-spec		'(infix sexp-parser)
;;; :library-imports	'((rnrs eval))

 :terminals	'(ID NUM LPAREN RPAREN
		     (left: ADD SUB)
		     (left: MUL DIV DIV0)
		     (left: MOD)
		     (left: EXPT)
		     (left: LT GT LE GE EQ)
		     (nonassoc: UADD)
		     (nonassoc: USUB))

 :rules	'((expr		(expr ADD expr)		: (list $2 $1 $3)
			(expr SUB expr)		: (list $2 $1 $3)
			(expr DIV expr)		: (list $2 $1 $3)
			(expr MUL expr)		: (list $2 $1 $3)

			(expr DIV0 expr)		: (list $2 $1 $3)
			(expr MOD expr)		: (list $2 $1 $3)
			(expr EXPT expr)		: (list $2 $1 $3)
			(expr LT expr)		: (list $2 $1 $3)
			(expr GT expr)		: (list $2 $1 $3)
			(expr LE expr)		: (list $2 $1 $3)
			(expr GE expr)		: (list $2 $1 $3)
			(expr EQ expr)		: (list $2 $1 $3)
			(ADD expr (prec: UADD))	: $2
			(SUB expr (prec: USUB))	: (- $2)
			(ID)			: $1
			(ID LPAREN args RPAREN)	: (cons $1 $3)
			(NUM)			: $1
			(LPAREN expr RPAREN)	: $2)

	  (args		()			: '()
			(expr arg-rest)		: (cons $1 $2))

	  (arg-rest	(expr arg-rest)		: (cons $1 $2)
			()			: '())))

;;; end of file
