;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: infix string lexer specification for (silex)
;;;Date: Sat Aug 15, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (nausicaa)
  (prefix (silex) lex.)
  (lalr))

(lex.lex (lex.input-file:	"string-lexer.l")
	 (lex.output-file:	"string-lexer.sls")
	 (lex.library-spec:	"(infix string-lexer)")
	 (lex.library-imports:	'((lalr lr-driver)
				  (parser-tools lexical-token)
				  (parser-tools source-location)))
	 (lex.table-name:	'infix-string-lexer-table)
	 (lex.lexer-format:	'code)
	 (lex.counters:		'all))

;;; --------------------------------------------------------------------

;;;There  are 2  parsers!!!   Unfortunately the  only  way to  implement
;;;correctly and with  beautiful infix syntax the string  parser and the
;;;sexp/macro parser is to have 2 separate parsers.

(lalr-parser

 (output-file:		"string-parser.sls")
 (parser-name:		'make-infix-string-parser)
 (library-spec:		'(infix string-parser))
;;; (library-imports:	'((rnrs eval)))

 (terminals:	'(ID QUESTION-ID COLON-ID NUM LPAREN RPAREN COMMA
		     (left: ADD SUB)
		     (left: MUL DIV DIV0)
		     (left: MOD)
		     (left: EXPT)
		     (left: LT GT LE GE EQ)
		     (nonassoc: UADD)
		     (nonassoc: USUB)))

 (rules: '((expr		(expr ADD expr)		: (list $2 $1 $3)
				(expr SUB expr)		: (list $2 $1 $3)
				(expr DIV expr)		: (list $2 $1 $3)
				(expr MUL expr)		: (list $2 $1 $3)

				(expr DIV0 expr)	: (list $2 $1 $3)
				(expr MOD expr)		: (list $2 $1 $3)
				(expr EXPT expr)	: (list $2 $1 $3)
				(expr LT expr)		: (list $2 $1 $3)
				(expr GT expr)		: (list $2 $1 $3)
				(expr LE expr)		: (list $2 $1 $3)
				(expr GE expr)		: (list $2 $1 $3)
				(expr EQ expr)		: (list $2 $1 $3)
				(ADD expr (prec: UADD))	: $2
				(SUB expr (prec: USUB))	: (list $1 $2)
				(ID)			: $1
				(ID LPAREN args RPAREN)	: (cons $1 $3)
				(expr QUESTION-ID expr COLON-ID expr) : (list 'if $1 $3 $5)
				(NUM)			: $1
				(LPAREN expr RPAREN)	: $2)

	   (args		()			: '()
				(expr arg-rest)		: (cons $1 $2))

	   (arg-rest	(COMMA expr arg-rest)	: (cons $2 $3)
			()			: '()))))

;;; --------------------------------------------------------------------

(lalr-parser

 (output-file:		"sexp-parser.sls")
 (parser-name:		'make-infix-sexp-parser)
 (library-spec:		'(infix sexp-parser))

 (terminals:	'(ID QUESTION-ID COLON-ID NUM LPAREN RPAREN
		     (left: ADD SUB)
		     (left: MUL DIV DIV0)
		     (left: MOD)
		     (left: EXPT)
		     (left: LT GT LE GE EQ)
		     (nonassoc: UADD)
		     (nonassoc: USUB)))

 (rules: '((expr	(expr ADD expr)		: (list $2 $1 $3)
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
			(SUB expr (prec: USUB))	: (list $1 $2)
			(ID)			: $1

			;;The  following are  two  versions of  function
			;;application.
			(ID LPAREN args RPAREN)	: (cons $1 $3)

			;;The following is the if-then-else operator.
			;;
			;;When parsing  a sexp the $2 is  the raw symbol
			;;IF.
			;;
			;;When  parsing  a   macro  input  form  the  $2
			;;(semantic  value of  QUESTION-ID) is  a syntax
			;;object  holding the  IF  binding from  (rnrs);
			;;this  is a  trick  to allow  insertion of  the
			;;correct binding in the output form.
			(expr QUESTION-ID expr COLON-ID expr) : (list $2 $1 $3 $5)

			(NUM)			: $1
			(LPAREN expr RPAREN)	: $2)

	   (args	()			: '()
			(expr arg-rest)		: (cons $1 $2))

	   (arg-rest	(expr arg-rest)		: (cons $1 $2)
			()			: '()))))

;;; end of file
