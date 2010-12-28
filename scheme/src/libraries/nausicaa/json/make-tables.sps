;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: rebuild the json lexer and parser tables
;;;Date: Sun May 30, 2010
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


#!r6rs
(import (rnrs)
  (nausicaa silex)
  (prefix (nausicaa lalr) lalr.))


(lex (input-file:	"string-lexer.l")
     (output-file:	"string-lexer.sls")
     (library-spec:	'(nausicaa json string-lexer))
     (library-imports:	'((nausicaa silex default-error-handler)
			  (nausicaa parser-tools lexical-token)
			  (nausicaa parser-tools source-location)))
     (table-name:	'json-string-lexer-table)
     (counters:		'all))

(lex (input-file:	"rfc-lexer.l")
     (output-file:	"rfc-lexer.sls")
     (library-spec:	'(nausicaa json rfc-lexer))
     (library-imports:	'((nausicaa silex default-error-handler)
			  (nausicaa parser-tools lexical-token)
			  (nausicaa parser-tools source-location)))
     (table-name:	'json-rfc-lexer-table)
     (counters:		'all))

(lex (input-file:	"extended-lexer.l")
     (output-file:	"extended-lexer.sls")
     (library-spec:	'(nausicaa json extended-lexer))
     (library-imports:	'((nausicaa silex default-error-handler)
			  (nausicaa parser-tools lexical-token)
			  (nausicaa parser-tools source-location)))
     (table-name:	'json-extended-lexer-table)
     (counters:		'all))


(lalr.lalr-parser

 (lalr.output-file:		"sexp-parser.sls")
 (lalr.parser-name:		'make-json-sexp-parser)
 (lalr.library-spec:		'(nausicaa json sexp-parser))

 (lalr.terminals:		'(BEGIN_ARRAY END_ARRAY
					      BEGIN_OBJECT END_OBJECT
					      NAME_SEPARATOR VALUE_SEPARATOR
					      FALSE TRUE NULL
					      NUMBER STRING))

 (lalr.rules:
  '((json-text
     (object)				: $1
     (array)				: $1)

    (object
     (BEGIN_OBJECT END_OBJECT)		: '()
     (BEGIN_OBJECT pair END_OBJECT)	: (list $2)
     (BEGIN_OBJECT pair pair-rest END_OBJECT)
					: (cons $2 $3))

    (pair
     (STRING NAME_SEPARATOR value)	: (cons $1 $3))
    (pair-rest
     (VALUE_SEPARATOR pair)		: (list $2)
     (VALUE_SEPARATOR pair pair-rest)
					: (cons $2 $3))


    (array
     (BEGIN_ARRAY END_ARRAY)		: '#()
     (BEGIN_ARRAY value END_ARRAY)	: (vector $2)
     (BEGIN_ARRAY value value-rest END_ARRAY)
					: (list->vector (cons $2 $3)))

    (value
     (FALSE)				: $1
     (NULL)				: $1
     (TRUE)				: $1
     (NUMBER)				: $1
     (STRING)				: $1
     (object)				: $1
     (array)				: $1)
    (value-rest
     (VALUE_SEPARATOR value)		: (list $2)
     (VALUE_SEPARATOR value value-rest)	: (cons $2 $3))

    )))

;;; end of file
