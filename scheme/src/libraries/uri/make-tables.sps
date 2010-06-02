;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: rebuild the URI lexer and parser tables
;;;Date: Wed Jun  2, 2010
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


(import (rnrs)
  (silex)
  (lalr))


(lex (:input-file	"generic-lexer.l")
     (:output-file	"generic-lexer.sls")
     (:library-spec	'(uri generic-lexer))
     (:library-imports	'((silex default-error-handler)
			  (parser-tools lexical-token)
			  (parser-tools source-location)))
     (:table-name	'uri-generic-lexer-table)
     (:counters		'all))


#;(lalr-parser

 (:output-file		"sexp-parser.sls")
 (:parser-name		'make-json-sexp-parser)
 (:library-spec		'(json sexp-parser))

 (:terminals		'(BEGIN_ARRAY END_ARRAY
				      BEGIN_OBJECT END_OBJECT
				      NAME_SEPARATOR VALUE_SEPARATOR
				      FALSE TRUE NULL
				      NUMBER STRING))

 (:rules
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
