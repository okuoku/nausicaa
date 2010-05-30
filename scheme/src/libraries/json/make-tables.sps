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


(import (rnrs)
  (keywords)
  (silex)
  (lalr))

(define-keywords
  :counters
  :dump-table
  :input-file
  :input-string
  :library-imports
  :library-spec
  :output-file
  :parser-name
  :rules
  :table-name
  :terminals
  )


(lex (:input-file	"string-lexer.l")
     (:output-file	"string-lexer.sls")
     (:library-spec	'(json string-lexer))
     (:library-imports	'((lalr lr-driver)
			  (parser-tools lexical-token)
			  (parser-tools source-location)))
     (:table-name	'json-string-lexer-table)
     (:counters		'all))

(lex (:input-file	"rfc-lexer.l")
     (:output-file	"rfc-lexer.sls")
     (:library-spec	'(json rfc-lexer))
     (:library-imports	'((lalr lr-driver)
			  (parser-tools lexical-token)
			  (parser-tools source-location)))
     (:table-name	'json-rfc-lexer-table)
     (:counters		'all))


(lalr-parser

 :output-file		"parser.sls"
 :parser-name		'make-json-parser
 :library-spec		'(json parser)
;; :library-imports	'()

 :terminals	'(BEGIN_ARRAY END_ARRAY
		  BEGIN_OBJECT END_OBJECT
		  NAME_SEPARATOR VALUE_SEPARATOR
		  FALSE TRUE NULL
		  NUMBER STRING)

 :rules
 '((json-text		(object)					: $1
			(array)						: $1)

   (object		(BEGIN_OBJECT END_OBJECT)			: '()
			(BEGIN_OBJECT member END_OBJECT)		: $2
			(BEGIN_OBJECT member member-rest END_OBJECT)	: (cons $2 $3))
   (member		(STRING NAME_SEPARATOR value)			: (cons $1 $3))
   (member-rest		(VALUE_SEPARATOR member)			: (list $2)
			(VALUE_SEPARATOR member member-rest)		: (cons $2 $3))


   (array		(BEGIN_ARRAY END_ARRAY)				: '#()
			(BEGIN_ARRAY value END_ARRAY)			: (vector $2)
			(BEGIN_ARRAY value value-rest END_ARRAY)	: (list->vector (cons $2 $3)))

   (value		(FALSE)						: $1
			(NULL)						: $1
			(TRUE)						: $1
			(NUMBER)					: $1
			(STRING)					: $1
			(object)					: $1
			(array)						: $1)
   (value-rest		(VALUE_SEPARATOR value)				: (list $2)
			(VALUE_SEPARATOR value value-rest)		: (cons $2 $3))

   ))

;;; end of file
