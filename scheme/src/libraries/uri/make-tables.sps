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


(lalr-parser

 (:output-file		"generic-parser.sls")
 (:parser-name		'make-uri-generic-parser)
 (:library-spec		'(uri generic-parser))

 (:terminals		'(AT COLON QUESTION SHARP SLASH DOUBLE_SLASH OPEN_BRACKET CLOSE_BRACKET
			     SCHEME_STRING USERINFO_STRING IPVFUTURE_STRING))

 (:rules
  '((uri
     (scheme COLON hier-part)		: #f
     (scheme COLON hier-part uri-tail)	: #f)

    (uri-tail
     (QUESTION query)			: #f
     (SHARP fragment)			: #f
     (QUESTION query SHARP fragment)	: #f)

    (scheme
     (SCHEME_STRING)			: #f)

    (hier-part
     (DOUBLE_SLASH authority path-abempty)	: #f
     (path-absolute)				: #f
     (path-rootless)				: #f
     (path-empty)				: #f)

    (authority
     (host)				: #f
     (host COLON port)			: #f
     (userinfo AT host COLON port)	: #f
     (userinfo AT host)			: #f)

    (host
     (IP-literal)			: #f
     (IPv4address)			: #f
     (reg-name)				: #f)

    (IP-literal
     (OPEN_BRACKET IPv6address CLOSE_BRACKET)	: #f
     (OPEN_BRACKET IPVFUTURE_STRING   CLOSE_BRACKET)	: #f)


    (userinfo
     (USERINFO_STRING)			: $1)

    (query		()					: #f)
    (fragment		()					: #f)

    (string		(USERINFO_STRING)			: $1
			(SCHEME_STRING)				: $1)

    )))

;;; end of file
