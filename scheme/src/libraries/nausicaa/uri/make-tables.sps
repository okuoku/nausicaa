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


#!r6rs
(import (rnrs)
  (nausicaa silex)
  (prefix (nausicaa lalr) lalr.))


(lex (input-file:	"generic-lexer.l")
     (output-file:	"generic-lexer.sls")
     (library-spec:	'(nausicaa uri generic-lexer))
     (library-imports:	'((nausicaa silex default-error-handler)
			  (nausicaa parser-tools lexical-token)
			  (nausicaa parser-tools source-location)))
     (table-name:	'uri-generic-lexer-table)
     (counters:		'all))


(lalr.lalr-parser

 (lalr.output-file:		"generic-parser.sls")
 (lalr.parser-name:		'make-uri-generic-parser)
 (lalr.library-spec:		'(nausicaa uri generic-parser))

 (lalr.terminals:		'(COLON SLASH QUESTION NUMBER-SIGN COMPONENT))

 (lalr.rules:
  '((uri

;;;      scheme                   authority       path              query              fragment
;;;          $1    $2    $3    $4        $5    $6   $7       $8        $9         $10       $11
     (COMPONENT COLON SLASH SLASH COMPONENT SLASH path QUESTION COMPONENT NUMBER-SIGN COMPONENT)
     : (values $1 $5 $7 $8 $10)

;;;      scheme                   authority       path              query
;;;          $1    $2    $3    $4        $5    $6   $7       $8        $9
     (COMPONENT COLON SLASH SLASH COMPONENT SLASH path QUESTION COMPONENT)
     : (values $1 $5 $7 $8 #f)

;;;      scheme                   authority       path              fragment
;;;          $1    $2    $3    $4        $5    $6   $7          $8        $9
     (COMPONENT COLON SLASH SLASH COMPONENT SLASH path NUMBER-SIGN COMPONENT)
     : (values $1 $5 $7 #f $9)

;;;      scheme                   authority       path
;;;          $1    $2    $3    $4        $5    $6   $7
     (COMPONENT COLON SLASH SLASH COMPONENT SLASH path)
     : (values $1 $5 $7 #f #f)

;;;      scheme                   authority
;;;          $1    $2    $3    $4        $5    $6
     (COMPONENT COLON SLASH SLASH COMPONENT SLASH)
     : (values $1 $5 #f #f #f)

;;;      scheme                   authority
;;;          $1    $2    $3    $4        $5
     (COMPONENT COLON SLASH SLASH COMPONENT)
     : (values $1 $5 #f #f #f)


     )

    (path		(COMPONENT SLASH path)	: (cons $1 $3)
			(COMPONENT SLASH)	: (list $1)
			(COMPONENT)		: (list $1))
    )))


#;(lalr-parser

 (output-file:		"generic-parser.sls")
 (parser-name:		'make-uri-generic-parser)
 (library-spec:		'(nausicaa uri generic-parser))

 (terminals:		'(AT COLON QUESTION SHARP SLASH DOUBLE_SLASH OPEN_BRACKET CLOSE_BRACKET
			     SCHEME_STRING USERINFO_STRING IPVFUTURE_STRING))

 (rules:
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
