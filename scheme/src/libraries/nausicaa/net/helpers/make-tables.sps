;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: rebuild the lexer and parser tables for net libraries
;;;Date: Wed Jun  9, 2010
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
  (silex)
  (prefix (lalr) lalr.))


(lex (input-file:	"ipv6-address-lexer.l")
     (output-file:	"ipv6-address-lexer.sls")
     (library-spec:	'(net helpers ipv6-address-lexer))
     (library-imports:	'((silex default-error-handler)
			  (parser-tools lexical-token)
			  (parser-tools source-location)))
     (table-name:	'ipv6-address-lexer-table)
     (counters:		'all))

(lex (input-file:	"ipv4-address-lexer.l")
     (output-file:	"ipv4-address-lexer.sls")
     (library-spec:	'(net helpers ipv4-address-lexer))
     (library-imports:	'((silex default-error-handler)
			  (parser-tools lexical-token)
			  (parser-tools source-location)))
     (table-name:	'ipv4-address-lexer-table)
     (counters:		'all))


(lalr.lalr-parser

 (lalr.output-file:		"ipv4-address-parser.sls")
 (lalr.parser-name:		'make-ipv4-address-parser)
 (lalr.library-spec:		'(net helpers ipv4-address-parser))

 (lalr.terminals:		'(DOT NUMBER PREFIX-LENGTH))

 (lalr.rules:
  '((ipv4-address
     (NUMBER DOT NUMBER DOT NUMBER DOT NUMBER)
		: (list $1 $3 $5 $7)
     (NUMBER DOT NUMBER DOT NUMBER DOT NUMBER PREFIX-LENGTH)
		: (list $1 $3 $5 $7 (list $8)))
    )))


(lalr.lalr-parser

 (lalr.output-file:		"ipv6-address-parser.sls")
 (lalr.parser-name:		'make-ipv6-address-parser)
 (lalr.library-spec:		'(net helpers ipv6-address-parser))

 (lalr.terminals:		'(COLON DOT SLASH NUMBER))

 (lalr.rules:
  '((ipv6-address
     (NUMBER double-colon-tail)		: (cons (string->number $1 16) $2)
     (NUMBER tail)			: (cons (string->number $1 16) $2)
     (double-colon-tail)		: $1)

    (tail
     (COLON ipv4-address)		: $2
     (COLON NUMBER tail)		: (cons (string->number $2 16) $3)
     (COLON NUMBER double-colon-tail)	: (cons (string->number $2 16) $3)
     (prefix-length)			: $1
     ()					: '())

    (double-colon-tail
     (COLON COLON after-double-colon)	: (cons #f $3))

    (after-double-colon
     (NUMBER no-double-colon-tail)	: (cons (string->number $1 16) $2)
     (ipv4-address)			: $1
     (prefix-length)			: $1
     ()					: '())

    (no-double-colon-tail
     (COLON NUMBER no-double-colon-tail): (cons (string->number $2 16) $3)
     (COLON ipv4-address)		: $2
     (prefix-length)			: $1
     ()					: '())

    (prefix-length
     (SLASH NUMBER)			: `((,(string->number $2))))

    (ipv4-address
     (dot-couple DOT dot-couple ipv4-address-tail)
					: (cons* $1 $3 $4))

    (ipv4-address-tail
     (prefix-length)			: $1
     ()					: '())

    (dot-couple
     (NUMBER DOT NUMBER)		: (+ (bitwise-arithmetic-shift-left (string->number $1 10) 8)
					     (string->number $3 10)))
    )))

;;; end of file
