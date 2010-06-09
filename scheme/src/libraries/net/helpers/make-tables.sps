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


(import (rnrs)
  (silex)
  (lalr))


(lex (:input-file	"ipv6-address-lexer.l")
     (:output-file	"ipv6-address-lexer.sls")
     (:library-spec	'(net helpers ipv6-address-lexer))
     (:library-imports	'((silex default-error-handler)
			  (parser-tools lexical-token)
			  (parser-tools source-location)))
     (:table-name	'ipv6-address-lexer-table)
     (:counters		'all))


(lalr-parser

 (:output-file		"ipv6-address-parser.sls")
 (:parser-name		'make-ipv6-address-parser)
 (:library-spec		'(net helpers ipv6-address-parser))

 (:terminals		'(COLON HEXINT))

 (:rules
  '((ipv6-address

;;;        8            7            6            5            4            3            2            1
     (HEXINT COLON HEXINT COLON HEXINT COLON HEXINT COLON HEXINT COLON HEXINT COLON HEXINT COLON HEXINT)
				: #f)

    )))

;;; end of file
