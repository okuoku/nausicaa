;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: infix notation
;;;Date: Sat Aug 15, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;
;;;This  program  is free  software:  you  can redistribute  it
;;;and/or modify it  under the terms of the  GNU General Public
;;;License as published by the Free Software Foundation, either
;;;version  3 of  the License,  or (at  your option)  any later
;;;version.
;;;
;;;This  program is  distributed in  the hope  that it  will be
;;;useful, but  WITHOUT ANY WARRANTY; without  even the implied
;;;warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
;;;PURPOSE.   See  the  GNU  General Public  License  for  more
;;;details.
;;;
;;;You should  have received a  copy of the GNU  General Public
;;;License   along   with    this   program.    If   not,   see
;;;<http://www.gnu.org/licenses/>.
;;;



(library (infix)
  (export infix-string->sexpr)
  (import (rnrs)
    (silex lexer)
    (infix string-lexer)
    (infix string-parser))


(define (infix-string->sexpr string)
  (let* ((IS		(lexer-make-IS :string string :counters 'all))
	 (lexer		(lexer-make-lexer infix-string-lexer-table IS))
	 (parser	(make-infix-string-parser))
	 (error-handler	(lambda (message token)
			  (error #f
			    (if (not (lexical-token? token))
				message
			      (let* ((position	(lexical-token-source token))
				     (line	(source-location-line position))
				     (column	(source-location-column position)))
				(string-append
				 message
				 " line " (if line (number->string line) "unknown")
				 " column " (if column (number->string column) "unknown"))))
			    token))))
    (parser lexer error-handler #f)))


;;;; done

)

;;; end of file
