;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexer and parser for JSON
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


(library (json)
  (export
    make-json-rfc-lexer make-json-parser
    json->tokens)
  (import (nausicaa)
    (silex lexer)
    (json string-lexer)
    (json rfc-lexer)
    (json parser)
    (parser-tools lexical-token)
    (parser-tools source-location))


(define (make-json-rfc-lexer IS)
  (let ((lexer (lexer-make-lexer json-rfc-lexer-table IS)))
    (lambda ()
      (let ((token (lexer)))
	(if (eq? 'QUOTED-TEXT-OPEN (<lexical-token>-category token))
	    (%lex-string IS token)
	  token)))))

(define (%lex-string IS opening-token)
  (let-values (((lexer)		(lexer-make-lexer json-string-lexer-table IS))
	       ((port getter)	(open-string-output-port)))
    (do ((token (lexer) (lexer)))
	((eq? token 'QUOTED-TEXT-CLOSE)
	 (let ((pos  (<lexical-token>-location opening-token))
	       (text (getter)))
	   (make-<lexical-token> 'STRING
				 (make-<source-location>
				  (<source-location>-input  pos)
				  (<source-location>-line   pos)
				  (<source-location>-column pos)
				  (<source-location>-offset pos))
				 text (string-length text))))
      (display token port))))

(define (json->tokens IS)
  (let ((lexer (make-json-rfc-lexer IS)))
    (let loop ((token		(lexer))
	       (list-of-tokens	'()))
      (if (<lexical-token>?/special token)
	  (reverse list-of-tokens)
	(loop (lexer) (cons token list-of-tokens))))))


;;;; done

)

;;; end of file
