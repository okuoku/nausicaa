;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexer for R6RS programs and libraries
;;;Date: Wed Dec 22, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa r6rs lexer)
  (export
    r6rs-lexer-table
    r6rs-nested-comment-lexer-table
    r6rs-string-lexer-table

    parse-string)
  (import (nausicaa)
    (nausicaa r6rs lexer-table)
    (nausicaa r6rs string-lexer-table)
    (nausicaa r6rs nested-comment-lexer-table)
    (nausicaa parser-tools lexical-token)
    (nausicaa silex lexer)
    )


(define (parse-string IS)
  ;;Given  an input  system  from  which a  double  quote character  has
  ;;already  been consumed,  read  characters composing  an R6RS  string
  ;;stopping at the ending double quote.  Return the Scheme string.
  ;;
  ;;If an error occurs reading  the string: a condition object is raised
  ;;with  components &lexical,  &message, &who,  &irritants;  the single
  ;;value in the &irritants list is the string that caused the error.
  ;;
  ;;If end of  input is found reading the string:  a condition object is
  ;;raised with components &lexical, &message, &who, &irritants.
  ;;
  (let-values (((port getter)	(open-string-output-port))
	       ((lexer)		(lexer-make-lexer r6rs-string-lexer-table IS)))
    (let next (((T <lexical-token>) (lexer)))
      (define (%error message)
	(raise
	 (condition (make-lexical-violation)
		    (make-message-condition message)
		    (make-who-condition 'parse-string)
		    (make-irritants-condition (list T.value)))))
      (cond (T.end-of-input?
	     (%error "end of input found while parsing string"))
	    (T.lexer-error?
	     (%error "lexical violation while parsing string"))
	    ((eq? T 'STRING)
	     (getter))
	    (else
	     (display T port)
	     (next (lexer)))))))


;;;; done

)

;;; end of file
