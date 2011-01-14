;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for XML lexer
;;;Date: Fri Jan 14, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (nausicaa)
  (nausicaa parser-tools)
  (prefix (nausicaa silex lexer) lex.)
  (prefix (nausicaa xml tags lexer) xml.)
  (nausicaa checks))


;;;; helpers

(define-syntax test-lexical-error
  (syntax-rules ()
    ((_ ?lexer ?string)
     (check
	 (guard (E ((lexical-violation? E)
;;;		      (display (condition-message E))(newline)
		    (condition-irritants E))
		   (else E))
	   (?lexer ?string))
       => '((*lexer-error* ?string))))
    ((_ ?name ?lexer ?string)
     (check ?name
       (guard (E ((lexical-violation? E)
;;;		      (display (condition-message E))(newline)
		  (condition-irritants E))
		 (else E))
	 (?lexer ?string))
       => '((*lexer-error* ?string))))
    ))


(parametrise ((check-test-name	'string-tokeniser))

  (define (tokenise string)
    (let* ((IS		(lex.lexer-make-IS
			 (lex.string: string)
			 (lex.counters: 'all)))
	   (lexer	(lex.lexer-make-lexer xml.xml-lexer-table IS))
	   (result	'()))
      (do (((T <lexical-token>) (lexer) (lexer)))
	  (T.special?
	   (reverse `(,(if (is-a? T <lexical-token>)
			   T.category
			 T)
		      . ,result)))
	(set-cons! result T))))

;;; --------------------------------------------------------------------

  (check	;empty string
      (tokenise "\"")
    => '(STRING *eoi*))


  #t)


;;;; done

(check-report)

;;; end of file
