;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for R6RS lexer
;;;Date: Wed Dec 22, 2010
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
(import (nausicaa)
  (nausicaa silex lexer)
  (nausicaa parser-tools lexical-token)
  (nausicaa parser-tools source-location)
  (nausicaa r6rs lexer)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing R6RS lexer\n")

(define eoi `(*eoi* . ,(eof-object)))


(parametrise ((check-test-name	'identifiers))

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(lexer-make-lexer r6rs-lexer-table IS))
	   (result		'()))
      (do (((T <lexical-token>) (lexer) (lexer)))
	  (T.special?
	   (reverse `((,T.category . ,T.value)
		      . ,result)))
	(set! result (cons T result)))))

;;; --------------------------------------------------------------------

  (check	;empty string
      (tokenise "ciao")
    => `(IDENTIFIER ,eoi))

  #t)


;;;; done

(check-report)

;;; end of file
