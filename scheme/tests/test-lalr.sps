;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for lalr
;;;Date: Thu Jul 16, 2009
;;;
;;;Abstract
;;;
;;;	Simple calculator in Scheme.
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;Copyright (c) 2004 Dominique Boucher
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


(import (nausicaa)
  (silex lexer)
  (calc-parser)
  (calc-parser-helper)
  (calc-parser-lexer)
  (lalr common)
  (checks)
  (rnrs mutable-pairs))

(check-set-mode! 'report-failed)
(display "*** testing lalr\n")



(define (doit string)
  (let* ((IS		(lexer-make-IS :string string :counters 'line))
	 (lexer		(lexer-make-lexer calc-parser-lexer-table IS))
	 (error-handler	(lambda (message token)
			  (error #f (string-append message
						   " line "
						   (number->string
						    (source-location-line
						     (lexical-token-source token))))
				 (lexical-token-value token)))))
    (calc-parser lexer error-handler)))

(parameterise ((table-of-variables (make-eq-hashtable)))
  (doit "1 + 2 + 4 * 3
a = 2
a * 3
"))


;;;; done

(check-report)

;;; end of file
