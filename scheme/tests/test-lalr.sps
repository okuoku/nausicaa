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
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing lalr\n")


(define (doit string)
  (let* ((IS		(lexer-make-IS :string string :counters 'all))
	 (lexer		(lexer-make-lexer calc-parser-lexer-table IS))
	 (error-handler	(lambda (message token)
			  (error #f (string-append message
						   " line "
						   (number->string
						    (source-location-line
						     (lexical-token-source token))))
				 (lexical-token-value token))))
	 (parser	(make-calc-parser)))
    (parameterise ((table-of-variables (make-eq-hashtable))
		   (evaluated-expressions '()))
      (parser lexer error-handler)
      (evaluated-expressions))))

(check
      (doit "1\n")
  => '(1))

(check
      (doit "1 + 2 + 4 * 3 \n a = 2 \n a * 3 \n")
  => '(6 15))

(check
      (doit "sin(1.2)\n")
  => (list (sin 1.2)))

(check
      (doit "atan(1.2, 0.5)\n")
  => (list (atan 1.2 0.5)))



;;;; done

(check-report)

;;; end of file
