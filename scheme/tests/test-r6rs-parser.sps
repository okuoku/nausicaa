;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for R6RS parser
;;;Date: Fri Jan  7, 2011
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
  (nausicaa silex lexer)
  (nausicaa r6rs lexer)
  (nausicaa r6rs parser)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing R6RS parser\n")


(parametrise ((check-test-name	'sexps))

  (define (error-handler message token)
    #f)

  (define (parse string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(make-token-lexer IS))
	   (parser	(make-r6rs-parser)))
      (parser lexer error-handler #f)))

  (check
      (parse "ciao")
    => 'ciao)

  #t)


;;;; done

(check-report)

;;; end of file
