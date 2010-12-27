;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for assertions library
;;;Date: Mon Oct 25, 2010
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
  (prefix (nausicaa language assertions) ass.)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing assertions\n")


(parametrise ((check-test-name	'assert))

  (check
      (ass.assert (integer? 1))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
;;;		 (write (condition-message E))(newline)
		 #t)
		(else E))
	(ass.assert (integer? "ciao")))
    => #t)

  #t)


(parametrise ((check-test-name	'arguments))

  (define (doit a b c)
    (ass.arguments (ass.who 'doit)
		   (ass.number-of-arguments 3)
		   (ass.argument (ass.formal a)
				 (ass.ordinal 1)
				 (ass.predicate integer?)
				 (ass.description "value of a"))
		   (ass.argument (ass.formal b)
				 (ass.ordinal 2)
				 (ass.predicate string?)
				 (ass.description "value of b"))
		   (ass.argument (ass.formal c)
				 (ass.ordinal 3)
				 (ass.predicate symbol?)
				 (ass.description "value of c")))
    (list a b c))

;;; --------------------------------------------------------------------

  (check
      (doit 1 "two" 'three)
    => '(1 "two" three))

  (check
      (guard (E ((assertion-violation? E)
		 (condition-message E))
		(else E))
	(doit #\a "two" 'three))
    => "expected value of a as argument number 1")

  (check
      (guard (E ((assertion-violation? E)
		 (condition-message E))
		(else E))
	(doit 1 2 'three))
    => "expected value of b as argument number 2")

  (check
      (guard (E ((assertion-violation? E)
		 (condition-message E))
		(else E))
	(doit 1 "two" 3))
    => "expected value of c as argument number 3")

  #t)


;;;; done

(check-report)

;;; end of file
