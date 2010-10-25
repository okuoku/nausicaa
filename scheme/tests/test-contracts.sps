;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for contracts
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
  (contracts)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing contracts\n")


(parametrise ((check-test-name	'base))

  (let ()	;no return value check
    (define (%doit a b c)
      (list a b c))

    (define-contract doit %doit
      integer? string? symbol?)

    (check
	(doit 1 "two" 'three)
      => '(1 "two" three))

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (doit #\a "two" 'three))
      => #t)

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (doit 1 2 'three))
      => #t)

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (doit 1 "two" 3))
      => #t)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;return value check

    (define flag
      (make-parameter #t))

    (define (%doit a b c)
      (if (flag)
	  (list a b c)
	(vector a b c)))

    (define-contract doit %doit
      integer? string? symbol? -> list?)

    (check
	(doit 1 "two" 'three)
      => '(1 "two" three))

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (doit #\a "two" 'three))
      => #t)

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (doit 1 2 'three))
      => #t)

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (doit 1 "two" 3))
      => #t)

    (check
	(guard (E ((assertion-violation? E)
;;;		   (write E)(newline)
		   #t)
		  (else E))
	  (parametrise ((flag #f))
            (doit 1 "two" 'three)))
      => #t)

    ;; (parametrise ((flag #f))
    ;;   (doit 1 "two" 'three))

    #f)


  #t)


;;;; done

(check-report)

;;; end of file
