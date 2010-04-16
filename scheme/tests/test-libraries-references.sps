;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for library references handling
;;;Date: Fri Apr 16, 2010
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


(import (nausicaa)
  (libraries names)
  (libraries references)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing library references\n")


(parametrise ((check-test-name	'decomposition))

  (check
      (receive (identifiers version-reference)
	  (library-reference-decompose '(alpha beta (1 (<= 2) (or 10 (and (>= 4) (>= 2))))))
	(vector identifiers version-reference))
    => '#( (alpha beta)  (1 (<= 2) (or 10 (and (>= 4) (>= 2)))) ))

;;; --------------------------------------------------------------------

  (check
      (library-reference->identifiers '(alpha beta (1 (<= 2) (or 10 (and (>= 4) (>= 2))))))
    => '(alpha beta))

  (check
      (library-reference->version-reference '(alpha beta (1 (<= 2) (or 10 (and (>= 4) (>= 2))))))
    => '(1 (<= 2) (or 10 (and (>= 4) (>= 2)))))

  #t)


(parametrise ((check-test-name	'predicates))

  (check
      (library-version-reference? '())
    => #t)

  (check
      (library-version-reference? '(1))
    => #t)

  (check
      (library-version-reference? '(1 2 3))
    => #t)

  (check
      (library-version-reference? '(and))
    => #f)

  (check
      (library-version-reference? '(and 1))
    => #t)

  (check
      (library-version-reference? '(and 1 2))
    => #t)

  (check
      (library-version-reference? '(or))
    => #f)

  (check
      (library-version-reference? '(or 1))
    => #t)

  (check
      (library-version-reference? '(or 1 2))
    => #t)

  (check
      (library-version-reference? '(not))
    => #f)

  (check
      (library-version-reference? '(not 1))
    => #t)

  (check
      (library-version-reference? '(not 1 2))
    => #f)

  (check
      (library-version-reference? '(<= 1))
    => #f)

  (check
      (library-version-reference? '((<=)))
    => #f)

  (check
      (library-version-reference? '((<= 1)))
    => #t)

  (check
      (library-version-reference? '((<= 1 2)))
    => #f)

  (check
      (library-version-reference? '(>= 1))
    => #f)

  (check
      (library-version-reference? '((>=)))
    => #f)

  (check
      (library-version-reference? '((>= 1)))
    => #t)

  (check
      (library-version-reference? '((>= 1 2)))
    => #f)

  (check
      (library-version-reference? '((>= 1) (<= 2) (or 3 4)))
    => #t)

  (check
      (library-version-reference? '(1 (<= 2) (or 10 (and (>= 4) (>= 2)))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (library-reference? '())
    => #f)

  (check
      (library-reference? '(alpha))
    => #t)

  (check
      (library-reference? '(alpha beta))
    => #t)

  (check
      (library-reference? '(alpha beta gamma))
    => #t)

  (check
      (library-reference? '(alpha beta gamma ()))
    => #t)

  (check
      (library-reference? '(alpha beta gamma (1)))
    => #t)

  (check
      (library-reference? '(alpha beta gamma (1 2 3)))
    => #t)

  (check
      (library-reference? '(alpha 123 gamma))
    => #f)

  (check
      (library-reference? '(alpha beta gamma (1 ciao)))
    => #f)

  (check
      (library-reference? '(alpha ((>= 1) (<= 2) (or 3 4))))
    => #t)

  (check
      (library-reference? '(alpha beta (1 (<= 2) (or 10 (and (>= 4) (>= 2))))))
    => #t)

  #f)


(parametrise ((check-test-name	'conformance-sub-version))

  (check
      (conforming-sub-version-and-sub-version-reference? 1 1)
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 0)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(<= 1))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 2 '(<= 1))
    => #f)

  (check
      (conforming-sub-version-and-sub-version-reference? 0 '(<= 1))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(>= 1))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 2 '(>= 1))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 0 '(>= 1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(not 1))
    => #f)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(not 0))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(or 1 2))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(or 2 1))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(or 2 3))
    => #f)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(or 2 3 4))
    => #f)

  (check
      (conforming-sub-version-and-sub-version-reference? 4 '(or 2 3 4))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 5 '(or 2 (or 3 (or 4 (or 5)))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(and 1))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(and 1 1))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(and 1 1 1))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(and 1 (and 1 (and 1 (and 1)))))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(and (>= 0) (<= 2)))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 0 '(and (>= 0) (<= 2)))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 2 '(and (>= 0) (<= 2)))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(and (>= 0) (<= 2) (not 3)))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(and (>= 0) (<= 2) (not 1)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(or (and (>= 0) (<= 2))
								4))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 4 '(or (and (>= 0) (<= 2))
								4))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 3 '(or (and (>= 0) (<= 2))
								4))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(conforming-sub-version-and-sub-version-reference? 3 '(>=)))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(conforming-sub-version-and-sub-version-reference? 3 '(<=)))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(conforming-sub-version-and-sub-version-reference? 3 '(and)))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(conforming-sub-version-and-sub-version-reference? 3 '(not)))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(conforming-sub-version-and-sub-version-reference? 3 '(not ciao)))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(conforming-sub-version-and-sub-version-reference? 3 '(not)))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(conforming-sub-version-and-sub-version-reference? 3 '(or)))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(conforming-sub-version-and-sub-version-reference? 3 '(ciao)))
    => #t)

  #t)


(parametrise ((check-test-name	'conformance-version))

  (check
      (conforming-version-and-version-reference? '() '())
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '())
    => #t)

  (check
      (conforming-version-and-version-reference? '() '(1))
    => #f)

  (check
      (conforming-version-and-version-reference? '(1) '(1))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '(0))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1) '((<= 1)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(2) '((<= 1)))
    => #f)

  (check
      (conforming-version-and-version-reference? '(0) '((<= 1)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1) '((>= 1)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(2) '((>= 1)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(0) '((>= 1)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1) '((not 1)))
    => #f)

  (check
      (conforming-version-and-version-reference? '(1) '((not 0)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1) '((or 1 2)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '((or 2 1)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '((or 2 3)))
    => #f)

  (check
      (conforming-version-and-version-reference? '(1) '((or 2 3 4)))
    => #f)

  (check
      (conforming-version-and-version-reference? '(4) '((or 2 3 4)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(5) '((or 2 (or 3 (or 4 (or 5))))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1) '((and 1)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '((and 1 1)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '((and 1 1 1)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '((and 1 (and 1 (and 1 (and 1))))))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '((and (>= 0) (<= 2))))
    => #t)

  (check
      (conforming-version-and-version-reference? '(0) '((and (>= 0) (<= 2))))
    => #t)

  (check
      (conforming-version-and-version-reference? '(2) '((and (>= 0) (<= 2))))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '((and (>= 0) (<= 2) (not 3))))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '((and (>= 0) (<= 2) (not 1))))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1) '((or (and (>= 0) (<= 2))
							    4)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(4) '((or (and (>= 0) (<= 2))
							    4)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(3) '((or (and (>= 0) (<= 2))
							    4)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(1 2 3))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(1 2 4))
    => #f)

  (check
      (conforming-version-and-version-reference? '(1 2 0) '(1 2))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1 2 1) '(1 2))
    => #f)

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(1 (>= 1) (not 1)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(1 (<= 1) (not 1)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(or (1 (<= 1) (not 1))
							       (1 (>= 1) (not 1))))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(or (1 2 5) (1 2 10)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(and (1 (>= 1) (not 1))
								(1 2 (not 5))))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(and (1 (<= 1) (not 1))
								(1 2 (not 5))))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(not (1 5 4)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(not (1 2 3)))
    => #f)

  #t)


(parametrise ((check-test-name	'conformance-reference))

  (check
      (conforming-library-name-and-library-reference? '(a (1)) '(a (1)))
    => #t)

  (check
      (conforming-library-name-and-library-reference? '(a (1)) '(a (0)))
    => #f)

  (check
      (conforming-library-name-and-library-reference? '(a b c (1)) '(a b c (1)))
    => #t)

  (check
      (conforming-library-name-and-library-reference? '(a b c (1)) '(a b c (0)))
    => #f)

  (check
      (conforming-library-name-and-library-reference? '(a b c (1)) '(a z c (1)))
    => #f)

  #f)


;;;; done

(check-report)

;;; end of file
