;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for low level routines of (libraries)
;;;Date: Sat Apr 10, 2010
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
  (libraries low)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing low level routines of libraries library\n")


(parametrise ((check-test-name	'library-reference))

  (check
      (%version-reference? '())
    => #t)

  (check
      (%version-reference? '(1))
    => #t)

  (check
      (%version-reference? '(1 2 3))
    => #t)

  (check
      (%version-reference? '(and))
    => #f)

  (check
      (%version-reference? '(and 1))
    => #t)

  (check
      (%version-reference? '(and 1 2))
    => #t)

  (check
      (%version-reference? '(or))
    => #f)

  (check
      (%version-reference? '(or 1))
    => #t)

  (check
      (%version-reference? '(or 1 2))
    => #t)

  (check
      (%version-reference? '(not))
    => #f)

  (check
      (%version-reference? '(not 1))
    => #t)

  (check
      (%version-reference? '(not 1 2))
    => #f)

  (check
      (%version-reference? '(<= 1))
    => #f)

  (check
      (%version-reference? '((<=)))
    => #f)

  (check
      (%version-reference? '((<= 1)))
    => #t)

  (check
      (%version-reference? '((<= 1 2)))
    => #f)

  (check
      (%version-reference? '(>= 1))
    => #f)

  (check
      (%version-reference? '((>=)))
    => #f)

  (check
      (%version-reference? '((>= 1)))
    => #t)

  (check
      (%version-reference? '((>= 1 2)))
    => #f)

  (check
      (%version-reference? '((>= 1) (<= 2) (or 3 4)))
    => #t)

  (check
      (%version-reference? '(1 (<= 2) (or 10 (and (>= 4) (>= 2)))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (%library-reference? '())
    => #f)

  (check
      (%library-reference? '(alpha))
    => #t)

  (check
      (%library-reference? '(alpha beta))
    => #t)

  (check
      (%library-reference? '(alpha beta gamma))
    => #t)

  (check
      (%library-reference? '(alpha beta gamma ()))
    => #t)

  (check
      (%library-reference? '(alpha beta gamma (1)))
    => #t)

  (check
      (%library-reference? '(alpha beta gamma (1 2 3)))
    => #t)

  (check
      (%library-reference? '(alpha 123 gamma))
    => #f)

  (check
      (%library-reference? '(alpha beta gamma (1 ciao)))
    => #f)

  (check
      (%library-reference? '(alpha ((>= 1) (<= 2) (or 3 4))))
    => #t)

  (check
      (%library-reference? '(alpha beta (1 (<= 2) (or 10 (and (>= 4) (>= 2))))))
    => #t)

  #t)


(parametrise ((check-test-name	'conformance-sub-version))

  (check
      (%sub-version-conforms-to-sub-version-reference? 1 1)
    => #t)

  (check
      (%sub-version-conforms-to-sub-version-reference? 1 0)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (%sub-version-conforms-to-sub-version-reference? 1 '(<= 1))
    => #t)

  (check
      (%sub-version-conforms-to-sub-version-reference? 2 '(<= 1))
    => #f)

  (check
      (%sub-version-conforms-to-sub-version-reference? 0 '(<= 1))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (%sub-version-conforms-to-sub-version-reference? 1 '(>= 1))
    => #t)

  (check
      (%sub-version-conforms-to-sub-version-reference? 2 '(>= 1))
    => #t)

  (check
      (%sub-version-conforms-to-sub-version-reference? 0 '(>= 1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (%sub-version-conforms-to-sub-version-reference? 1 '(not 1))
    => #f)

  (check
      (%sub-version-conforms-to-sub-version-reference? 1 '(not 0))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (%sub-version-conforms-to-sub-version-reference? 1 '(or 1 2))
    => #t)

  (check
      (%sub-version-conforms-to-sub-version-reference? 1 '(or 2 1))
    => #t)

  (check
      (%sub-version-conforms-to-sub-version-reference? 1 '(or 2 3))
    => #f)

  (check
      (%sub-version-conforms-to-sub-version-reference? 1 '(or 2 3 4))
    => #f)

  (check
      (%sub-version-conforms-to-sub-version-reference? 4 '(or 2 3 4))
    => #t)

  (check
      (%sub-version-conforms-to-sub-version-reference? 5 '(or 2 (or 3 (or 4 (or 5)))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (%sub-version-conforms-to-sub-version-reference? 1 '(and 1))
    => #t)

  (check
      (%sub-version-conforms-to-sub-version-reference? 1 '(and 1 1))
    => #t)

  (check
      (%sub-version-conforms-to-sub-version-reference? 1 '(and 1 1 1))
    => #t)

  (check
      (%sub-version-conforms-to-sub-version-reference? 1 '(and 1 (and 1 (and 1 (and 1)))))
    => #t)

  (check
      (%sub-version-conforms-to-sub-version-reference? 1 '(and (>= 0) (<= 2)))
    => #t)

  (check
      (%sub-version-conforms-to-sub-version-reference? 0 '(and (>= 0) (<= 2)))
    => #t)

  (check
      (%sub-version-conforms-to-sub-version-reference? 2 '(and (>= 0) (<= 2)))
    => #t)

  (check
      (%sub-version-conforms-to-sub-version-reference? 1 '(and (>= 0) (<= 2) (not 3)))
    => #t)

  (check
      (%sub-version-conforms-to-sub-version-reference? 1 '(and (>= 0) (<= 2) (not 1)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (%sub-version-conforms-to-sub-version-reference? 1 '(or (and (>= 0) (<= 2))
							      4))
    => #t)

  (check
      (%sub-version-conforms-to-sub-version-reference? 4 '(or (and (>= 0) (<= 2))
							      4))
    => #t)

  (check
      (%sub-version-conforms-to-sub-version-reference? 3 '(or (and (>= 0) (<= 2))
							      4))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(%sub-version-conforms-to-sub-version-reference? 3 '(>=)))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(%sub-version-conforms-to-sub-version-reference? 3 '(<=)))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(%sub-version-conforms-to-sub-version-reference? 3 '(and)))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(%sub-version-conforms-to-sub-version-reference? 3 '(not)))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(%sub-version-conforms-to-sub-version-reference? 3 '(not ciao)))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(%sub-version-conforms-to-sub-version-reference? 3 '(not)))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(%sub-version-conforms-to-sub-version-reference? 3 '(or)))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(%sub-version-conforms-to-sub-version-reference? 3 '(ciao)))
    => #t)

  #t)


(parametrise ((check-test-name	'low-import-specs))

  (check
      (%apply-import-spec/only '() '(a b c))
    => '())

  (check
      (%apply-import-spec/only '((a b)) '())
    => '())

  (check
      (%apply-import-spec/only '((a b)) '(b))
    => '((a b)))

  (check
      (%apply-import-spec/only '((a b)) '(b c))
    => '((a b)))

  (check
      (%apply-import-spec/only '((a ea) (b eb) (c ec))
			       '(eb ec))
    => '((b eb) (c ec)))

;;; --------------------------------------------------------------------

  (check
      (%apply-import-spec/except '() '(b))
    => '())

  (check
      (%apply-import-spec/except '((a b)) '())
    => '((a b)))

  (check
      (%apply-import-spec/except '((a b)) '(b))
    => '())

  (check
      (%apply-import-spec/except '((a b)) '(c))
    => '((a b)))

  (check
      (%apply-import-spec/except '((a ea) (b eb) (c ec))
				 '(eb ec))
    => '((a ea)))

;;; --------------------------------------------------------------------

  (check
      (%apply-import-spec/prefix '() 'ciao:)
    => '())

  (check
      (%apply-import-spec/prefix '((a b)) 'ciao:)
    => '((a ciao:b)))

  (check
      (%apply-import-spec/prefix '((a ea) (b eb) (c ec))
				 'ciao:)
    => '((a ciao:ea) (b ciao:eb) (c ciao:ec)))

;;; --------------------------------------------------------------------

  (check
      (%apply-import-spec/rename '() '())
    => '())

  (check
      (%apply-import-spec/rename '((a b)) '())
    => '((a b)))

  (check
      (%apply-import-spec/rename '() '((a b)))
    => '())

  (check
      (%apply-import-spec/rename '((a b)) '((b c)))
    => '((a c)))

  (check
      (%apply-import-spec/rename '((a ea) (b eb) (c ec))
				 '((eb ebb) (ec ecc)))
    => '((a ea) (b ebb) (c ecc)))

  #t)


;;;; done

(check-report)

;;; end of file
