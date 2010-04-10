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


(parametrise ((check-test-name	'low-import-specs))

  (check
      (%library-version-ref '(alpha))
    => '())

  (check
      (%library-version-ref '(alpha beta))
    => '())

  (check
      (%library-version-ref '(alpha beta gamma))
    => '())

  (check
      (%library-version-ref '(alpha beta gamma ()))
    => '())

  (check
      (%library-version-ref '(alpha beta gamma (1)))
    => '(1))

  (check
      (%library-version-ref '(alpha beta gamma (1 2 3)))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (%library-version? '())
    => #t)

  (check
      (%library-version? '(1))
    => #t)

  (check
      (%library-version? '(1 2 3))
    => #t)

  (check
      (%library-version? 'ciao)
    => #f)

  (check
      (%library-version? '(1 ciao))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (%library-name? '())
    => #f)

  (check
      (%library-name? '(alpha))
    => #t)

  (check
      (%library-name? '(alpha beta))
    => #t)

  (check
      (%library-name? '(alpha beta gamma))
    => #t)

  (check
      (%library-name? '(alpha beta gamma ()))
    => #t)

  (check
      (%library-name? '(alpha beta gamma (1)))
    => #t)

  (check
      (%library-name? '(alpha beta gamma (1 2 3)))
    => #t)

  (check
      (%library-name? '(alpha 123 gamma))
    => #f)

  (check
      (%library-name? '(alpha beta gamma (1 ciao)))
    => #f)

;;; --------------------------------------------------------------------

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
