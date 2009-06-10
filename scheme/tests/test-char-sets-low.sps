;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test for char-sets low level library
;;;Date: Wed Jun 10, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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


;;;; setup

(import (nausicaa)
  (checks)
  (char-sets low))

(check-set-mode! 'report-failed)
(display "*** testing char-sets low\n")


(parameterise ((check-test-name	'range-constructors))

  (check
      (%make-range 10 20)
    => '(10 . 20))

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(%make-range 10 10))
    => #t)

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(%make-range -10 20))
    => #t)

  )


(parameterise ((check-test-name	'range-inspection))

  (check
      (%range-length (%make-range 10 20))
    => 10)

  (check
      (%range-length (%make-range 10 11))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (%range-empty? '(10 . 20))
    => #f)

  (check
      (%range-empty? '(10 . 10))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (%range-contains? (%make-range 10 20) 10)
    => #t)

  (check
      (%range-contains? (%make-range 10 20) 20)
    => #f)

  (check
      (%range-contains? (%make-range 10 20) 1)
    => #f)

  (check
      (%range-contains? (%make-range 10 20) 30)
    => #f)

  (check
      (%range-contains? (%make-range 10 20) 15)
    => #t)

  )


(parameterise ((check-test-name	'range-comparison))

  (check
      (%range=? (%make-range 10 20)
		(%make-range 10 20))
    => #t)

  (check
      (%range=? (%make-range 9 20)
		(%make-range 10 20))
    => #f)

  (check
      (%range=? (%make-range 10 20)
		(%make-range 10 21))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (%range<? (%make-range 10 20)
		(%make-range 10 20))
    => #f)

  (check
      (%range<? (%make-range 10 20)
		(%make-range 30 40))
    => #t)

  (check
      (%range<? (%make-range 30 40)
		(%make-range 10 20))
    => #f)

  (check
      (%range<? (%make-range 10 20)
		(%make-range 15 25))
    => #f)

  (check
      (%range<? (%make-range 15 25)
		(%make-range 10 20))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (%range-start<? (%make-range 10 20)
		      (%make-range 15 25))
    => #t)

  (check
      (%range-start<? (%make-range 15 20)
		      (%make-range 15 25))
    => #f)

  (check
      (%range-start<? (%make-range 18 20)
		      (%make-range 15 25))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (%range-start<=? (%make-range 10 20)
		       (%make-range 15 25))
    => #t)

  (check
      (%range-start<=? (%make-range 15 20)
		       (%make-range 15 25))
    => #t)

  (check
      (%range-start<=? (%make-range 18 20)
		       (%make-range 15 25))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (%range-contiguous? (%make-range 10 20)
			  (%make-range 20 25))
    => #t)

  (check
      (%range-contiguous? (%make-range 10 30)
			  (%make-range 20 40))
    => #f)

  (check
      (%range-contiguous? (%make-range 20 40)
			  (%make-range 10 30))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (%range-overlapping? (%make-range 10 20)
			   (%make-range 20 25))
    => #f)

  (check
      (%range-overlapping? (%make-range 10 30)
			   (%make-range 20 40))
    => #t)

  (check
      (%range-overlapping? (%make-range 20 40)
			   (%make-range 10 30))
    => #t)

  )


(parameterise ((check-test-name	'range-operation))

  (check
      (%range-concatenate (%make-range 10 20)
			  (%make-range 10 20))
    (=> %range=?)
    (%make-range 10 20))

  (check
      (%range-concatenate (%make-range 10 20)
			  (%make-range 30 40))
    (=> %range=?)
    (%make-range 10 40))

;;; --------------------------------------------------------------------

  (check
      (%range-union (%make-range 10 20)
		    (%make-range 10 20))
    (=> %range=?)
    (%make-range 10 20))

  (check
      (%range-union (%make-range 10 30)
		    (%make-range 20 40))
    (=> %range=?)
    (%make-range 10 40))

  (check
      (%range-union (%make-range 10 20)
		    (%make-range 20 40))
    (=> %range=?)
    (%make-range 10 40))

  (check
      (%range-union (%make-range 20 40)
		    (%make-range 10 20))
    (=> %range=?)
    (%make-range 10 40))

  (check
      (%range-union (%make-range 10 20)
		    (%make-range 30 40))
    => '((10 . 20) (30 . 40)))

;;; --------------------------------------------------------------------

  (check
      ;; equal ranges
      (%range-difference (%make-range 10 20)
			 (%make-range 10 20))
    => #f)

  (check
      ;; overlapping ranges
      (%range-difference (%make-range 10 30)
			 (%make-range 20 40))
    => '((10 . 20) (30 . 40)))

  (check
      ;; overlapping ranges
      (%range-difference (%make-range 20 40)
			 (%make-range 10 30))
    => '((10 . 20) (30 . 40)))

  (check
      ;; non-overlapping ranges
      (%range-difference (%make-range 10 20)
			 (%make-range 30 40))
    => '((10 . 20) (30 . 40)))

  (check
      ;; non-overlapping ranges
      (%range-difference (%make-range 30 40)
			 (%make-range 10 20))
    => '((10 . 20) (30 . 40)))

  (check
      ;; contiguous ranges
      (%range-difference (%make-range 10 20)
			 (%make-range 20 40))
    (=> %range=?)
    (%make-range 10 40))

  (check
      ;; contiguous ranges
      (%range-difference (%make-range 20 40)
			 (%make-range 10 20))
    (=> %range=?)
    (%make-range 10 40))

  )


;;;; done

(check-report)

;;; end of file
