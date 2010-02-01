;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/BLAS
;;;Contents: tests for the high-level library
;;;Date: Mon Feb  1, 2010
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
  (compensations)
  (only (foreign ffi sizeof) sizeof-double)
  (only (foreign ffi)
	array-set-c-double!
	array-ref-c-double)
  (only (foreign memory) malloc/c)
  (prefix (foreign math blas) blas:)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing BLAS platform\n")


(parametrise ((check-test-name	'base))

  (check
      (with-compensations
	(let* ((order	blas:col-major)
	       (transa	blas:no-trans)
	       (m	4) ;Size of Column (the number of rows)
	       (n	4) ;Size of Row (the number of columns)
	       (lda	4) ;Leading dimension of 5 * 4 matrix is 5
	       (incx	1)
	       (incy	1)
	       (alpha	1.)
	       (beta	0.)
	       (a	(malloc/c (* sizeof-double m n)))
	       (x	(malloc/c (* sizeof-double n)))
	       (y	(malloc/c (* sizeof-double n))))

	  ;; the elements of the first column
	  (array-set-c-double! a 0 1.)
	  (array-set-c-double! a 1 2.)
	  (array-set-c-double! a 2 3.)
	  (array-set-c-double! a 3 4.)
	  ;; the elements of the second column
	  (array-set-c-double! a m  1.)
	  (array-set-c-double! a (+ m 1) 1.)
	  (array-set-c-double! a (+ m 2) 1.)
	  (array-set-c-double! a (+ m 3) 1.)
	  ;; the elements of the third column
	  (array-set-c-double! a (* m 2)  3.)
	  (array-set-c-double! a (+ 1 (* m 2)) 4.)
	  (array-set-c-double! a (+ 2 (* m 2)) 5.)
	  (array-set-c-double! a (+ 3 (* m 2)) 6.)
	  ;; the elements of the fourth column
	  (array-set-c-double! a (* m 3)  5.)
	  (array-set-c-double! a (+ 1 (* m 3)) 6.)
	  (array-set-c-double! a (+ 2 (* m 3)) 7.)
	  (array-set-c-double! a (+ 3 (* m 3)) 8.)
	  ;; the elements of x and y
	  (array-set-c-double! x 0 1.)
	  (array-set-c-double! x 1 2.)
	  (array-set-c-double! x 2 1.)
	  (array-set-c-double! x 3 1.)
	  (array-set-c-double! y 0 0.)
	  (array-set-c-double! y 1 0.)
	  (array-set-c-double! y 2 0.)
	  (array-set-c-double! y 3 0.)

	  (blas:dgemv order transa m n alpha a lda x incx beta y incy)

	  (map (lambda (i)
		 (array-ref-c-double y i))
	    '(0 1 2 3))))
    => '(11. 14. 17. 20.))

  #t)


;;;; done

(check-report)

;;; end of file
