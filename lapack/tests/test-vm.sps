;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/LAPACK
;;;Contents: tests for the vm library
;;;Date: Sun Feb  7, 2010
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
  (foreign memory)
  (foreign math lapack vm)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing knuth-morris-pratt\n")


(parametrise ((check-test-name	'rv))

  (check
      (with-compensations
	(let* ((len	5)
	       (vec	(rvc/c len)))
	  (rvc-fill! vec '(1 2 3 4 5))
	  (rvc->list vec len)))
    => '(1. 2. 3. 4. 5.))

  (check
      (with-compensations
	(let* ((len	5)
	       (vec	(rvc/c len)))
	  (rvc-set! vec 2 5)
	  (rvc-ref vec 2)))
    => 5.)

  #t)


(parametrise ((check-test-name	'cv))

  (check
      (with-compensations
	(let* ((len	5)
	       (vec	(cvc/c len)))
	  (cvc-fill! vec '(1+2i 3+4i 5+6i 7+8i 9))
	  (cvc->list vec len)))
    => '(1.+2.i 3.+4.i 5.+6.i 7.+8.i 9.+0.i))

  (check
      (with-compensations
	(let* ((len	5)
	       (vec	(cvc/c len)))
	  (cvc-set! vec 2 5+4i)
	  (cvc-ref vec 2)))
    => 5.+4.i)

  #t)


(parametrise ((check-test-name	'rm))

  (check
      (with-compensations
	(let* ((row-num	2)
	       (col-num 3)
	       (ldm	row-num)
	       (mat	(rmx/c row-num col-num)))
	  (rmx-fill! mat ldm '((1 2 3)
			   (4 5 6)))
	  (rmx->list mat ldm row-num col-num)))
    => '((1. 2. 3.)
	 (4. 5. 6.)))

  (check
      (with-compensations
	(let* ((row-num	2)
	       (col-num 3)
	       (ldm	row-num)
	       (mat	(rmx/c row-num col-num)))
	  (rmx-set! mat ldm 1 2 5)
	  (rmx-ref mat ldm 1 2)))
    => 5.)

  #t)


(parametrise ((check-test-name	'cm))

  (check
      (with-compensations
	(let* ((row-num	2)
	       (col-num 3)
	       (ldm	row-num)
	       (mat	(cmx/c row-num col-num)))
	  (cmx-fill! mat ldm '((1+2i 3+4i 5+6i)
			       (7+8i 9 10)))
	  (cmx->list mat ldm row-num col-num)))
    => '((1.+2.i 3.+4.i 5.+6.i)
	 (7.+8.i 9.+0.i 10.+0.i)))

  (check
      (with-compensations
	(let* ((row-num	2)
	       (col-num 3)
	       (ldm	row-num)
	       (mat	(cmx/c row-num col-num)))
	  (cmx-set! mat ldm 1 2 5)
	  (cmx-ref mat ldm 1 2)))
    => 5.+0.i)

  #t)


;;;; done

(check-report)

;;; end of file
