;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/BLAS
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
  (foreign math blas vm)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing VM\n")


(parametrise ((check-test-name	'real-vectors))

  (check
      (with-compensations
	(let* ((N 5) (X (svec/c N)))
	  (svec-fill! X 1 '(1 2 3 4 5))
	  (svec->list N X 1)))
    => '(1. 2. 3. 4. 5.))

  (check
      (with-compensations
	(let* ((N 5) (X (svec/c N)))
	  (svec-set! X 1 2 5)
	  (svec-ref  X 1 2)))
    => 5.)

  (check
      (with-compensations
	(let* ((N 5) (X (dvec/c N)))
	  (dvec-fill! X 1 '(1 2 3 4 5))
	  (dvec->list N X 1)))
    => '(1. 2. 3. 4. 5.))

  (check
      (with-compensations
	(let* ((N 5) (X (dvec/c N)))
	  (dvec-set! X 1 2 5)
	  (dvec-ref  X 1 2)))
    => 5.)

  #t)


(parametrise ((check-test-name	'complex-vectors))

  (check
      (with-compensations
	(let* ((N 5) (X (cvec/c N)))
	  (cvec-fill! X 1 '(1+2i 3+4i 5+6i 7+8i 9))
	  (cvec->list N X 1)))
    => '(1.+2.i 3.+4.i 5.+6.i 7.+8.i 9.+0.i))

  (check
      (with-compensations
	(let* ((N 5) (X (cvec/c N)))
	  (cvec-set! X 1 2 5+4i)
	  (cvec-ref  X 1 2)))
    => 5.+4.i)

  (check
      (with-compensations
	(let* ((N 5) (X (zvec/c N)))
	  (zvec-fill! X 1 '(1+2i 3+4i 5+6i 7+8i 9))
	  (zvec->list N X 1)))
    => '(1.+2.i 3.+4.i 5.+6.i 7.+8.i 9.+0.i))

  (check
      (with-compensations
	(let* ((N 5) (X (zvec/c N)))
	  (zvec-set! X 1 2 5+4i)
	  (zvec-ref  X 1 2)))
    => 5.+4.i)

  #t)


(parametrise ((check-test-name	'real-matrices))

  (check
      (with-compensations
	(let* ((M	2)
	       (N	3)
	       (lda	M)
	       (A	(smat/c M N)))
	  (smat-fill! A lda '((1 2 3)
			   (4 5 6)))
	  (smat->list M N A lda)))
    => '((1. 2. 3.)
	 (4. 5. 6.)))

  (check
      (with-compensations
	(let* ((M	2)
	       (N	3)
	       (lda	M)
	       (A	(smat/c M N)))
	  (smat-set! A lda 1 2 5)
	  (smat-ref A lda 1 2)))
    => 5.)

  (check
      (with-compensations
	(let* ((M	2)
	       (N	3)
	       (lda	M)
	       (A	(dmat/c M N)))
	  (dmat-fill! A lda '((1 2 3)
			   (4 5 6)))
	  (dmat->list M N A lda)))
    => '((1. 2. 3.)
	 (4. 5. 6.)))

  (check
      (with-compensations
	(let* ((M	2)
	       (N 3)
	       (lda	M)
	       (A	(dmat/c M N)))
	  (dmat-set! A lda 1 2 5)
	  (dmat-ref A lda 1 2)))
    => 5.)

  #t)


(parametrise ((check-test-name	'complex-matrices))

  (check
      (with-compensations
	(let* ((M	2)
	       (N	3)
	       (lda	M)
	       (A	(cmat/c M N)))
	  (cmat-fill! A lda '((1+2i 3+4i 5+6i)
			      (7+8i 9    10)))
	  (cmat->list M N A lda)))
    => '((1.+2.i 3.+4.i 5.+6.i)
	 (7.+8.i 9.+0.i 10.+0.i)))

  (check
      (with-compensations
	(let* ((M	2)
	       (N	3)
	       (lda	M)
	       (A	(cmat/c M N)))
	  (cmat-set! A lda 1 2 5)
	  (cmat-ref  A lda 1 2)))
    => 5.+0.i)

  (check
      (with-compensations
	(let* ((M	2)
	       (N	3)
	       (lda	M)
	       (A	(zmat/c M N)))
	  (zmat-fill! A lda '((1+2i 3+4i 5+6i)
			      (7+8i 9    10)))
	  (zmat->list M N A lda)))
    => '((1.+2.i 3.+4.i 5.+6.i)
	 (7.+8.i 9.+0.i 10.+0.i)))

  (check
      (with-compensations
	(let* ((M	2)
	       (N	3)
	       (lda	M)
	       (A	(zmat/c M N)))
	  (zmat-set! A lda 1 2 5)
	  (zmat-ref  A lda 1 2)))
    => 5.+0.i)

  #t)


;;;; done

(check-report)

;;; end of file
