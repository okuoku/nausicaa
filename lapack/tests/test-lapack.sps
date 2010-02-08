;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/LAPACK
;;;Contents: test high-level API
;;;Date: Thu Feb  4, 2010
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
  (begin0)
  (lists)
  (pretty-print)
  (compensations)
  (only (foreign ffi)
	array-set-c-double!
	array-ref-c-double)
  (only (foreign ffi sizeof)
	strideof-double
	strideof-int)
  (foreign memory)
  (foreign math lapack)
  (foreign math lapack vm)

  (foreign math blas)

  (checks))

(check-set-mode! 'report-failed)
(display "*** testing lapack\n")


;;;; helpers

(define (&char val)
  (begin0-let ((p (malloc-small/c)))
    (pointer-set-c-signed-char! p 0 (char->integer val))))

(define (&int val)
  (begin0-let ((p (malloc-small/c)))
    (pointer-set-c-integer! p 0 val)))

(define (double=? ell1 ell2)
  (list=? (lambda (a b)
	    (< (abs (- a b)) 1e-6))
	  ell1 ell2))

(define (complex=? ell1 ell2)
  (list=? (lambda (a b)
	    (< (magnitude (- a b)) 1e-6))
	  ell1 ell2))


(parametrise ((check-test-name	'linear-equations))

  (with-compensations
    (let* ((n	4)
	   (A	(rmx/c n n))
	   (B	(rvc/c n))
	   (piv	(piv/c n)))

      (define A-coeffs
	'(( 1.80   2.88   2.05  -0.89)
	  ( 5.25  -2.95  -0.95  -3.80)
	  ( 1.58  -2.69  -2.90  -1.04)
	  (-1.11  -0.66  -0.59   0.80)))

      (define B-coeffs
	'(9.52  24.35  0.77  -6.22))

      (rmx-fill! A n A-coeffs)
      (rvc-fill! B B-coeffs)

      (dgesv n 1 A n piv B n)

      (check
	  (rvc->list B n)
	(=> double=?)
	'(1. -1. 3. -5.))

      (check
	  (piv->list piv n)
	=> '(2 2 3 4))

;;;      (pretty-print (rmx->list A n n n))

      (let ((Y (rvc/c n)))
	(rmx-fill! A n A-coeffs)
	(dgemv col-major no-trans n n 1. A n B 1 0. Y 1)
	(check
	    (rvc->list Y n)
	  (=> double=?)
	  B-coeffs))

      #f))

;;; --------------------------------------------------------------------

  (with-compensations
    (let* ((n	4)
	   (A	(cmx/c n n))
	   (B	(cvc/c n))
	   (piv	(piv/c n)))

      (define A-coeffs
	'((-1.34+2.55i   0.28+3.17i  -6.39-2.20i   0.72-0.92i)
	  (-0.17-1.41i   3.31-0.15i  -0.15+1.34i   1.29+1.38i)
	  (-3.29-2.39i  -1.91+4.42i  -0.14-1.35i   1.72+1.35i)
	  ( 2.41+0.39i  -0.56+1.47i  -0.83-0.69i  -1.96+0.67i)))

      (define B-coeffs
	'(26.26+51.78i 6.43-8.68i -5.75+25.31i 1.16+2.57i))

      (cmx-fill! A n A-coeffs)
      (cvc-fill! B B-coeffs)

      (zgesv n 1 A n piv B n)

      (check
	  (cvc->list B n)
	(=> complex=?)
	'(1.0000+1.0000i 2.0000-3.0000i -4.0000-5.0000i -0.0000+6.0000i))

      (check
	  (piv->list piv n)
	=> '(3 2 3 4))

      ;; (let ((Y (cvc/c n)))
      ;; 	(cmx-fill! A n A-coeffs)
      ;; 	(zgemv col-major no-trans n n 1.+0.i A n B 1 0. Y 1)
      ;; 	(check
      ;; 	    (rvc->list Y n)
      ;; 	  (=> complex=?)
      ;; 	  B-coeffs))

      #f))

;;; --------------------------------------------------------------------

  #t)


;;;; done

(check-report)

;;; end of file
