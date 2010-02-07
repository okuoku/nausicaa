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


(parametrise ((check-test-name	'linear-equations))

  (define (pivots->list pivots* n)
    (let loop ((i 0) (ell '()))
      (if (= i n)
	  (reverse ell)
	(loop (+ 1 i) (cons (array-ref-c-integer pivots* i) ell)))))

  (with-compensations
    (let* ((n	4)
	   (A	(rmx/c 4 4))
	   (B	(rvc/c 4))
	   (pivots*	(malloc-block/c (* 4 strideof-integer))))

      (define (pivots-ref idx)
	(array-ref-c-integer pivots* idx))

      (rmx-fill! A n '((1.80   2.88   2.05  -0.89)
		       (5.25  -2.95  -0.95  -3.80)
		       (1.58  -2.69  -2.90  -1.04)
		       (-1.11  -0.66  -0.59   0.80)))
      (rvc-fill! B '(9.52  24.35   0.77  -6.22))

      (dgesv n 1 A n pivots* B n)

      (check
	  (rvc->list B n)
	(=> double=?)
	'(1. -1. 3. -5.))

      (check
	  (pivots->list pivots* n)
	=> '(2 2 3 4))

      ))


  #t)


;;;; done

(check-report)

;;; end of file
