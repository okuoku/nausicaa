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
  (foreign ffi)
  (prefix (foreign math blas) blas:)
  (prefix (foreign math blas vm) vm:)
  (lists)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing BLAS platform\n")

(define (eqf? a b)
  (< (magnitude (- a b)) 1e-3))

(define (eql? ell1 ell2)
  (list=? (lambda (a b)
	    (if (list? a)
		(eql? a b)
	      (eqf? a b)))
	  ell1 ell2))

(define (sqr A)
  (* A A))


(parametrise ((check-test-name	'single-level1))

  (check
      (with-compensations
	(let* ((N	3)
	       (SX	(vm:svec/c N))
	       (SY	(vm:svec/c N)))
	  (vm:svec-fill! SX 1 '(1. 2. 3.))
	  (vm:svec-fill! SY 1 '(4. 5. 6.))
	  (blas:sdot N SX 1 SY 1)))
    (=> eqf?)
    32.)

  (check
      (with-compensations
	(let* ((N	3)
	       (SX	(vm:svec/c N))
	       (SY	(vm:svec/c N)))
	  (vm:svec-fill! SX 1 '(1. 2. 3.))
	  (vm:svec-fill! SY 1 '(4. 5. 6.))
	  (blas:sdot N SX 1 SY -1)))
    (=> eqf?)
    28.)

;;; --------------------------------------------------------------------

  ;; (check
  ;;     (with-compensations
  ;; 	(let* ((N	3)
  ;; 	       (SB	1000.)
  ;; 	       (SX	(vm:svec/c N))
  ;; 	       (SY	(vm:svec/c N)))
  ;; 	  (vm:svec-fill! SX 1 '(1. 2. 3.))
  ;; 	  (vm:svec-fill! SY 1 '(4. 5. 6.))
  ;; 	  (blas:sdsdot 1 SB SX 1 SY 1)))
  ;;   (=> eqf?)
  ;;   1032.)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(let* ((N	3)
	       (SX	(vm:svec/c N)))
	  (vm:svec-fill! SX 1 '(1. 2. 3.))
	  (blas:snrm2 N SX 1)))
    (=> eqf?)
    (sqrt 14.))

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(let* ((N	3)
	       (SX	(vm:svec/c N)))
	  (vm:svec-fill! SX 1 '(1. -2. 3.))
	  (blas:sasum N SX 1)))
    (=> eqf?)
    6.)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(let* ((N	3)
	       (SX	(vm:svec/c N)))
	  (vm:svec-fill! SX 1 '(1. 3. -2.))
	  (blas:isamax N SX 1)))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(let* ((N	3)
	       (SX	(vm:svec/c N))
	       (SY	(vm:svec/c N)))
	  (vm:svec-fill! SX 1 '(1. 2. 3.))
	  (vm:svec-fill! SY 1 '(4. 5. 6.))
	  (blas:sswap N SX 1 SY 1)
	  (list (vm:svec->list N SX 1)
		(vm:svec->list N SY 1))))
    => '((4. 5. 6.)
	 (1. 2. 3.)))

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(let* ((N	3)
	       (SX	(vm:svec/c N))
	       (SY	(vm:svec/c N)))
	  (vm:svec-fill! SX 1 '(1. 2. 3.))
	  (vm:svec-fill! SY 1 '(4. 5. 6.))
	  (blas:scopy N SX 1 SY 1)
	  (list (vm:svec->list N SX 1)
		(vm:svec->list N SY 1))))
    => '((1. 2. 3.)
	 (1. 2. 3.)))

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(let* ((N	3)
	       (SX	(vm:svec/c N))
	       (SY	(vm:svec/c N)))
	  (vm:svec-fill! SX 1 '(1. 2. 3.))
	  (vm:svec-fill! SY 1 '(4. 5. 6.))
	  (blas:saxpy N 10. SX 1 SY 1)
	  (list (vm:svec->list N SX 1)
		(vm:svec->list N SY 1))))
    => `((1. 2. 3.)
	 ,(list (+ 4. (* 10. 1.))
		(+ 5. (* 10. 2.))
		(+ 6. (* 10. 3.)))))

;;; --------------------------------------------------------------------

  (let* ((SA	2.)
	 (SB	3.)
	 (SR	(sqrt (+ (sqr SA) (sqr SB)))))
    (check
	(receive (SC SS)
	    (blas:srotg SA SB)
	  (list (+ (* SC SA)     (* SS SB))
		(+ (* (- SS) SA) (* SC SB))))
      (=> eql?)
      `(,SR 0.)))

;;; --------------------------------------------------------------------

  (let* ((SA	2.)
	 (SB	3.)
	 (SR	(sqrt (+ (sqr SA) (sqr SB)))))
    (check
	(receive (SC SS)
	    (blas:srotg SA SB)
	  (with-compensations
	    (let* ((N	2)
		   (SX	(vm:svec/c N))
		   (SY	(vm:svec/c N)))
	      (vm:svec-fill! SX 1 (list SA SA))
	      (vm:svec-fill! SY 1 (list SB SB))
	      (blas:srot N SX 1 SY 1 SC SS)
	      (list (vm:svec->list N SX 1)
		    (vm:svec->list N SY 1)))))
      (=> eql?)
      `((,SR ,SR) (0. 0.))))

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(let ((SH	(vm:svec/c 4))
	      (SA	.2)
	      (SB	.3)
	      (SC	.4)
	      (SD	.4))
	  (blas:srotmg SA SB SC SD 1 SH)
	  (let* ((SH11 (pointer-ref-c-float SH 0))
		 (SH21 (pointer-ref-c-float SH 1))
		 (SH12 (pointer-ref-c-float SH 2))
		 (SH22 (pointer-ref-c-float SH 3))
		 (A	(* (sqrt SA) SC))
		 (B	(* (sqrt SB) SD)))
	    ;;(+ (* SH11 a) (* SH12 B)) ;; -> R
	    (+ (* SH21 A) (* SH22 B)))))
    (=> eqf?)
    0.)

  #t)


;;;; done

(check-report)

;;; end of file
