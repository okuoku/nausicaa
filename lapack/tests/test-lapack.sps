;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/LAPACK
;;;Contents: test high-level API
;;;Date: Thu Feb  4, 2010
;;;
;;;Abstract
;;;
;;;	The test values are from:
;;;
;;;		<http://www.nag.co.uk/lapack-ex/lapack-ex.html>
;;;
;;;	or generated with GNU Octave.
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
;;;  (foreign math blas)
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
	    (if (list? a)
		(double=? a b)
	      (< (magnitude (- a b)) 1e-3)))
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

      ;;To use this load the BLAS library.
      ;;
      ;; (let ((Y (rvc/c n)))
      ;; 	(rmx-fill! A n A-coeffs)
      ;; 	(dgemv col-major no-trans n n 1. A n B 1 0. Y 1)
      ;; 	(check
      ;; 	    (rvc->list Y n)
      ;; 	  (=> double=?)
      ;; 	  B-coeffs))

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
	(=> double=?)
	'(1.0000+1.0000i 2.0000-3.0000i -4.0000-5.0000i -0.0000+6.0000i))

      (check
	  (piv->list piv n)
	=> '(3 2 3 4))

      ;; (let ((Y (cvc/c n)))
      ;; 	(cmx-fill! A n A-coeffs)
      ;; 	(zgemv col-major no-trans n n 1.+0.i A n B 1 0. Y 1)
      ;; 	(check
      ;; 	    (rvc->list Y n)
      ;; 	  (=> double=?)
      ;; 	  B-coeffs))

      #f))

;;; --------------------------------------------------------------------

  #t)


(parametrise ((check-test-name	'singular-value-decomposition))

  (with-compensations
    ;;This is the example program from the FAQ at:
    ;;
    ;;   <http://www.netlib.org/clapack/faq.html>
    ;;
    (let* ((N		4)
	   (M		6)
	   (a		(rmx/c M N))
	   (s		(rvc/c N))
	   (lwork	4096)
	   (wk		(rvc/c lwork))
	   (u		(rmx/c M M))
	   (vt		(rmx/c N N)))

      (rmx-fill! a M '(( 2.27  -1.54   1.15  -1.94)
		       ( 0.28  -1.67   0.94  -0.78)
		       (-0.48  -3.09   0.99  -0.21)
		       ( 1.07   1.22   0.79   0.63)
		       (-2.35   2.93  -1.45   2.30)
		       ( 0.62  -7.39   1.03  -2.57)))

      (dgesvd #\A #\A M N a M s u M vt N wk lwork)

      (check
      	  (rvc->list s N)
      	(=> double=?)
	'(9.9966  3.6831  1.3569  0.5000))

      (check
      	  (rmx->list u M M N)
      	(=> double=?)
	'((-0.2774 -0.6003 -0.1277 +0.1323)
	  (-0.2020 -0.0301 +0.2805 +0.7034)
	  (-0.2918 +0.3348 +0.6453 +0.1906)
	  (+0.0938 -0.3699 +0.6781 -0.5399)
	  (+0.4213 +0.5266 +0.0413 -0.0575)
	  (-0.7816 +0.3353 -0.1645 -0.3957)))

      (check
      	  (rmx->list vt N N N)
      	(=> double=?)
	'((-0.1921 +0.8794 -0.2140 +0.3795)
	  (-0.8030 -0.3926 -0.2980 +0.3351)
	  (+0.0041 -0.0752 +0.7827 +0.6178)
	  (-0.5642 +0.2587 +0.5027 -0.6017)))

      #f))

  #t)


(parametrise ((check-test-name	'factorisations))

  (with-compensations

    (let* ((M	6)
	   (N	4)
	   (A	(rmx/c M N))
	   (piv	(piv/c N)))

      (rmx-fill! A M '(( 2.27  -1.54   1.15  -1.94)
		       ( 0.28  -1.67   0.94  -0.78)
		       (-0.48  -3.09   0.99  -0.21)
		       ( 1.07   1.22   0.79   0.63)
		       (-2.35   2.93  -1.45   2.30)
		       ( 0.62  -7.39   1.03  -2.57)))

      (dgetrf M N A M piv)

      (check
	  (rmx->list A M M N)
	(=> double=?)
	'((-2.35        2.93       -1.45         2.3)
	  (-0.26383      -6.617     0.64745     -1.9632)
	  (+0.20426     0.55742     0.92527     0.41454)
	  (-0.45532    -0.38599     0.41036     0.74935)
	  (-0.96596    -0.19499    -0.13444    -0.06055)
	  (-0.11915     0.19962     0.68952    -0.53366)))


      (check
	  (piv->list piv N)
	=> '(5 6 3 4))

      #f))

;;; --------------------------------------------------------------------

  (with-compensations

    (let* ((M	4)
	   (N	4)
	   (A	(cmx/c M N))
	   (piv	(piv/c N)))

      (cmx-fill! A M '((-1.34+2.55i   0.28+3.17i  -6.39-2.20i   0.72-0.92i)
		       (-0.17-1.41i   3.31-0.15i  -0.15+1.34i   1.29+1.38i)
		       (-3.29-2.39i  -1.91+4.42i  -0.14-1.35i   1.72+1.35i)
		       ( 2.41+0.39i  -0.56+1.47i  -0.83-0.69i  -1.96+0.67i)))

      (zgetrf M N A M piv)

      (check
	  (cmx->list A M M N)
	(=> double=?)
	'((-3.29-2.39i	     -1.91+4.42i       -0.14-1.35i         1.72+1.35i)
	  ( 0.23761+0.25596i  4.8952-0.71136i  -0.46228+1.6966i    1.2269+0.61897i)
	  (-0.10195-0.70101i -0.66915+0.36887i -5.1414-1.13i       0.99826+0.38502i)
	  (-0.53585+0.27073i -0.20402+0.86012i  0.008233+0.12106i  0.14824-0.12522i)))

      (check
	  (piv->list piv N)
	=> '(3 2 3 4))

      #f))

  #t)


(parametrise ((check-test-name	'inverse))

  (with-compensations
    (let* ((N	4)
	   (A	(rmx/c N N))
	   (piv	(piv/c N)))

      (rmx-fill! A N '(( 2.27  -1.54   1.15  -1.94)
		       ( 0.28  -1.67   0.94  -0.78)
		       (-0.48  -3.09   0.99  -0.21)
		       ( 1.07   1.22   0.79   0.63)))

      (dgetrf N N A N piv)

      (let* ((lwork	(let ((work (rvc/c 1)))
			  (dgetri N A N piv work -1)
			  (exact (pointer-ref-c-double work 0))))
	     (work	(rvc/c lwork)))

	(dgetri N A N piv work lwork)

	(check
	    (rmx->list A N N N)
	  (=> double=?)
	  '(( 0.70187 -1.7582  0.68437  0.21266)
	    (-0.31892  1.0523 -0.6987   0.087803)
	    (-0.61374  2.0798 -0.63068  0.47487)
	    ( 0.19515 -1.6596  0.98156  0.46062)))

	#f)))

;;; --------------------------------------------------------------------

  (with-compensations
    (let* ((N	4)
	   (A	(cmx/c N N))
	   (piv	(piv/c N)))

      (cmx-fill! A N '((-1.34+2.55i   0.28+3.17i  -6.39-2.20i   0.72-0.92i)
		       (-0.17-1.41i   3.31-0.15i  -0.15+1.34i   1.29+1.38i)
		       (-3.29-2.39i  -1.91+4.42i  -0.14-1.35i   1.72+1.35i)
		       ( 2.41+0.39i  -0.56+1.47i  -0.83-0.69i  -1.96+0.67i)))

      (zgetrf N N A N piv)

      (let* ((lwork	(let ((work (cvc/c 1)))
			  (zgetri N A N piv work -1)
			  (exact (pointer-ref-c-double work 0))))
	     (work	(cvc/c lwork)))

	(zgetri N A N piv work lwork)

	(check
	    (cmx->list A N N N)
	  (=> double=?)
	  '(( 0.075662-0.43236i   1.6512-3.1342i    1.2663+0.041789i   3.8181+1.1195i)
	    (-0.19415+0.079808i  -1.19-0.14264i    -0.24014-0.58887i  -0.010076-1.4969i)
	    (-0.095676-0.049102i  0.73711-0.42897i  0.32242+0.077566i  0.68875+0.7891i)
	    ( 0.37018-0.50397i    3.7253-3.1813i    1.7014+0.72673i    3.9367+3.3255i)))

	#f)))


  #t)


(parametrise ((check-test-name	'eigenproblem))

  (define (cvc-make-rectangular! w wr wi N)
    (do ((i 0 (+ 1 i)))
	((= i N))
      (let ((i2 (* 2 i)))
	(array-set-c-double! w i2       (array-ref-c-double wr i))
	(array-set-c-double! w (+ 1 i2) (array-ref-c-double wi i)))))

  (with-compensations
    (let* ((N	4)
	   (A	(rmx/c N N))
	   (wr	(rvc/c N))
	   (wi	(rvc/c N))
	   (vl	(rmx/c N N)))

      (rmx-fill! A N '(( 2.27  -1.54   1.15  -1.94)
		       ( 0.28  -1.67   0.94  -0.78)
		       (-0.48  -3.09   0.99  -0.21)
		       ( 1.07   1.22   0.79   0.63)))

      (let* ((lwork	(let ((work (rvc/c 1)))
			  (dgeev #\V #\N N A N wr wi
				 vl N pointer-null 1
				 work -1)
			  (exact (pointer-ref-c-double work 0))))
	     (work	(rvc/c lwork)))

	(dgeev #\V #\N N A N wr wi
	       vl N pointer-null 1
	       work lwork)

	(let ((w (cvc/c N)))
	  (cvc-make-rectangular! w wr wi N)

	  (check
	      (cvc->list w N)
	    (=> double=?)
	    '(0.06096+1.8083i
	      0.06096-1.8083i
	      1.049+0.50586i
	      1.049-0.50586i))

	  (check
	      (rmx->list vl N N N)
            (=> double=?)
            '((-0.19304-0.46924i -0.19304+0.46924i  0.75676+0i         0.75676-0i)
              ( 0.24056-0.26082i  0.24056+0.26082i -0.14714+0.089966i -0.14714-0.089966i)
              ( 0.58735+0i        0.58735-0i       -0.40803-0.082558i -0.40803+0.082558i)
              (-0.49998-0.14733i -0.49998+0.14733i  0.3512-0.31768i    0.3512+0.31768i)))

	  #f))))

  #t)


;;;; done

(check-report)

;;; end of file
