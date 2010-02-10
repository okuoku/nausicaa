;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/LAPACK
;;;Contents: test loading of platform library
;;;Date: Tue Feb  2, 2010
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
  (compensations)
  (only (foreign ffi)
	array-set-c-double!
	array-ref-c-double)
  (only (foreign ffi sizeof)
	strideof-double)
  (foreign memory)
  (foreign math lapack platform)
  (foreign math lapack sizeof)
  (foreign math lapack vm)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing lapack platform\n")


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
	      (< (abs (- a b)) 1e-3)))
	  ell1 ell2))


(parametrise ((check-test-name	'svd))

  (with-compensations
    ;;This is the example program from the FAQ at:
    ;;
    ;;   <http://www.netlib.org/clapack/faq.html>
    ;;
    (let* ((M		6)
	   (N		4)
	   (LDA*	(&int M))
	   (LDU*	(&int M))
	   (LDVT*	(&int N))
	   (a		(rmx/c M N))
	   (s		(rvc/c N))
	   (wk		(rvc/c 4096))
	   (u		(rmx/c M M))
	   (vt		(rmx/c N N))
	   (JOBU*	(&char #\A))
	   (JOBVT*	(&char #\A))
	   (LWORK*	(&int 4096))
	   (INFO*	(&int 0)))

      (rmx-fill! a M '(( 2.27  -1.54   1.15  -1.94)
		       ( 0.28  -1.67   0.94  -0.78)
		       (-0.48  -3.09   0.99  -0.21)
		       ( 1.07   1.22   0.79   0.63)
		       (-2.35   2.93  -1.45   2.30)
		       ( 0.62  -7.39   1.03  -2.57)))

      (dgesvd_ JOBU* JOBVT* (&int M) (&int N)
	       a LDA* s u LDU* vt LDVT* wk LWORK* INFO*)

      (check
	  (pointer-ref-c-integer INFO* 0)
	=> 0)

      (check
	  (rvc->list s N)
	(=> double=?)
	'(9.9966  3.6831  1.3569  0.5000))

      (check
      	  (rmx->list a M M N)
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


;;;; done

(check-report)

;;; end of file
