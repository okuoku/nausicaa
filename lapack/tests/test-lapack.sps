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
  (compensations)
  (only (foreign ffi)
	array-set-c-double!
	array-ref-c-double)
  (only (foreign ffi sizeof)
	strideof-double)
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


(parametrise ((check-test-name	'base))

  (check
      (with-compensations
	;;This is the example program from the FAQ at:
	;;
	;;   <http://www.netlib.org/clapack/faq.html>
	;;
	(let* ((size		4)
	       (M		size)
	       (N		size)
	       (LDA*		(&int M))
	       (LDU*		(&int M))
	       (LDVT*		(&int N))
	       (a		(rmx/c size size))
	       (s		(rvc/c size))
	       (wk		(rvc/c 201))
	       (u		(rmx/c size size))
	       (vt		(rmx/c size size))
	       (JOBU*		(&char #\A))
	       (JOBVT*		(&char #\A))
	       (LWORK*		(&int 201)))

	  (rmx-fill! a 4 '((16.  5.  9.  4.)
			   (2.  11.  7. 14.)
			   (3.  10.  6. 15.)
			   (13.  8. 12.  1.)))


	  ;; int dgesvd_(char *jobu, char *jobvt, integer *m, integer *n,
	  ;;             doublereal *a, integer *lda,
	  ;;		 doublereal *s, doublereal *u, integer *ldu,
	  ;;		 doublereal *vt, integer *ldvt,
	  ;;		 doublereal *work, integer *lwork, integer *info)
	  (dgesvd JOBU* JOBVT* (&int M) (&int N)
		  a LDA* s u LDU* vt LDVT* wk LWORK*)

	  (do ((i 0 (+ 1 i)))
	      ((= i size))
	    (display (rvc-ref s i))
	    (newline))
	  #t))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
