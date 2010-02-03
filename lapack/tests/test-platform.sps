;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/LAPACK
;;;Contents: test loading of platform library
;;;Date: Tue Feb  2, 2010
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
  (foreign math lapack platform)
  (foreign math lapack sizeof)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing lapack platform\n")


;;;; helpers

(define (rmx/c rows cols)
  (malloc-block/c (* strideof-double rows cols)))

(define (rmx-set! rmx row col val)
  (array-set-c-double! rmx (+ col (* row col)) val))

(define (rmx-ref rmx row col)
  (array-ref-c-double rmx (+ col (* row col))))

(define (rmx-fill! rmx m)
  (let rows ((m m) (i 0))
    (unless (null? m)
      (let cols ((n (car m)) (j 0))
	(unless (null? n)
	  (rmx-set! rmx i j (car n))
	  (cols (cdr n) (+ 1 j))))
      (rows (cdr m) (+ 1 i)))))

;;; --------------------------------------------------------------------

(define (rvc/c len)
  (malloc-block/c (* strideof-double len)))

(define (rvc-set! rvc idx val)
  (array-set-c-double! rvc idx val))

(define (rvc-ref rvc idx)
  (array-ref-c-double rvc idx))

(define (vrt-fill! rvc m)
  (let elms ((m m) (i 0))
    (unless (null? m)
      (rvc-set! rvc i (car m))
      (elms (cdr m) (+ 1 i)))))

;;; --------------------------------------------------------------------

(define (&char val)
  (begin0-let ((p (malloc-small/c)))
    (pointer-set-c-signed-char! p 0 val)))

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
	       (JOBU*		(&char (char->integer #\A)))
	       (JOBVT*		(&char (char->integer #\A)))
	       (LWORK*		(&int 201))
	       (INFO*		(&int 0)))

	  (rmx-fill! a '((16.  5.  9.  4.)
			 (2.  11.  7. 14.)
			 (3.  10.  6. 15.)
			 (13.  8. 12.  1.)))


	  ;; int dgesvd_(char *jobu, char *jobvt, integer *m, integer *n,
	  ;;             doublereal *a, integer *lda,
	  ;;		 doublereal *s, doublereal *u, integer *ldu,
	  ;;		 doublereal *vt, integer *ldvt,
	  ;;		 doublereal *work, integer *lwork, integer *info)
	  (dgesvd_ JOBU* JOBVT* (&int M) (&int N)
		   a LDA* s u LDU* vt LDVT* wk LWORK* INFO*)

	  (do ((i 0 (+ 1 i)))
	      ((= i size))
	    (display (rvc-ref s i))
	    (newline))
	  (pointer-ref-c-integer INFO* 0)))
    => 0)

  #t)


;;;; done

(check-report)

;;; end of file
