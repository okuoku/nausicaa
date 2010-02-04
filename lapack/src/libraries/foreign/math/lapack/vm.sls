;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/LAPACK
;;;Contents: simple vector and matrix library
;;;Date: Thu Feb  4, 2010
;;;
;;;Abstract
;;;
;;;	This  library supports  only  vectors and  matrices of  "double"
;;;	numbers.
;;;
;;;	  We have to acknowledge that CLAPACK follows the conventions of
;;;	LAPACK, which is  written in Fortran 77: matrices  are stored in
;;;	column-major order.  So the matrix:
;;;
;;;		 -              -
;;;		| a_11 a_12 a_13 |
;;;		| a_21 a_22 a_23 |
;;;		| a_31 a_32 a_33 |
;;;		 -              -
;;;
;;;	is stored in an array as:
;;;
;;;	   double matrix[3*3];
;;;
;;;	   matrix[0] = a_11; matrix[3] = a_21; matrix[6] = a_13;
;;;	   matrix[1] = a_21; matrix[4] = a_22; matrix[7] = a_23;
;;;	   matrix[2] = a_31; matrix[5] = a_32; matrix[8] = a_33;
;;;
;;;	this library follows this convention, too.
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


(library (foreign math lapack vm)
  (export

    ;; real matrices of "double"
    rmx/c
    rmx-set!		rmx-ref
    rmx-fill!

    ;; complex matrices of "double"
    cmx/c
    cmx-set!		cmx-ref
    cmx-fill!

    ;; real vectors of "double"
    rvc/c
    rvc-set!		rvc-ref
    rvc-fill!

    ;; complex vectors of "double"
    cvc/c
    cvc-set!		cvc-ref
    cvc-fill!)
  (import (rnrs)
    (begin0)
    (only (foreign ffi)
	  array-set-c-double!
	  array-ref-c-double)
    (only (foreign ffi sizeof)
	  strideof-double)
    (only (foreign memory)
	  malloc-block/c)
    (foreign math lapack sizeof))


;;;; real matrices of "double"

(define (rmx/c rows cols)
  ;;Allocate an array capable of holding  a real matrix of ROWS rows and
  ;;COLS columns; return a pointer to it.  The appropriate free function
  ;;is pushed to the current compensations stack.
  ;;
  (malloc-block/c (* strideof-double rows cols)))

(define (rmx-set! rmx col-len row col val)
  ;;Store the  real value VAL  at location ROW,  COL in the  real matrix
  ;;referenced by the pointer object RMX and column length COL-LEN.
  ;;
  (array-set-c-double! rmx (+ row (* col-len col)) (inexact val)))

(define (rmx-ref rmx col-len row col)
  ;;Return  the real  value  at location  ROW,  COL in  the real  matrix
  ;;referenced by the pointer object RMX and column length COL-LEN.
  ;;
  (array-ref-c-double rmx (+ row (* col-len col))))

(define (rmx-fill! rmx col-len m)
  ;;Fill the real matrix referenced by the pointer object RMX and column
  ;;length COL-LEN, with  values from the list M.  M is  a list of lists
  ;;in row-major order:
  ;;
  ;;	((a_11 a_12 a_13)
  ;;	 (a_21 a_22 a_23)
  ;;	 (a_31 a_32 a_33))
  ;;
  (let rows ((m m) (i 0))
    (unless (null? m)
      (let cols ((n (car m)) (j 0))
	(unless (null? n)
	  (rmx-set! rmx col-len i j (car n))
	  (cols (cdr n) (+ 1 j))))
      (rows (cdr m) (+ 1 i)))))


;;;; complex matrices of "double"

(define (cmx/c rows cols)
  ;;Allocate an array  capable of holding a complex  matrix of ROWS rows
  ;;and  COLS columns;  return a  pointer to  it.  The  appropriate free
  ;;function is pushed to the current compensations stack.
  ;;
  (malloc-block/c (* 2 strideof-double rows cols)))

(define (cmx-set! cmx col-len row col val)
  ;;Store  the complex value  VAL at  location ROW,  COL in  the complex
  ;;matrix  referenced  by the  pointer  object  CMX  and column  length
  ;;COL-LEN.
  ;;
  (let ((offset (* 2 (+ row (* col-len col))))
	(val    (inexact val)))
    (array-set-c-double! cmx offset (real-part val))
    (array-set-c-double! cmx (+ 1 offset) (imag-part val))))

(define (cmx-ref cmx col-len row col)
  ;;Return the complex value at  location ROW, COL in the complex matrix
  ;;referenced by the pointer object CMX and column length COL-LEN.
  ;;
  (let ((offset (* 2 (+ row (* col-len col)))))
    (make-rectangular (array-ref-c-double cmx offset)
		      (array-ref-c-double cmx (+ 1 offset)))))

(define (cmx-fill! cmx col-len m)
  ;;Fill the  complex matrix  referenced by the  pointer object  CMX and
  ;;column length COL-LEN, with values from  the list M.  M is a list of
  ;;lists in row-major order:
  ;;
  ;;	((a_11 a_12 a_13)
  ;;	 (a_21 a_22 a_23)
  ;;	 (a_31 a_32 a_33))
  ;;
  (let rows ((m m) (i 0))
    (unless (null? m)
      (let cols ((n (car m)) (j 0))
	(unless (null? n)
	  (cmx-set! cmx col-len i j (car n))
	  (cols (cdr n) (+ 1 j))))
      (rows (cdr m) (+ 1 i)))))


;;;; real vectors of "double"

(define (rvc/c len)
  ;;Allocate an array capable of  holding a real vector of LEN elements;
  ;;return a pointer to it.   The appropriate free function is pushed to
  ;;the current compensations stack.
  ;;
  (malloc-block/c (* strideof-double len)))

(define (rvc-set! rvc idx val)
  ;;Store  the  real  value VAL  at  location  IDX  in the  real  vector
  ;;referenced by the pointer object RVC.
  ;;
  (array-set-c-double! rvc idx (inexact val)))

(define (rvc-ref rvc idx)
  ;;Return the real value at  location IDX in the real vector referenced
  ;;by the pointer object RVC.
  ;;
  (array-ref-c-double rvc idx))

(define (rvc-fill! rvc m)
  ;;Fill  the real  vector referenced  by  the pointer  object RVC  with
  ;;values from the list M.
  ;;
  (let elms ((m m) (i 0))
    (unless (null? m)
      (rvc-set! rvc i (car m))
      (elms (cdr m) (+ 1 i)))))


;;;; complex vectors of "double"

(define (cvc/c len)
  ;;Allocate  an  array capable  of  holding  a  complex vector  of  LEN
  ;;elements; return a pointer to  it.  The appropriate free function is
  ;;pushed to the current compensations stack.
  ;;
  (malloc-block/c (* 2 strideof-double len)))

(define (cvc-set! cvc idx val)
  ;;Store the  complex value VAL at  location IDX in  the complex vector
  ;;referenced by the pointer object CVC.
  ;;
  (let ((offset (* 2 idx)))
    (array-set-c-double! cvc offset val)
    (array-set-c-double! cvc (+ 1 offset) val)))

(define (cvc-ref cvc idx)
  ;;Return  the complex  value at  location  IDX in  the complex  vector
  ;;referenced by the pointer object CVC.
  ;;
  (let ((offset (* 2 idx)))
    (make-rectangular (array-set-c-double! cvc offset)
		      (array-set-c-double! cvc (+ 1 offset)))))

(define (cvc-fill! cvc m)
  ;;Fill the  complex vector referenced  by the pointer object  CVC with
  ;;values from the list M.
  ;;
  (let elms ((m m) (i 0))
    (unless (null? m)
      (cvc-set! cvc i (car m))
      (elms (cdr m) (+ 1 i)))))


;;;; done

)

;;; end of file
