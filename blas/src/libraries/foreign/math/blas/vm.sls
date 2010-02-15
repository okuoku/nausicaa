;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/BLAS
;;;Contents: simple vector and matrix library
;;;Date: Thu Feb  4, 2010
;;;
;;;Abstract
;;;
;;;	This  library supports  only  vectors and  matrices of  "double"
;;;	numbers.
;;;
;;;	  We have  to acknowledge that CBLAS follows  the conventions of
;;;	BLAS, which  is written  in Fortran 77:  matrices are  stored in
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


(library (foreign math blas vm)
  (export

    ;; vectors
    svec/c svec-set! svec-ref svec-fill! svec->list
    dvec/c dvec-set! dvec-ref dvec-fill! dvec->list
    cvec/c cvec-set! cvec-ref cvec-fill! cvec->list cvec-make-rectangular!
    zvec/c zvec-set! zvec-ref zvec-fill! zvec->list zvec-make-rectangular!

    ;; real matrices of "double"
    smat/c smat-set! smat-ref smat-fill! smat->list
    dmat/c dmat-set! dmat-ref dmat-fill! dmat->list
    cmat/c cmat-set! cmat-ref cmat-fill! cmat->list
    zmat/c zmat-set! zmat-ref zmat-fill! zmat->list)
  (import (rnrs)
    (begin0)
    (only (foreign ffi)
	  array-set-c-float!
	  array-ref-c-float
	  array-set-c-double!
	  array-ref-c-double)
    (only (foreign ffi sizeof)
	  strideof-float
	  strideof-double)
    (only (foreign memory)
	  malloc-block/c))


;;;; real vectors

;;Allocate  an array capable  of holding  a real  vector of  N elements;
;;return a  pointer to it.  The  appropriate free function  is pushed to
;;the current compensations stack.
(define (svec/c N)
  (malloc-block/c (* strideof-float N)))

(define (dvec/c N)
  (malloc-block/c (* strideof-double N)))

;;Store the real value VAL at location IDX in the real vector referenced
;;by the pointer object X.
(define (svec-set! X incX idx val)
  (array-set-c-float! X (* incX idx) (inexact val)))

(define (dvec-set! X incX idx val)
  (array-set-c-double! X (* incX idx) (inexact val)))

;;Return the real value at location IDX in the real vector referenced by
;;the pointer object X.
(define (svec-ref X incX idx)
  (array-ref-c-float X (* incX idx)))

(define (dvec-ref X incX idx)
  (array-ref-c-double X (* incX idx)))

;;Fill the  real vector referenced by  the pointer object  X with values
;;from the list ELL.
(define (svec-fill! X incX ell)
  (let elms ((ell ell) (i 0))
    (unless (null? ell)
      (svec-set! X incX i (car ell))
      (elms (cdr ell) (+ 1 i)))))

(define (dvec-fill! X incX ell)
  (let elms ((ell ell) (i 0))
    (unless (null? ell)
      (dvec-set! X incX i (car ell))
      (elms (cdr ell) (+ 1 i)))))

;;Build  and return  a  list holding  the  values from  the real  vector
;;referenced by the pointer X and Ngth N.
(define (svec->list N X incX)
  (let loop ((i 0) (ell '()))
    (if (= i N)
	(reverse ell)
      (loop (+ 1 i) (cons (svec-ref X incX i) ell)))))

(define (dvec->list N X incX)
  (let loop ((i 0) (ell '()))
    (if (= i N)
	(reverse ell)
      (loop (+ 1 i) (cons (dvec-ref X incX i) ell)))))


;;;; complex vectors

;;Allocate an array  capable of holding a complex  vector of N elements;
;;return a  pointer to it.  The  appropriate free function  is pushed to
;;the current compensations stack.
(define (cvec/c N)
  (malloc-block/c (* 2 strideof-float N)))

(define (zvec/c N)
  (malloc-block/c (* 2 strideof-double N)))

;;Store  the complex value  VAL at  location IDX  in the  complex vector
;;referenced by the pointer object X.
(define (cvec-set! X incX idx val)
  (let ((offset (* 2 incX idx)))
    (array-set-c-float! X offset       (inexact (real-part val)))
    (array-set-c-float! X (+ 1 offset) (inexact (imag-part val)))))

(define (zvec-set! X incX idx val)
  (let ((offset (* 2 incX idx)))
    (array-set-c-double! X offset       (inexact (real-part val)))
    (array-set-c-double! X (+ 1 offset) (inexact (imag-part val)))))

;;Return  the  complex value  at  location  IDX  in the  complex  vector
;;referenced by the pointer object X.
(define (cvec-ref X incX idx)
  (let ((offset (* 2 incX idx)))
    (make-rectangular (array-ref-c-float X offset)
		      (array-ref-c-float X (+ 1 offset)))))

(define (zvec-ref X incX idx)
  (let ((offset (* 2 incX idx)))
    (make-rectangular (array-ref-c-double X offset)
		      (array-ref-c-double X (+ 1 offset)))))

;;Fill the  complex vector referenced  by the pointer object  X with
;;values from the list M.
(define (cvec-fill! X incX m)
  (let elms ((m m) (i 0))
    (unless (null? m)
      (cvec-set! X incX i (car m))
      (elms (cdr m) (+ 1 i)))))

(define (zvec-fill! X incX m)
  (let elms ((m m) (i 0))
    (unless (null? m)
      (zvec-set! X incX i (car m))
      (elms (cdr m) (+ 1 i)))))

;;Build and  return a  list holding the  values from the  complex vector
;;referenced by the pointer X and length N.
(define (cvec->list N X incX)
  (let loop ((i 0) (ell '()))
    (if (= i N)
	(reverse ell)
      (loop (+ 1 i) (cons (cvec-ref X incX i) ell)))))

(define (zvec->list N X incX)
  (let loop ((i 0) (ell '()))
    (if (= i N)
	(reverse ell)
      (loop (+ 1 i) (cons (zvec-ref X incX i) ell)))))

;;Store in X, a vector of  complex nubmers, the vectors R and I holding,
;;respectively, the real parts and the imaginary parts.  N is the length
;;of the vectors.
(define (cvec-make-rectangular! N X incX R incR I incI)
  (do ((i 0 (+ 1 i)))
      ((= i N))
    (cvec-set! X incX i (svec-ref R incR i))
    (cvec-set! X incX i (svec-ref I incI i))))

(define (zvec-make-rectangular! N X incX R incR I incI)
  (do ((i 0 (+ 1 i)))
      ((= i N))
    (zvec-set! X incX i (dvec-ref R incR i))
    (zvec-set! X incX i (dvec-ref I incI i))))


;;;; real matrices

;;Allocate an  array capable of  holding a real  matrix of M rows  and N
;;columns; return  a pointer  to it.  The  appropriate free  function is
;;pushed to the current compensations stack.
(define (smat/c M N)
  (malloc-block/c (* strideof-float M N)))

(define (dmat/c M N)
  (malloc-block/c (* strideof-double M N)))

;;Store  the real  value VAL  at location  ROW, COL  in the  real matrix
;;referenced by the pointer object A and column length LDA.
(define (smat-set! A lda row col val)
  (array-set-c-float! A (+ row (* lda col)) (inexact val)))

(define (dmat-set! A lda row col val)
  (array-set-c-double! A (+ row (* lda col)) (inexact val)))

;;Return  the  real  value at  location  ROW,  COL  in the  real  matrix
;;referenced by the pointer object A and column length LDA.
(define (smat-ref A lda row col)
  (array-ref-c-float A (+ row (* lda col))))

(define (dmat-ref A lda row col)
  (array-ref-c-double A (+ row (* lda col))))

;;Fill the  real matrix  referenced by the  pointer object A  and column
;;length LDA, with values from the list  ELL.  ELL is a list of lists in
;;row-major order:
;;
;;	((a_11 a_12 a_13)
;;	 (a_21 a_22 a_23)
;;	 (a_31 a_32 a_33))
;;
(define (smat-fill! A lda ell)
  (let rows ((ell ell) (i 0))
    (unless (null? ell)
      (let cols ((n (car ell)) (j 0))
	(unless (null? n)
	  (smat-set! A lda i j (car n))
	  (cols (cdr n) (+ 1 j))))
      (rows (cdr ell) (+ 1 i)))))

(define (dmat-fill! A lda ell)
  (let rows ((ell ell) (i 0))
    (unless (null? ell)
      (let cols ((n (car ell)) (j 0))
	(unless (null? n)
	  (dmat-set! A lda i j (car n))
	  (cols (cdr n) (+ 1 j))))
      (rows (cdr ell) (+ 1 i)))))

;;Build  and return a  list of  lists holding  the values,  in row-major
;;order, from the matrix referenced by the pointer A.  LDA is the length
;;of a column.  M and N are the numbers of rows and columns.
(define (smat->list M N A lda)
  (let loop-rows ((rows '()) (i 0))
    (if (= i M)
	(reverse rows)
      (let loop-cols ((cols '()) (j 0))
	(if (= j N)
	    (loop-rows (cons (reverse cols) rows) (+ 1 i))
	  (loop-cols (cons (smat-ref A lda i j) cols) (+ 1 j)))))))

(define (dmat->list M N A lda)
  (let loop-rows ((rows '()) (i 0))
    (if (= i M)
	(reverse rows)
      (let loop-cols ((cols '()) (j 0))
	(if (= j N)
	    (loop-rows (cons (reverse cols) rows) (+ 1 i))
	  (loop-cols (cons (dmat-ref A lda i j) cols) (+ 1 j)))))))


;;;; complex matrices

;;Allocate an array capable of holding  a complex matrix of M rows and N
;;columns; return  a pointer  to it.  The  appropriate free  function is
;;pushed to the current compensations stack.
(define (cmat/c M N)
  (malloc-block/c (* 2 strideof-float M N)))

(define (zmat/c M N)
  (malloc-block/c (* 2 strideof-double M N)))

;;Store the complex value VAL at location ROW, COL in the complex matrix
;;referenced by the pointer object A and column length LDA.
(define (cmat-set! A lda row col val)
  (let ((offset (* 2 (+ row (* lda col))))
	(val    (inexact val)))
    (array-set-c-float! A offset       (inexact (real-part val)))
    (array-set-c-float! A (+ 1 offset) (inexact (imag-part val)))))

(define (zmat-set! A lda row col val)
  (let ((offset (* 2 (+ row (* lda col))))
	(val    (inexact val)))
    (array-set-c-double! A offset       (inexact (real-part val)))
    (array-set-c-double! A (+ 1 offset) (inexact (imag-part val)))))

;;Return the  complex value at location  ROW, COL in  the complex matrix
;;referenced by the pointer object A and column length LDA.
(define (cmat-ref A lda row col)
  (let ((offset (* 2 (+ row (* lda col)))))
    (make-rectangular (array-ref-c-float A offset)
		      (array-ref-c-float A (+ 1 offset)))))

(define (zmat-ref A lda row col)
  (let ((offset (* 2 (+ row (* lda col)))))
    (make-rectangular (array-ref-c-double A offset)
		      (array-ref-c-double A (+ 1 offset)))))

;;Fill the complex matrix referenced  by the pointer object A and column
;;length LDA, with values from the list  ELL.  ELL is a list of lists in
;;row-major order:
;;
;;	((a_11 a_12 a_13)
;;	 (a_21 a_22 a_23)
;;	 (a_31 a_32 a_33))
;;
(define (cmat-fill! A lda ell)
  (let rows ((ell ell) (i 0))
    (unless (null? ell)
      (let cols ((n (car ell)) (j 0))
	(unless (null? n)
	  (cmat-set! A lda i j (car n))
	  (cols (cdr n) (+ 1 j))))
      (rows (cdr ell) (+ 1 i)))))

(define (zmat-fill! A lda ell)
  (let rows ((ell ell) (i 0))
    (unless (null? ell)
      (let cols ((n (car ell)) (j 0))
	(unless (null? n)
	  (zmat-set! A lda i j (car n))
	  (cols (cdr n) (+ 1 j))))
      (rows (cdr ell) (+ 1 i)))))

;;Build  and return a  list of  lists holding  the values,  in row-major
;;order,  from the matrix  referenced by  the pointer  RMX.  LDA  is the
;;length of a column.  M and N are the numbers of rows and columns.
(define (cmat->list M N A lda)
  (let loop-rows ((rows '()) (i 0))
    (if (= i M)
	(reverse rows)
      (let loop-cols ((cols '()) (j 0))
	(if (= j N)
	    (loop-rows (cons (reverse cols) rows) (+ 1 i))
	  (loop-cols (cons (cmat-ref A lda i j) cols) (+ 1 j)))))))

(define (zmat->list M N A lda)
  (let loop-rows ((rows '()) (i 0))
    (if (= i M)
	(reverse rows)
      (let loop-cols ((cols '()) (j 0))
	(if (= j N)
	    (loop-rows (cons (reverse cols) rows) (+ 1 i))
	  (loop-cols (cons (zmat-ref A lda i j) cols) (+ 1 j)))))))


;;;; done

)

;;; end of file
