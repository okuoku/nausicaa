;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/BLAS
;;;Contents: primitive functions
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


(library (foreign math blas primitives)
  (export

    srotg	srotmg

    (rename (cblas_sdsdot		sdsdot)
	    (cblas_dsdot		dsdot)
	    (cblas_sdot			sdot)
	    (cblas_ddot			ddot)

	    ;;Functions having prefixes Z and C only.
	    (cblas_cdotu_sub		cdotu_sub)
	    (cblas_cdotc_sub		cdotc_sub)
	    (cblas_zdotu_sub		zdotu_sub)
	    (cblas_zdotc_sub		zdotc_sub)

	     ;;Functions having prefixes S D SC DZ.
	    (cblas_snrm2		snrm2)
	    (cblas_sasum		sasum)
	    (cblas_dnrm2		dnrm2)
	    (cblas_dasum		dasum)
	    (cblas_scnrm2		scnrm2)
	    (cblas_scasum		scasum)
	    (cblas_dznrm2		dznrm2)
	    (cblas_dzasum		dzasum)

	     ;;Functions having standard 4 prefixes (S D C Z).
	    (cblas_isamax		isamax)
	    (cblas_idamax		idamax)
	    (cblas_icamax		icamax)
	    (cblas_izamax		izamax)

	     ;;Routines with standard 4 prefixes (s, d, c, z).
	    (cblas_sswap		sswap)
	    (cblas_scopy		scopy)
	    (cblas_saxpy		saxpy)
	    (cblas_dswap		dswap)
	    (cblas_dcopy		dcopy)
	    (cblas_daxpy		daxpy)
	    (cblas_cswap		cswap)
	    (cblas_ccopy		ccopy)
	    (cblas_caxpy		caxpy)
	    (cblas_zswap		zswap)
	    (cblas_zcopy		zcopy)
	    (cblas_zaxpy		zaxpy)

	     ;;Routines with S and D prefix only.
;;;	    (cblas_srotg		srotg)
;;;	    (cblas_srotmg		srotmg)
	    (cblas_srot			srot)
	    (cblas_srotm		srotm)
	    (cblas_drotg		drotg)
	    (cblas_drotmg		drotmg)
	    (cblas_drot			drot)
	    (cblas_drotm		drotm)

	     ;;Routines with S D C Z CS and ZD prefixes.
	    (cblas_sscal		sscal)
	    (cblas_dscal		dscal)
	    (cblas_cscal		cscal)
	    (cblas_zscal		zscal)
	    (cblas_csscal		csscal)
	    (cblas_zdscal		zdscal)

	     ;;Routines with standard 4 prefixes  (S, D, C, Z).
	    (cblas_sgemv		sgemv)
	    (cblas_sgbmv		sgbmv)
	    (cblas_strmv		strmv)
	    (cblas_stbmv		stbmv)
	    (cblas_stpmv		stpmv)
	    (cblas_strsv		strsv)
	    (cblas_stbsv		stbsv)
	    (cblas_stpsv		stpsv)
	    (cblas_dgemv		dgemv)
	    (cblas_dgbmv		dgbmv)
	    (cblas_dtrmv		dtrmv)
	    (cblas_dtbmv		dtbmv)
	    (cblas_dtpmv		dtpmv)
	    (cblas_dtrsv		dtrsv)
	    (cblas_dtbsv		dtbsv)
	    (cblas_dtpsv		dtpsv)
	    (cblas_cgemv		cgemv)
	    (cblas_cgbmv		cgbmv)
	    (cblas_ctrmv		ctrmv)
	    (cblas_ctbmv		ctbmv)
	    (cblas_ctpmv		ctpmv)
	    (cblas_ctrsv		ctrsv)
	    (cblas_ctbsv		ctbsv)
	    (cblas_ctpsv		ctpsv)
	    (cblas_zgemv		zgemv)
	    (cblas_zgbmv		zgbmv)
	    (cblas_ztrmv		ztrmv)
	    (cblas_ztbmv		ztbmv)
	    (cblas_ztpmv		ztpmv)
	    (cblas_ztrsv		ztrsv)
	    (cblas_ztbsv		ztbsv)
	    (cblas_ztpsv		ztpsv)

	     ;;Routines with S and D prefixes only.
	    (cblas_ssymv		ssymv)
	    (cblas_ssbmv		ssbmv)
	    (cblas_sspmv		sspmv)
	    (cblas_sger			sger)
	    (cblas_ssyr			ssyr)
	    (cblas_sspr			sspr)
	    (cblas_ssyr2		ssyr2)
	    (cblas_sspr2		sspr2)
	    (cblas_dsymv		dsymv)
	    (cblas_dsbmv		dsbmv)
	    (cblas_dspmv		dspmv)
	    (cblas_dger			dger)
	    (cblas_dsyr			dsyr)
	    (cblas_dspr			dspr)
	    (cblas_dsyr2		dsyr2)
	    (cblas_dspr2		dspr2)

	     ;;Routines with C and Z prefixes only.
	    (cblas_chemv		chemv)
	    (cblas_chbmv		chbmv)
	    (cblas_chpmv		chpmv)
	    (cblas_cgeru		cgeru)
	    (cblas_cgerc		cgerc)
	    (cblas_cher			cher)
	    (cblas_chpr			chpr)
	    (cblas_cher2		cher2)
	    (cblas_chpr2		chpr2)
	    (cblas_zhemv		zhemv)
	    (cblas_zhbmv		zhbmv)
	    (cblas_zhpmv		zhpmv)
	    (cblas_zgeru		zgeru)
	    (cblas_zgerc		zgerc)
	    (cblas_zher			zher)
	    (cblas_zhpr			zhpr)
	    (cblas_zher2		zher2)
	    (cblas_zhpr2		zhpr2)

	     ;;Routines with standard 4 prefixes  (S, D, C, Z).
	    (cblas_sgemm		sgemm)
	    (cblas_ssymm		ssymm)
	    (cblas_ssyrk		ssyrk)
	    (cblas_ssyr2k		ssyr2k)
	    (cblas_strmm		strmm)
	    (cblas_strsm		strsm)
	    (cblas_dgemm		dgemm)
	    (cblas_dsymm		dsymm)
	    (cblas_dsyrk		dsyrk)
	    (cblas_dsyr2k		dsyr2k)
	    (cblas_dtrmm		dtrmm)
	    (cblas_dtrsm		dtrsm)
	    (cblas_cgemm		cgemm)
	    (cblas_csymm		csymm)
	    (cblas_csyrk		csyrk)
	    (cblas_csyr2k		csyr2k)
	    (cblas_ctrmm		ctrmm)
	    (cblas_ctrsm		ctrsm)
	    (cblas_zgemm		zgemm)
	    (cblas_zsymm		zsymm)
	    (cblas_zsyrk		zsyrk)
	    (cblas_zsyr2k		zsyr2k)
	    (cblas_ztrmm		ztrmm)
	    (cblas_ztrsm		ztrsm)

	     ;;Routines with prefixes C and Z only.
	    (cblas_chemm		chemm)
	    (cblas_cherk		cherk)
	    (cblas_cher2k		cher2k)
	    (cblas_zhemm		zhemm)
	    (cblas_zherk		zherk)
	    (cblas_zher2k		zher2k)))
  (import (rnrs)
    (compensations)
    (foreign ffi)
    (only (foreign ffi sizeof) strideof-double)
    (foreign memory)
    (foreign cstrings)
    (foreign math blas platform)
    (foreign math blas sizeof))


;;;; boxes

(define box-length (* 2 strideof-double))
(define box1 (malloc box-length))
(define box2 (malloc box-length))
(define box3 (malloc box-length))
(define box4 (malloc box-length))



;;;; level 1 BLAS functions

;; (define-prim float sdsdot (int N) (float alpha) (float* X) (int incX) (float* Y) (int incY))

;; (define-prim double dsdot (int N) (float* X)  (int incX) (float* Y)  (int incY))
;; (define-prim float  sdot  (int N) (float* X)  (int incX) (float* Y)  (int incY))
;; (define-prim double ddot  (int N) (double* X) (int incX) (double* Y) (int incY))

;; ;;; Functions having prefixes Z and C only

;; (define-prim void cdotu_sub (int N) (void* X) (int incX) (void* Y) (int incY) (void* dotu))
;; (define-prim void cdotc_sub (int N) (void* X) (int incX) (void* Y) (int incY) (void* dotc))
;; (define-prim void zdotu_sub (int N) (void* X) (int incX) (void* Y) (int incY) (void* dotu))
;; (define-prim void zdotc_sub (int N) (void* X) (int incX) (void* Y) (int incY) (void* dotc))

;; ;;; Functions having prefixes S D SC DZ

;; (define-prim float snrm2 (int N) (float* X) (int incX))
;; (define-prim float sasum (int N) (float* X) (int incX))
;; (define-prim double dnrm2 (int N) (double* X) (int incX))
;; (define-prim double dasum (int N) (double* X) (int incX))
;; (define-prim float scnrm2 (int N) (void* X) (int incX))
;; (define-prim float scasum (int N) (void* X) (int incX))
;; (define-prim double dznrm2 (int N) (void* X) (int incX))
;; (define-prim double dzasum (int N) (void* X) (int incX))

;; ;;; Functions having standard 4 prefixes (S D C Z)

;; (define-prim cblas-index isamax (int N) (float* X) (int incX))
;; (define-prim cblas-index idamax (int N) (double* X) (int incX))
;; (define-prim cblas-index icamax (int N) (void* X) (int incX))
;; (define-prim cblas-index izamax (int N) (void* X) (int incX))


;;;; level 1 BLAS routines

;;; Routines with standard 4 prefixes (s) (d) (c) (z)

;; (define-prim void sswap (int N) (float* X) (int incX) (float* Y) (int incY))
;; (define-prim void scopy (int N) (float* X) (int incX) (float* Y) (int incY))
;; (define-prim void saxpy (int N) (float alpha) (float* X) (int incX) (float* Y) (int incY))

;; (define-prim void dswap (int N) (double* X) (int incX) (double* Y) (int incY))
;; (define-prim void dcopy (int N) (double* X) (int incX) (double* Y) (int incY))
;; (define-prim void daxpy (int N) (double alpha) (double* X) (int incX) (double* Y) (int incY))

;; (define-prim void cswap (int N) (void* X) (int incX) (void* Y) (int incY))
;; (define-prim void ccopy (int N) (void* X) (int incX) (void* Y) (int incY))
;; (define-prim void caxpy (int N) (void* alpha) (void* X) (int incX) (void* Y) (int incY))

;; (define-prim void zswap (int N) (void* X) (int incX) (void* Y) (int incY))
;; (define-prim void zcopy (int N) (void* X) (int incX) (void* Y) (int incY))
;; (define-prim void zaxpy (int N) (void* alpha) (void* X) (int incX) (void* Y) (int incY))

;; ;;; Routines with S and D prefix only

(define (srotg sa sb)
  (let ((sa*	box1)
	(sb*	box2)
	(c*	box3)
	(s*	box4))
    (pointer-set-c-float! sa* 0 sa)
    (pointer-set-c-float! sb* 0 sb)
    (cblas_srotg sa* sb* c* s*)
    (values (pointer-ref-c-float c* 0)
	    (pointer-ref-c-float s* 0))))

(define (srotmg sa sb sc sd flag sh)
  (if (= -2 flag)
      (begin
	(pointer-set-c-float! sh 0 1.)  ;SH(1,1)
	(pointer-set-c-float! sh 1 0.)  ;SH(2,1)
	(pointer-set-c-float! sh 2 0.)  ;SH(1,2)
	(pointer-set-c-float! sh 3 1.))	;SH(2,2)
    (let ((sa*	box1)
	  (sb*	box2)
	  (sc*	box3))
      (pointer-set-c-float! sa* 0 sa)
      (pointer-set-c-float! sb* 0 sb)
      (pointer-set-c-float! sc* 0 sc)
      (cblas_srotmg sa* sb* sc* sd sh)
      (case flag
	((1) (values))
	((0)
	 (pointer-set-c-float! sh 0 1.)  ;SH(1,1)
	 (pointer-set-c-float! sh 3 1.)) ;SH(2,2)
	((-1)
	 (pointer-set-c-float! sh 1 -1.)  ;SH(2,1)
	 (pointer-set-c-float! sh 2 -1.)) ;SH(1,2)
	(else
	 (assertion-violation 'srotmg "invalid flag value, must be: 1, 0, -1 or -2" flag))))))

;; (define-prim void srotg (float* a) (float* b) (float* c) (float* s))
;; (define-prim void srotmg (float* d1) (float* d2) (float* b1) (float b2) (float* P))
;; (define-prim void srot (int N) (float* X) (int incX) (float* Y) (int incY) (float c) (float s))
;; (define-prim void srotm (int N) (float* X) (int incX) (float* Y) (int incY) (float* P))

;; (define-prim void drotg (double* a) (double* b) (double* c) (double* s))
;; (define-prim void drotmg (double* d1) (double* d2) (double* b1) (double b2) (double* P))
;; (define-prim void drot (int N) (double* X) (int incX) (double* Y) (int incY) (double c) (double s))
;; (define-prim void drotm (int N) (double* X) (int incX) (double* Y) (int incY) (double* P))

;; ;;; Routines with S D C Z CS and ZD prefixes

;; (define-prim void sscal (int N) (float alpha) (float* X) (int incX))
;; (define-prim void dscal (int N) (double alpha) (double* X) (int incX))
;; (define-prim void cscal (int N) (void* alpha) (void* X) (int incX))
;; (define-prim void zscal (int N) (void* alpha) (void* X) (int incX))
;; (define-prim void csscal (int N) (float alpha) (void* X) (int incX))
;; (define-prim void zdscal (int N) (double alpha) (void* X) (int incX))


;;;; level 2 BLAS

;;; Routines with standard 4 prefixes (S) (D) (C) (Z)

;; (define-prim void sgemv (cblas-order order) (cblas-transpose transA) (int M) (int N) (float alpha) (float* A) (int lda) (float* X) (int incX) (float beta) (float* Y) (int incY))
;; (define-prim void sgbmv (cblas-order order) (cblas-transpose transA) (int M) (int N) (int KL) (int KU) (float alpha) (float* A) (int lda) (float* X) (int incX) (float beta) (float* Y) (int incY))
;; (define-prim void strmv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (float* A) (int lda) (float* X) (int incX))
;; (define-prim void stbmv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (int K) (float* A) (int lda) (float* X) (int incX))
;; (define-prim void stpmv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (float* Ap) (float* X) (int incX))
;; (define-prim void strsv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (float* A) (int lda) (float* X) (int incX))
;; (define-prim void stbsv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (int K) (float* A) (int lda) (float* X) (int incX))
;; (define-prim void stpsv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (float* Ap) (float* X) (int incX))

;; (define-prim void dgemv (cblas-order order) (cblas-transpose transA) (int M) (int N) (double alpha) (double* A) (int lda) (double* X) (int incX) (double beta) (double* Y) (int incY))
;; (define-prim void dgbmv (cblas-order order) (cblas-transpose transA) (int M) (int N) (int KL) (int KU) (double alpha) (double* A) (int lda) (double* X) (int incX) (double beta) (double* Y) (int incY))
;; (define-prim void dtrmv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (double* A) (int lda) (double* X) (int incX))
;; (define-prim void dtbmv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (int K) (double* A) (int lda) (double* X) (int incX))
;; (define-prim void dtpmv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (double* Ap) (double* X) (int incX))
;; (define-prim void dtrsv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (double* A) (int lda) (double* X) (int incX))
;; (define-prim void dtbsv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (int K) (double* A) (int lda) (double* X) (int incX))
;; (define-prim void dtpsv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (double* Ap) (double* X) (int incX))

;; (define-prim void cgemv (cblas-order order) (cblas-transpose transA) (int M) (int N) (void* alpha) (void* A) (int lda) (void* X) (int incX) (void* beta) (void* Y) (int incY))
;; (define-prim void cgbmv (cblas-order order) (cblas-transpose transA) (int M) (int N) (int KL) (int KU) (void* alpha) (void* A) (int lda) (void* X) (int incX) (void* beta) (void* Y) (int incY))
;; (define-prim void ctrmv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (enum DIAG Diag) (int N) (void* A) (int lda) (void* X) (int incX))
;; (define-prim void ctbmv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (int K) (void* A) (int lda) (void* X) (int incX))
;; (define-prim void ctpmv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (void* Ap) (void* X) (int incX))
;; (define-prim void ctrsv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (void* A) (int lda) (void* X) (int incX))
;; (define-prim void ctbsv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (int K) (void* A) (int lda) (void* X) (int incX))
;; (define-prim void ctpsv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (void* Ap) (void* X) (int incX))

;; (define-prim void zgemv (cblas-order order) (cblas-transpose transA) (int M) (int N) (void* alpha) (void* A) (int lda) (void* X) (int incX) (void* beta) (void* Y) (int incY))
;; (define-prim void zgbmv (cblas-order order) (cblas-transpose transA) (int M) (int N) (int KL) (int KU) (void* alpha) (void* A) (int lda) (void* X) (int incX) (void* beta) (void* Y) (int incY))
;; (define-prim void ztrmv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (void* A) (int lda) (void* X) (int incX))
;; (define-prim void ztbmv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (int K) (void* A) (int lda) (void* X) (int incX))
;; (define-prim void ztpmv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (void* Ap) (void* X) (int incX))
;; (define-prim void ztrsv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (void* A) (int lda) (void* X) (int incX))
;; (define-prim void ztbsv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (int K) (void* A) (int lda) (void* X) (int incX))
;; (define-prim void ztpsv (cblas-order order) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int N) (void* Ap) (void* X) (int incX))

;; ;;; Routines with S and D prefixes only

;; (define-prim void ssymv (cblas-order order) (cblas-uplo uplo) (int N) (float alpha) (float* A) (int lda) (float* X) (int incX) (float beta) (float* Y) (int incY))
;; (define-prim void ssbmv (cblas-order order) (cblas-uplo uplo) (int N) (int K) (float alpha) (float* A) (int lda) (float* X) (int incX) (float beta) (float* Y) (int incY))
;; (define-prim void sspmv (cblas-order order) (cblas-uplo uplo) (int N) (float alpha) (float* Ap) (float* X) (int incX) (float beta) (float* Y) (int incY))
;; (define-prim void sger (cblas-order order) (int M) (int N) (float alpha) (float* X) (int incX) (float* Y) (int incY) (float* A) (int lda))
;; (define-prim void ssyr (cblas-order order) (cblas-uplo uplo) (int N) (float alpha) (float* X) (int incX) (float* A) (int lda))
;; (define-prim void sspr (cblas-order order) (cblas-uplo uplo) (int N) (float alpha) (float* X) (int incX) (float* Ap))
;; (define-prim void ssyr2 (cblas-order order) (cblas-uplo uplo) (int N) (float alpha) (float* X) (int incX) (float* Y) (int incY) (float* A) (int lda))
;; (define-prim void sspr2 (cblas-order order) (cblas-uplo uplo) (int N) (float alpha) (float* X) (int incX) (float* Y) (int incY) (float* A))

;; (define-prim void dsymv (cblas-order order) (cblas-uplo uplo) (int N) (double alpha) (double* A) (int lda) (double* X) (int incX) (double beta) (double* Y) (int incY))
;; (define-prim void dsbmv (cblas-order order) (cblas-uplo uplo) (int N) (int K) (double alpha) (double* A) (int lda) (double* X) (int incX) (double beta) (double* Y) (int incY))
;; (define-prim void dspmv (cblas-order order) (cblas-uplo uplo) (int N) (double alpha) (double* Ap) (double* X) (int incX) (double beta) (double* Y) (int incY))
;; (define-prim void dger (cblas-order order) (int M) (int N) (double alpha) (double* X) (int incX) (double* Y) (int incY) (double* A) (int lda))
;; (define-prim void dsyr (cblas-order order) (cblas-uplo uplo) (int N) (double alpha) (double* X) (int incX) (double* A) (int lda))
;; (define-prim void dspr (cblas-order order) (cblas-uplo uplo) (int N) (double alpha) (double* X) (int incX) (double* Ap))
;; (define-prim void dsyr2 (cblas-order order) (cblas-uplo uplo) (int N) (double alpha) (double* X) (int incX) (double* Y) (int incY) (double* A) (int lda))
;; (define-prim void dspr2 (cblas-order order) (cblas-uplo uplo) (int N) (double alpha) (double* X) (int incX) (double* Y) (int incY) (double* A))

;; ;;; Routines with C and Z prefixes only

;; (define-prim void chemv (cblas-order order) (cblas-uplo uplo) (int N) (void* alpha) (void* A) (int lda) (void* X) (int incX) (void* beta) (void* Y) (int incY))
;; (define-prim void chbmv (cblas-order order) (cblas-uplo uplo) (int N) (int K) (void* alpha) (void* A) (int lda) (void* X) (int incX) (void* beta) (void* Y) (int incY))
;; (define-prim void chpmv (cblas-order order) (cblas-uplo uplo) (int N) (void* alpha) (void* Ap) (void* X) (int incX) (void* beta) (void* Y) (int incY))
;; (define-prim void cgeru (cblas-order order) (int M) (int N) (void* alpha) (void* X) (int incX) (void* Y) (int incY) (void* A) (int lda))
;; (define-prim void cgerc (cblas-order order) (int M) (int N) (void* alpha) (void* X) (int incX) (void* Y) (int incY) (void* A) (int lda))
;; (define-prim void cher (cblas-order order) (cblas-uplo uplo) (int N) (float alpha) (void* X) (int incX) (void* A) (int lda))
;; (define-prim void chpr (cblas-order order) (cblas-uplo uplo) (int N) (float alpha) (void* X) (int incX) (void* A))
;; (define-prim void cher2 (cblas-order order) (cblas-uplo uplo) (int N) (void* alpha) (void* X) (int incX) (void* Y) (int incY) (void* A) (int lda))
;; (define-prim void chpr2 (cblas-order order) (cblas-uplo uplo) (int N) (void* alpha) (void* X) (int incX) (void* Y) (int incY) (void* Ap))

;; (define-prim void zhemv (cblas-order order) (cblas-uplo uplo) (int N) (void* alpha) (void* A) (int lda) (void* X) (int incX) (void* beta) (void* Y) (int incY))
;; (define-prim void zhbmv (cblas-order order) (cblas-uplo uplo) (int N) (int K) (void* alpha) (void* A) (int lda) (void* X) (int incX) (void* beta) (void* Y) (int incY))
;; (define-prim void zhpmv (cblas-order order) (cblas-uplo uplo) (int N) (void* alpha) (void* Ap) (void* X) (int incX) (void* beta) (void* Y) (int incY))
;; (define-prim void zgeru (cblas-order order) (int M) (int N) (void* alpha) (void* X) (int incX) (void* Y) (int incY) (void* A) (int lda))
;; (define-prim void zgerc (cblas-order order) (int M) (int N) (void* alpha) (void* X) (int incX) (void* Y) (int incY) (void* A) (int lda))
;; (define-prim void zher (cblas-order order) (cblas-uplo uplo) (int N) (double alpha) (void* X) (int incX) (void* A) (int lda))
;; (define-prim void zhpr (cblas-order order) (cblas-uplo uplo) (int N) (double alpha) (void* X) (int incX) (void* A))
;; (define-prim void zher2 (cblas-order order) (cblas-uplo uplo) (int N) (void* alpha) (void* X) (int incX) (void* Y) (int incY) (void* A) (int lda))
;; (define-prim void zhpr2 (cblas-order order) (cblas-uplo uplo) (int N) (void* alpha) (void* X) (int incX) (void* Y) (int incY) (void* Ap))


;;;; level 3 BLAS

;;; Routines with standard 4 prefixes (S) (D) (C) (Z)

;; (define-prim void sgemm (cblas-order order) (cblas-transpose transA) (cblas-transpose transB) (int M) (int N) (int K) (float alpha) (float* A) (int lda) (float* B) (int ldb) (float beta) (float* C) (int ldc))
;; (define-prim void ssymm (cblas-order order) (cblas-side side) (cblas-uplo uplo) (int M) (int N) (float alpha) (float* A) (int lda) (float* B) (int ldb) (float beta) (float* C) (int ldc))
;; (define-prim void ssyrk (cblas-order order) (cblas-uplo uplo) (cblas-transpose trans) (int N) (int K) (float alpha) (float* A) (int lda) (float beta) (float* C) (int ldc))
;; (define-prim void ssyr2k (cblas-order order) (cblas-uplo uplo) (cblas-transpose trans) (int N) (int K) (float alpha) (float* A) (int lda) (float* B) (int ldb) (float beta) (float* C) (int ldc))
;; (define-prim void strmm (cblas-order order) (cblas-side side) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int M) (int N) (float alpha) (float* A) (int lda) (float* B) (int ldb))
;; (define-prim void strsm (cblas-order order) (cblas-side side) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int M) (int N) (float alpha) (float* A) (int lda) (float* B) (int ldb))

;; (define-prim void dgemm (cblas-order order) (cblas-transpose transA) (cblas-transpose transB) (int M) (int N) (int K) (double alpha) (double* A) (int lda) (double* B) (int ldb) (double beta) (double* C) (int ldc))
;; (define-prim void dsymm (cblas-order order) (cblas-side side) (cblas-uplo uplo) (int M) (int N) (double alpha) (double* A) (int lda) (double* B) (int ldb) (double beta) (double* C) (int ldc))
;; (define-prim void dsyrk (cblas-order order) (cblas-uplo uplo) (cblas-transpose trans) (int N) (int K) (double alpha) (double* A) (int lda) (double beta) (double* C) (int ldc))
;; (define-prim void dsyr2k (cblas-order order) (cblas-uplo uplo) (cblas-transpose trans) (int N) (int K) (double alpha) (double* A) (int lda) (double* B) (int ldb) (double beta) (double* C) (int ldc))
;; (define-prim void dtrmm (cblas-order order) (cblas-side side) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int M) (int N) (double alpha) (double* A) (int lda) (double* B) (int ldb))
;; (define-prim void dtrsm (cblas-order order) (cblas-side side) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int M) (int N) (double alpha) (double* A) (int lda) (double* B) (int ldb))

;; (define-prim void cgemm (cblas-order order) (cblas-transpose transA) (cblas-transpose transB) (int M) (int N) (int K) (void* alpha) (void* A) (int lda) (void* B) (int ldb) (void* beta) (void* C) (int ldc))
;; (define-prim void csymm (cblas-order order) (cblas-side side) (cblas-uplo uplo) (int M) (int N) (void* alpha) (void* A) (int lda) (void* B) (int ldb) (void* beta) (void* C) (int ldc))
;; (define-prim void csyrk (cblas-order order) (cblas-uplo uplo) (cblas-transpose trans) (int N) (int K) (void* alpha) (void* A) (int lda) (void* beta) (void* C) (int ldc))
;; (define-prim void csyr2k (cblas-order order) (cblas-uplo uplo) (cblas-transpose trans) (int N) (int K) (void* alpha) (void* A) (int lda) (void* B) (int ldb) (void* beta) (void* C) (int ldc))
;; (define-prim void ctrmm (cblas-order order) (cblas-side side) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int M) (int N) (void* alpha) (void* A) (int lda) (void* B) (int ldb))
;; (define-prim void ctrsm (cblas-order order) (cblas-side side) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int M) (int N) (void* alpha) (void* A) (int lda) (void* B) (int ldb))

;; (define-prim void zgemm (cblas-order order) (cblas-transpose transA) (cblas-transpose transB) (int M) (int N) (int K) (void* alpha) (void* A) (int lda) (void* B) (int ldb) (void* beta) (void* C) (int ldc))
;; (define-prim void zsymm (cblas-order order) (cblas-side side) (cblas-uplo uplo) (int M) (int N) (void* alpha) (void* A) (int lda) (void* B) (int ldb) (void* beta) (void* C) (int ldc))
;; (define-prim void zsyrk (cblas-order order) (cblas-uplo uplo) (cblas-transpose trans) (int N) (int K) (void* alpha) (void* A) (int lda) (void* beta) (void* C) (int ldc))
;; (define-prim void zsyr2k (cblas-order order) (cblas-uplo uplo) (cblas-transpose trans) (int N) (int K) (void* alpha) (void* A) (int lda) (void* B) (int ldb) (void* beta) (void* C) (int ldc))
;; (define-prim void ztrmm (cblas-order order) (cblas-side side) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int M) (int N) (void* alpha) (void* A) (int lda) (void* B) (int ldb))
;; (define-prim void ztrsm (cblas-order order) (cblas-side side) (cblas-uplo uplo) (cblas-transpose transA) (cblas-diag Diag) (int M) (int N) (void* alpha) (void* A) (int lda) (void* B) (int ldb))

;; ;;; Routines with prefixes C and Z only

;; (define-prim void chemm (cblas-order order) (cblas-side side) (cblas-uplo uplo) (int M) (int N) (void* alpha) (void* A) (int lda) (void* B) (int ldb) (void* beta) (void* C) (int ldc))
;; (define-prim void cherk (cblas-order order) (cblas-uplo uplo) (cblas-transpose trans) (int N) (int K) (float alpha) (void* A) (int lda) (float beta) (void* C) (int ldc))
;; (define-prim void cher2k (cblas-order order) (cblas-uplo uplo) (cblas-transpose trans) (int N) (int K) (void* alpha) (void* A) (int lda) (void* B) (int ldb) (float beta) (void* C) (int ldc))

;; (define-prim void zhemm (cblas-order order) (cblas-side side) (cblas-uplo uplo) (int M) (int N) (void* alpha) (void* A) (int lda) (void* B) (int ldb) (void* beta) (void* C) (int ldc))
;; (define-prim void zherk (cblas-order order) (cblas-uplo uplo) (cblas-transpose trans) (int N) (int K) (double alpha) (void* A) (int lda) (double beta) (void* C) (int ldc))
;; (define-prim void zher2k (cblas-order order) (cblas-uplo uplo) (cblas-transpose trans) (int N) (int K) (void* alpha) (void* A) (int lda) (void* B) (int ldb) (double beta) (void* C) (int ldc))


;;;; done

)

;;; end of file
