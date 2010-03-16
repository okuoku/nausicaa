;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/BLAS
;;;Contents: bindings to foreign functions
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


(library (foreign math blas platform)
  (export
    cblas_sdsdot		cblas_dsdot
    cblas_sdot			cblas_ddot

    ;;Functions having prefixes Z and C only.
    cblas_cdotu_sub		cblas_cdotc_sub
    cblas_zdotu_sub		cblas_zdotc_sub

    ;;Functions having prefixes S D SC DZ.
    cblas_snrm2			cblas_sasum
    cblas_dnrm2			cblas_dasum
    cblas_scnrm2		cblas_scasum
    cblas_dznrm2		cblas_dzasum

    ;;Functions having standard 4 prefixes (S D C Z).
    cblas_isamax		cblas_idamax
    cblas_icamax		cblas_izamax

;;;  level 1 BLAS routines

    ;;Routines with standard 4 prefixes (s, d, c, z).
    cblas_sswap			cblas_scopy
    cblas_saxpy			cblas_dswap
    cblas_dcopy			cblas_daxpy
    cblas_cswap			cblas_ccopy
    cblas_caxpy			cblas_zswap
    cblas_zcopy			cblas_zaxpy

    ;;Routines with S and D prefix only.
    cblas_srotg			cblas_srotmg
    cblas_srot			cblas_srotm
    cblas_drotg			cblas_drotmg
    cblas_drot			cblas_drotm

    ;;Routines with S D C Z CS and ZD prefixes.
    cblas_sscal			cblas_dscal
    cblas_cscal			cblas_zscal
    cblas_csscal		cblas_zdscal

;;;;  level 2 BLAS

    ;;Routines with standard 4 prefixes  (S, D, C, Z).
    cblas_sgemv			cblas_sgbmv
    cblas_strmv			cblas_stbmv
    cblas_stpmv			cblas_strsv
    cblas_stbsv			cblas_stpsv
    cblas_dgemv			cblas_dgbmv
    cblas_dtrmv			cblas_dtbmv
    cblas_dtpmv			cblas_dtrsv
    cblas_dtbsv			cblas_dtpsv
    cblas_cgemv			cblas_cgbmv
    cblas_ctrmv			cblas_ctbmv
    cblas_ctpmv			cblas_ctrsv
    cblas_ctbsv			cblas_ctpsv
    cblas_zgemv			cblas_zgbmv
    cblas_ztrmv			cblas_ztbmv
    cblas_ztpmv			cblas_ztrsv
    cblas_ztbsv			cblas_ztpsv

    ;;Routines with S and D prefixes only.
    cblas_ssymv			cblas_ssbmv
    cblas_sspmv			cblas_sger
    cblas_ssyr			cblas_sspr
    cblas_ssyr2			cblas_sspr2
    cblas_dsymv			cblas_dsbmv
    cblas_dspmv			cblas_dger
    cblas_dsyr			cblas_dspr
    cblas_dsyr2			cblas_dspr2

    ;;Routines with C and Z prefixes only.
    cblas_chemv			cblas_chbmv
    cblas_chpmv			cblas_cgeru
    cblas_cgerc			cblas_cher
    cblas_chpr			cblas_cher2
    cblas_chpr2			cblas_zhemv
    cblas_zhbmv			cblas_zhpmv
    cblas_zgeru			cblas_zgerc
    cblas_zher			cblas_zhpr
    cblas_zher2			cblas_zhpr2

;;;;  level 3 BLAS

    ;;Routines with standard 4 prefixes  (S, D, C, Z).
    cblas_sgemm			cblas_ssymm
    cblas_ssyrk			cblas_ssyr2k
    cblas_strmm			cblas_strsm
    cblas_dgemm			cblas_dsymm
    cblas_dsyrk			cblas_dsyr2k
    cblas_dtrmm			cblas_dtrsm
    cblas_cgemm			cblas_csymm
    cblas_csyrk			cblas_csyr2k
    cblas_ctrmm			cblas_ctrsm
    cblas_zgemm			cblas_zsymm
    cblas_zsyrk			cblas_zsyr2k
    cblas_ztrmm			cblas_ztrsm

    ;;Routines with prefixes C and Z only.
    cblas_chemm			cblas_cherk
    cblas_cher2k		cblas_zhemm
    cblas_zherk			cblas_zher2k

    ;;Variadic!!!
    ;;
    ;; cblas_xerbla
    )
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign math blas shared-object)
    (foreign math blas sizeof))

(define float*		'void*)
(define double*		'void*)


;;;; prototypes for level 1 BLAS functions
;;
;;complex are recast as routines
;;

(define-c-functions cblas-shared-object
  (cblas_sdsdot		(float  cblas_sdsdot (int float float* int float* int)))
  (cblas_dsdot		(double cblas_dsdot  (int float* int float* int)))
  (cblas_sdot		(float  cblas_sdot   (int float* int float* int)))
  (cblas_ddot		(double cblas_ddot   (int double* int double* int)))

  ;;Functions having prefixes Z and C only.
  (cblas_cdotu_sub	(void cblas_cdotu_sub (int void* int void* int void*)))
  (cblas_cdotc_sub	(void cblas_cdotc_sub (int void* int void* int void*)))
  (cblas_zdotu_sub	(void cblas_zdotu_sub (int void* int void* int void*)))
  (cblas_zdotc_sub	(void cblas_zdotc_sub (int void* int void* int void*)))

  ;;Functions having prefixes S D SC DZ.
  (cblas_snrm2		(float cblas_snrm2 (int float* int)))
  (cblas_sasum		(float cblas_sasum (int float* int)))
  (cblas_dnrm2		(double cblas_dnrm2 (int double* int)))
  (cblas_dasum		(double cblas_dasum (int double* int)))
  (cblas_scnrm2		(float cblas_scnrm2 (int void* int)))
  (cblas_scasum		(float cblas_scasum (int void* int)))
  (cblas_dznrm2		(double cblas_dznrm2 (int void* int)))
  (cblas_dzasum		(double cblas_dzasum (int void* int)))

  ;;Functions having standard 4 prefixes (S D C Z).
  (cblas_isamax		(CBLAS_INDEX cblas_isamax (int float* int)))
  (cblas_idamax		(CBLAS_INDEX cblas_idamax (int double* int)))
  (cblas_icamax		(CBLAS_INDEX cblas_icamax (int void* int)))
  (cblas_izamax		(CBLAS_INDEX cblas_izamax (int void* int))))


;;;; prototypes for level 1 BLAS routines

(define-c-functions cblas-shared-object
  ;;Routines with standard 4 prefixes (s, d, c, z).
  (cblas_sswap		(void cblas_sswap (int float* int float* int)))
  (cblas_scopy		(void cblas_scopy (int float* int float* int)))
  (cblas_saxpy		(void cblas_saxpy (int float float* int float* int)))
  (cblas_dswap		(void cblas_dswap (int double* int double* int)))
  (cblas_dcopy		(void cblas_dcopy (int double* int double* int)))
  (cblas_daxpy		(void cblas_daxpy (int double double* int double* int)))
  (cblas_cswap		(void cblas_cswap (int void* int void* int)))
  (cblas_ccopy		(void cblas_ccopy (int void* int void* int)))
  (cblas_caxpy		(void cblas_caxpy (int void* void* int void* int)))
  (cblas_zswap		(void cblas_zswap (int void* int void* int)))
  (cblas_zcopy		(void cblas_zcopy (int void* int void* int)))
  (cblas_zaxpy		(void cblas_zaxpy (int void* void* int void* int)))

  ;;Routines with S and D prefix only.
  (cblas_srotg		(void cblas_srotg (float* float* float* float*)))
  (cblas_srotmg		(void cblas_srotmg (float* float* float* float float*)))
  (cblas_srot		(void cblas_srot (int float* int float* int float float)))
  (cblas_srotm		(void cblas_srotm (int float* int float* int float*)))
  (cblas_drotg		(void cblas_drotg (double* double* double* double*)))
  (cblas_drotmg		(void cblas_drotmg (double* double* double* double double*)))
  (cblas_drot		(void cblas_drot (int double* int double* int double double)))
  (cblas_drotm		(void cblas_drotm (int double* int double* int double*)))

  ;;Routines with S D C Z CS and ZD prefixes.
  (cblas_sscal		(void cblas_sscal (int float float* int)))
  (cblas_dscal		(void cblas_dscal (int double double* int)))
  (cblas_cscal		(void cblas_cscal (int void* void* int)))
  (cblas_zscal		(void cblas_zscal (int void* void* int)))
  (cblas_csscal		(void cblas_csscal (int float void* int)))
  (cblas_zdscal		(void cblas_zdscal (int double void* int))))


;;;; prototypes for level 2 BLAS

(define-c-functions cblas-shared-object
  ;;Routines with standard 4 prefixes  (S, D, C, Z).
  (cblas_sgemv		(void cblas_sgemv (CBLAS_ORDER CBLAS_TRANSPOSE int int
						       float float* int float* int float float* int)))
  (cblas_sgbmv		(void cblas_sgbmv (CBLAS_ORDER CBLAS_TRANSPOSE int int
						       int int float float* int float*
						       int float float* int)))
  (cblas_strmv		(void cblas_strmv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int float* int float* int)))
  (cblas_stbmv		(void cblas_stbmv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int int float* int float* int)))
  (cblas_stpmv		(void cblas_stpmv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int float* float* int)))
  (cblas_strsv		(void cblas_strsv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int float* int float* int)))
  (cblas_stbsv		(void cblas_stbsv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int int float* int float* int)))
  (cblas_stpsv		(void cblas_stpsv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int float* float* int)))
  (cblas_dgemv		(void cblas_dgemv (CBLAS_ORDER CBLAS_TRANSPOSE int int
						       double double* int double* int double double* int)))
  (cblas_dgbmv		(void cblas_dgbmv (CBLAS_ORDER CBLAS_TRANSPOSE int int
						       int int double double* int double*
						       int double double* int)))
  (cblas_dtrmv		(void cblas_dtrmv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int double* int double* int)))
  (cblas_dtbmv		(void cblas_dtbmv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int int double* int double* int)))
  (cblas_dtpmv		(void cblas_dtpmv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int double* double* int)))
  (cblas_dtrsv		(void cblas_dtrsv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int double* int double* int)))
  (cblas_dtbsv		(void cblas_dtbsv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int int double* int double* int)))
  (cblas_dtpsv		(void cblas_dtpsv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int double* double* int)))
  (cblas_cgemv		(void cblas_cgemv (CBLAS_ORDER CBLAS_TRANSPOSE int int
						       void* void* int void* int void* void* int)))
  (cblas_cgbmv		(void cblas_cgbmv (CBLAS_ORDER CBLAS_TRANSPOSE int int
						       int int void* void* int void*
						       int void* void* int)))
  (cblas_ctrmv		(void cblas_ctrmv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int void* int void* int)))
  (cblas_ctbmv		(void cblas_ctbmv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int int void* int void* int)))
  (cblas_ctpmv		(void cblas_ctpmv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int void* void* int)))
  (cblas_ctrsv		(void cblas_ctrsv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int void* int void* int)))
  (cblas_ctbsv		(void cblas_ctbsv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int int void* int void* int)))
  (cblas_ctpsv		(void cblas_ctpsv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int void* void* int)))
  (cblas_zgemv		(void cblas_zgemv (CBLAS_ORDER CBLAS_TRANSPOSE int int
						       void* void* int void* int void* void* int)))
  (cblas_zgbmv		(void cblas_zgbmv (CBLAS_ORDER CBLAS_TRANSPOSE int int
						       int int void* void* int void*
						       int void* void* int)))
  (cblas_ztrmv		(void cblas_ztrmv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int void* int void* int)))
  (cblas_ztbmv		(void cblas_ztbmv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int int void* int void* int)))
  (cblas_ztpmv		(void cblas_ztpmv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int void* void* int)))
  (cblas_ztrsv		(void cblas_ztrsv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int void* int void* int)))
  (cblas_ztbsv		(void cblas_ztbsv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int int void* int void* int)))
  (cblas_ztpsv		(void cblas_ztpsv (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE CBLAS_DIAG
						       int void* void* int)))

  ;;Routines with S and D prefixes only.
  (cblas_ssymv		(void cblas_ssymv (CBLAS_ORDER CBLAS_UPLO int float float*
						       int float* int float float* int)))
  (cblas_ssbmv		(void cblas_ssbmv (CBLAS_ORDER CBLAS_UPLO int int float float*
						       int float* int float float* int)))
  (cblas_sspmv		(void cblas_sspmv (CBLAS_ORDER CBLAS_UPLO int float float*
						       float* int float float* int)))
  (cblas_sger		(void cblas_sger (CBLAS_ORDER int int float float* int
						      float* int float* int)))
  (cblas_ssyr		(void cblas_ssyr (CBLAS_ORDER CBLAS_UPLO int float float* int float* int)))
  (cblas_sspr		(void cblas_sspr (CBLAS_ORDER CBLAS_UPLO int float float* int float*)))
  (cblas_ssyr2		(void cblas_ssyr2 (CBLAS_ORDER CBLAS_UPLO int float float*
						       int float* int float* int)))
  (cblas_sspr2		(void cblas_sspr2 (CBLAS_ORDER CBLAS_UPLO int float float*
						       int float* int float*)))
  (cblas_dsymv		(void cblas_dsymv (CBLAS_ORDER CBLAS_UPLO int double double*
						       int double* int double double* int)))
  (cblas_dsbmv		(void cblas_dsbmv (CBLAS_ORDER CBLAS_UPLO int int double double*
						       int double* int double double* int)))
  (cblas_dspmv		(void cblas_dspmv (CBLAS_ORDER CBLAS_UPLO int double double*
						       double* int double double* int)))
  (cblas_dger		(void cblas_dger (CBLAS_ORDER int int double double* int
						      double* int double* int)))
  (cblas_dsyr		(void cblas_dsyr (CBLAS_ORDER CBLAS_UPLO int double double*
						      int double* int)))
  (cblas_dspr		(void cblas_dspr (CBLAS_ORDER CBLAS_UPLO int double double* int double*)))
  (cblas_dsyr2		(void cblas_dsyr2 (CBLAS_ORDER CBLAS_UPLO int double double*
						       int double* int double* int)))
  (cblas_dspr2		(void cblas_dspr2 (CBLAS_ORDER CBLAS_UPLO int double double*
						       int double* int double)))

  ;;Routines with C and Z prefixes only.
  (cblas_chemv		(void cblas_chemv (CBLAS_ORDER CBLAS_UPLO int void* void*
						       int void* int void* void* int)))
  (cblas_chbmv		(void cblas_chbmv (CBLAS_ORDER CBLAS_UPLO int int void* void*
						       int void* int void* void* int)))
  (cblas_chpmv		(void cblas_chpmv (CBLAS_ORDER CBLAS_UPLO int void* void*
						       void* int void* void* int)))
  (cblas_cgeru		(void cblas_cgeru (CBLAS_ORDER int int void* void* int
						       void* int void* int)))
  (cblas_cgerc		(void cblas_cgerc (CBLAS_ORDER int int void* void* int
						       void* int void* int)))
  (cblas_cher		(void cblas_cher (CBLAS_ORDER CBLAS_UPLO int float void* int void* int)))
  (cblas_chpr		(void cblas_chpr (CBLAS_ORDER CBLAS_UPLO int float void* int void*)))
  (cblas_cher2		(void cblas_cher2 (CBLAS_ORDER CBLAS_UPLO int void* void* int
						       void* int void* int)))
  (cblas_chpr2		(void cblas_chpr2 (CBLAS_ORDER CBLAS_UPLO int void* void* int
						       void* int void*)))
  (cblas_zhemv		(void cblas_zhemv (CBLAS_ORDER CBLAS_UPLO int void* void*
						       int void* int void* void* int)))
  (cblas_zhbmv		(void cblas_zhbmv (CBLAS_ORDER CBLAS_UPLO int int void* void*
						       int void* int void* void* int)))
  (cblas_zhpmv		(void cblas_zhpmv (CBLAS_ORDER CBLAS_UPLO int void* void*
						       void* int void* void* int)))
  (cblas_zgeru		(void cblas_zgeru (CBLAS_ORDER int int void* void* int void* int void* int)))
  (cblas_zgerc		(void cblas_zgerc (CBLAS_ORDER int int void* void* int void* int void* int)))
  (cblas_zher		(void cblas_zher (CBLAS_ORDER CBLAS_UPLO int double void* int void* int)))
  (cblas_zhpr		(void cblas_zhpr (CBLAS_ORDER CBLAS_UPLO int double void* int void*)))
  (cblas_zher2		(void cblas_zher2 (CBLAS_ORDER CBLAS_UPLO int void* void* int
						       void* int void* int)))
  (cblas_zhpr2		(void cblas_zhpr2 (CBLAS_ORDER CBLAS_UPLO int void* void* int void* int void*))))


;;;; prototypes for level 3 BLAS

(define-c-functions cblas-shared-object
  ;;Routines with standard 4 prefixes  (S, D, C, Z).
  (cblas_sgemm		(void cblas_sgemm (CBLAS_ORDER CBLAS_TRANSPOSE CBLAS_TRANSPOSE int int
						       int float float* int float* int
						       float float* int)))
  (cblas_ssymm		(void cblas_ssymm (CBLAS_ORDER CBLAS_SIDE CBLAS_UPLO int int
						       float float* int float* int float float* int)))
  (cblas_ssyrk		(void cblas_ssyrk (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE int int
						       float float* int float float* int)))
  (cblas_ssyr2k		(void cblas_ssyr2k (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE int int
							float float* int float* int float float* int)))
  (cblas_strmm		(void cblas_strmm (CBLAS_ORDER CBLAS_SIDE CBLAS_UPLO CBLAS_TRANSPOSE
						       CBLAS_DIAG int int float float* int float* int)))
  (cblas_strsm		(void cblas_strsm (CBLAS_ORDER CBLAS_SIDE CBLAS_UPLO CBLAS_TRANSPOSE
						       CBLAS_DIAG int int float float* int float* int)))
  (cblas_dgemm		(void cblas_dgemm (CBLAS_ORDER CBLAS_TRANSPOSE CBLAS_TRANSPOSE int int
						       int double double* int double* int
						       double double* int)))
  (cblas_dsymm		(void cblas_dsymm (CBLAS_ORDER CBLAS_SIDE CBLAS_UPLO int int
						       double double* int double* int double double* int)))
  (cblas_dsyrk		(void cblas_dsyrk (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE int int
						       double double* int double double* int)))
  (cblas_dsyr2k		(void cblas_dsyr2k (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE int int
							double double* int double* int double
							double* int)))
  (cblas_dtrmm		(void cblas_dtrmm (CBLAS_ORDER CBLAS_SIDE CBLAS_UPLO CBLAS_TRANSPOSE
						       CBLAS_DIAG int int double double* int
						       double* int)))
  (cblas_dtrsm		(void cblas_dtrsm (CBLAS_ORDER CBLAS_SIDE CBLAS_UPLO CBLAS_TRANSPOSE
						       CBLAS_DIAG int int double double* int
						       double* int)))
  (cblas_cgemm		(void cblas_cgemm (CBLAS_ORDER CBLAS_TRANSPOSE CBLAS_TRANSPOSE int int
						       int void* void* int void* int
						       void* void* int)))
  (cblas_csymm		(void cblas_csymm (CBLAS_ORDER CBLAS_SIDE CBLAS_UPLO int int void* void* int
						       void* int void* void* int)))
  (cblas_csyrk		(void cblas_csyrk (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE int int
						       void* void* int void* void* int)))
  (cblas_csyr2k		(void cblas_csyr2k (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE int int
							void* void* int void* int void* void* int)))
  (cblas_ctrmm		(void cblas_ctrmm (CBLAS_ORDER CBLAS_SIDE CBLAS_UPLO CBLAS_TRANSPOSE
						       CBLAS_DIAG int int void* void* int
						       void* int)))
  (cblas_ctrsm		(void cblas_ctrsm (CBLAS_ORDER CBLAS_SIDE CBLAS_UPLO CBLAS_TRANSPOSE
						       CBLAS_DIAG int int void* void* int void* int)))
  (cblas_zgemm		(void cblas_zgemm (CBLAS_ORDER CBLAS_TRANSPOSE CBLAS_TRANSPOSE int int
						       int void* void* int void* int
						       void* void* int)))
  (cblas_zsymm		(void cblas_zsymm (CBLAS_ORDER CBLAS_SIDE CBLAS_UPLO int int
						       void* void* int void* int void* void* int)))
  (cblas_zsyrk		(void cblas_zsyrk (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE int int
						       void* void* int void* void* int)))
  (cblas_zsyr2k		(void cblas_zsyr2k (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE int int
							void* void* int void* int void* void* int)))
  (cblas_ztrmm		(void cblas_ztrmm (CBLAS_ORDER CBLAS_SIDE CBLAS_UPLO CBLAS_TRANSPOSE
						       CBLAS_DIAG int int void* void* int
						       void* int)))
  (cblas_ztrsm		(void cblas_ztrsm (CBLAS_ORDER CBLAS_SIDE CBLAS_UPLO CBLAS_TRANSPOSE
						       CBLAS_DIAG int int void* void* int void* int)))

  ;;Routines with prefixes C and Z only.
  (cblas_chemm		(void cblas_chemm (CBLAS_ORDER CBLAS_SIDE CBLAS_UPLO int int
						       void* void* int void* int void* void* int)))
  (cblas_cherk		(void cblas_cherk (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE int int
						       float void* int float void* int)))
  (cblas_cher2k		(void cblas_cher2k (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE int int
							void* void* int void* int float void* int)))
  (cblas_zhemm		(void cblas_zhemm (CBLAS_ORDER CBLAS_SIDE CBLAS_UPLO int int
						       void* void* int void* int void* void* int)))
  (cblas_zherk		(void cblas_zherk (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE int int
						       double void* int double void* int)))
  (cblas_zher2k		(void cblas_zher2k (CBLAS_ORDER CBLAS_UPLO CBLAS_TRANSPOSE int int
							void* void* int void* int double void* int)))

  ;;Variadic!!!
  ;;
  ;; (cblas_xerbla	 (void cblas_xerbla (int p, char *rout, char *form, ...)))
  )


;;;; done

)

;;; end of file
