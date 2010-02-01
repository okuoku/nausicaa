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
	    (cblas_srotg		srotg)
	    (cblas_srotmg		srotmg)
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
    (foreign memory)
    (foreign cstrings)
    (foreign math blas platform)
    (foreign math blas sizeof))





;;;; done

)

;;; end of file
