;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/BLAS
;;;Contents: compound library, high-level API
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


(library (foreign math blas)
  (export

    (rename (CblasRowMajor	row-major)
	    (CblasColMajor	col-major)
	    (CblasNoTrans	no-trans)
	    (CblasTrans		trans)
	    (CblasConjTrans	conj-trans)
	    (CblasUpper		upper)
	    (CblasLower		lower)
	    (CblasNonUnit	non-unit)
	    (CblasUnit		unit)
	    (CblasLeft		left)
	    (CblasRight		right))

    sdsdot			dsdot
    sdot			ddot

    ;;Functions having prefixes Z and C only.
    cdotu_sub			cdotc_sub
    zdotu_sub			zdotc_sub

    ;;Functions having prefixes S D SC DZ.
    snrm2			sasum
    dnrm2			dasum
    scnrm2			scasum
    dznrm2			dzasum

    ;;Functions having standard 4 prefixes (S D C Z).
    isamax			idamax
    icamax			izamax

;;;  level 1 BLAS routines

    ;;Routines with standard 4 prefixes (s, d, c, z).
    sswap			scopy
    saxpy			dswap
    dcopy			daxpy
    cswap			ccopy
    caxpy			zswap
    zcopy			zaxpy

    ;;Routines with S and D prefix only.
    srotg			srotmg
    srot			srotm
    drotg			drotmg
    drot			drotm

    ;;Routines with S D C Z CS and ZD prefixes.
    sscal			dscal
    cscal			zscal
    csscal			zdscal

;;;;  level 2 BLAS

    ;;Routines with standard 4 prefixes  (S, D, C, Z).
    sgemv			sgbmv
    strmv			stbmv
    stpmv			strsv
    stbsv			stpsv
    dgemv			dgbmv
    dtrmv			dtbmv
    dtpmv			dtrsv
    dtbsv			dtpsv
    cgemv			cgbmv
    ctrmv			ctbmv
    ctpmv			ctrsv
    ctbsv			ctpsv
    zgemv			zgbmv
    ztrmv			ztbmv
    ztpmv			ztrsv
    ztbsv			ztpsv

    ;;Routines with S and D prefixes only.
    ssymv			ssbmv
    sspmv			sger
    ssyr			sspr
    ssyr2			sspr2
    dsymv			dsbmv
    dspmv			dger
    dsyr			dspr
    dsyr2			dspr2

    ;;Routines with C and Z prefixes only.
    chemv			chbmv
    chpmv			cgeru
    cgerc			cher
    chpr			cher2
    chpr2			zhemv
    zhbmv			zhpmv
    zgeru			zgerc
    zher			zhpr
    zher2			zhpr2

;;;;  level 3 BLAS

    ;;Routines with standard 4 prefixes  (S, D, C, Z).
    sgemm			ssymm
    ssyrk			ssyr2k
    strmm			strsm
    dgemm			dsymm
    dsyrk			dsyr2k
    dtrmm			dtrsm
    cgemm			csymm
    csyrk			csyr2k
    ctrmm			ctrsm
    zgemm			zsymm
    zsyrk			zsyr2k
    ztrmm			ztrsm

    ;;Routines with prefixes C and Z only.
    chemm			cherk
    cher2k			zhemm
    zherk			zher2k)
  (import (rnrs)
    (foreign math blas primitives)
    (foreign math blas sizeof)))

;;; end of file
