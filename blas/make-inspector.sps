;;;!mosh
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/BLAS
;;;Contents: foreign library inspection generator
;;;Date:
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
  (foreign ffi inspector-maker))


;;;; type definitions

(define-c-type-alias CBLAS_INDEX	size_t)

(define-c-enumeration CBLAS_ORDER
  "enum CBLAS_ORDER"
  CblasRowMajor
  CblasColMajor)

(define-c-enumeration CBLAS_TRANSPOSE
  "enum CBLAS_TRANSPOSE"
  CblasNoTrans
  CblasTrans
  CblasConjTrans)

(define-c-enumeration CBLAS_UPLO
  "enum CBLAS_UPLO"
  CblasUpper
  CblasLower)

(define-c-enumeration CBLAS_DIAG
  "enum CBLAS_DIAG"
  CblasNonUnit
  CblasUnit)

(define-c-enumeration CBLAS_SIDE
  "enum CBLAS_SIDE"
  CblasLeft
  CblasRight)


;;;; done

(define blas-library-spec
  '(foreign math blas sizeof))

(define-shared-object blas  libblas.so)
(define-shared-object cblas libcblas.so)

(autoconf-lib-write "configuration/blas-inspector.m4" blas-library-spec)
(sizeof-lib-write   "src/libraries/foreign/math/blas/sizeof.sls.in" blas-library-spec)

;;; end of file
