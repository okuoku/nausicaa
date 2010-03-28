dnl (foreign math blas sizeof) --
dnl
dnl Part of: Nausicaa
dnl Contents: foreign library inspection generation
dnl Date: Sun Mar 28, 2010
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
dnl
dnl This program is free software:  you can redistribute it and/or modify
dnl it under the terms of the  GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or (at
dnl your option) any later version.
dnl
dnl This program is  distributed in the hope that it  will be useful, but
dnl WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
dnl MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
dnl General Public License for more details.
dnl
dnl You should  have received  a copy of  the GNU General  Public License
dnl along with this program.  If not, see <http://www.gnu.org/licenses/>.
dnl

AC_DEFUN([NAUSICAA_BLAS],[

NAUSICAA_INSPECT_TYPE([CBLAS_ORDER],[enum CBLAS_ORDER],[signed-int],[#f])

dnl enum CBLAS_ORDER
NAUSICAA_ENUM_VALUE([CblasRowMajor])
NAUSICAA_ENUM_VALUE([CblasColMajor])
NAUSICAA_INSPECT_TYPE([CBLAS_TRANSPOSE],[enum CBLAS_TRANSPOSE],[signed-int],[#f])

dnl enum CBLAS_TRANSPOSE
NAUSICAA_ENUM_VALUE([CblasNoTrans])
NAUSICAA_ENUM_VALUE([CblasTrans])
NAUSICAA_ENUM_VALUE([CblasConjTrans])
NAUSICAA_INSPECT_TYPE([CBLAS_UPLO],[enum CBLAS_UPLO],[signed-int],[#f])

dnl enum CBLAS_UPLO
NAUSICAA_ENUM_VALUE([CblasUpper])
NAUSICAA_ENUM_VALUE([CblasLower])
NAUSICAA_INSPECT_TYPE([CBLAS_DIAG],[enum CBLAS_DIAG],[signed-int],[#f])

dnl enum CBLAS_DIAG
NAUSICAA_ENUM_VALUE([CblasNonUnit])
NAUSICAA_ENUM_VALUE([CblasUnit])
NAUSICAA_INSPECT_TYPE([CBLAS_SIDE],[enum CBLAS_SIDE],[signed-int],[#f])

dnl enum CBLAS_SIDE
NAUSICAA_ENUM_VALUE([CblasLeft])
NAUSICAA_ENUM_VALUE([CblasRight])
NAU_DS_WITH_OPTION([BLAS_SHARED_OBJECT],[blas-shared-object],[libblas.so],
  [Blas shared library file],[select Blas shared library file])
NAU_DS_WITH_OPTION([CBLAS_SHARED_OBJECT],[cblas-shared-object],[libcblas.so],
  [Cblas shared library file],[select Cblas shared library file])


])


dnl end of file
