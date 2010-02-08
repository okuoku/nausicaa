dnl (foreign math lapack sizeof) --
dnl
dnl Part of: Nausicaa
dnl Contents: foreign library inspection generation
dnl Date: Mon Feb  8, 2010
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


NAUSICAA_INSPECT_TYPE([INTEGER],[integer],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([UINTEGER],[uinteger],[unsigned-int],[#f])
NAUSICAA_INSPECT_TYPE([SHORTINT],[shortint],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([REAL],[real],[float],[#f])
NAUSICAA_INSPECT_TYPE([DOUBLEREAL],[doublereal],[float],[#f])
NAUSICAA_INSPECT_TYPE([LOGICAL],[logical],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([SHORTLOGICAL],[shortlogical],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([LOGICAL1],[logical1],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([INTEGER1],[integer1],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([FTNLEN],[ftnlen],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([FLAG],[flag],[signed-int],[#f])
NAUSICAA_INSPECT_TYPE([FTNINT],[ftnint],[signed-int],[#f])

dnl Struct inspection: complex
NAUSICAA_INSPECT_STRUCT_TYPE([COMPLEX],[complex],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([COMPLEX_R],[complex],[r],[float])
NAUSICAA_INSPECT_FIELD_TYPE([COMPLEX_I],[complex],[i],[float])

dnl Struct inspection: doublecomplex
NAUSICAA_INSPECT_STRUCT_TYPE([DOUBLECOMPLEX],[doublecomplex],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([DOUBLECOMPLEX_R],[doublecomplex],[r],[float])
NAUSICAA_INSPECT_FIELD_TYPE([DOUBLECOMPLEX_I],[doublecomplex],[i],[float])
NAU_DS_WITH_OPTION([LAPACK_SHARED_OBJECT],[lapack-shared-object],[libclapack.so],
  [Lapack shared library file],[select Lapack shared library file])

dnl end of file
