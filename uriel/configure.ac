##
## Part of: Nausicaa/Uriel
## Contents: configuration template script
## Date: Fri Dec 26, 2008
##
## Abstract
##
##
##
## Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
##
## This program is free software:  you can redistribute it and/or modify
## it under the terms of the  GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or (at
## your option) any later version.
##
## This program is  distributed in the hope that it  will be useful, but
## WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
## MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
## General Public License for more details.
##
## You should  have received  a copy of  the GNU General  Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
##

AC_INIT([nausicaa-uriel],[0.1d0])
NAUSICAA_BEGIN
NAUSICAA_SYSTEM_SETUP
NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_R6RS],[(r6rs)])
NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_LANG_LIB],[(lang-lib)])
NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_ENV_LIB],[(env-lib)])
NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_LIST_LIB],[(list-lib)])
NAUSICAA_C_LANGUAGE
NAUSICAA_SIZEOF
NAUSICAA_C_TYPES_LIMITS
NAUSICAA_C_ERRNO
AC_CONFIG_FILES([../libraries/uriel/ffi/sizeof.sls:libraries/uriel/ffi/sizeof.sls.in])
AC_CONFIG_FILES([../libraries/uriel/errno.sls:libraries/uriel/errno.sls.in])
NAUSICAA_END


### end of file
