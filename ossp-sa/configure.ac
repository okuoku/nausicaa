dnl
dnl Part of: Nausicaa/OSSP/sa
dnl Contents: configuration template file
dnl Date: Tue Dec 16, 2008
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
dnl
dnl This  program  is free  software:  you  can redistribute  it
dnl and/or modify it  under the terms of the  GNU General Public
dnl License as published by the Free Software Foundation, either
dnl version  3 of  the License,  or (at  your option)  any later
dnl version.
dnl
dnl This  program is  distributed in  the hope  that it  will be
dnl useful, but  WITHOUT ANY WARRANTY; without  even the implied
dnl warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
dnl PURPOSE.   See  the  GNU  General Public  License  for  more
dnl details.
dnl
dnl You should  have received a  copy of the GNU  General Public
dnl License   along   with    this   program.    If   not,   see
dnl <http://www.gnu.org/licenses/>.
dnl

dnl page
dnl --------------------------------------------------------------------
dnl Setup.
dnl --------------------------------------------------------------------

AC_INIT([nausicaa-ossp-sa],[0.1d0])
NAUSICAA_BEGIN
NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_R6RS],[(r6rs)])
NAUSICAA_SCHEME_CHECK_LIBRARY([URIEL_LANG],[(uriel lang)])
NAUSICAA_SCHEME_CHECK_LIBRARY([URIEL_FOREIGN],[(uriel foreign)])
NAUSICAA_SYSTEM_SETUP
NAUSICAA_C_LANGUAGE
NAUSICAA_SIZEOF


dnl page
dnl --------------------------------------------------------------------
dnl Custom macros.
dnl --------------------------------------------------------------------

AC_DEFUN([NAU_SA_INSPECT_TYPE],
  [NAUSICAA_INSPECT_TYPE([$1],[$2],[$3],[#f],[#include <sa.h>])])

AC_DEFUN([NAU_SA_VALUEOF_TEST],
  [NAUSICAA_VALUEOF_TEST([$1],[$2],[#f],[#include <sa.h>])])

AC_DEFUN([NAU_SA_SIZEOF_TEST],
  [NAUSICAA_SIZEOF_TEST([$1],[$2],[#f],[#include <sa.h>])])


dnl page
dnl --------------------------------------------------------------------
dnl Inspection.
dnl --------------------------------------------------------------------

dnl Return types.
NAU_SA_INSPECT_TYPE([SA_RC_T],[sa_rc_t],[signed-int])
NAU_SA_VALUEOF_TEST([SA_OK],[SA_OK])
NAU_SA_VALUEOF_TEST([SA_ERR_ARG],[SA_ERR_ARG])
NAU_SA_VALUEOF_TEST([SA_ERR_USE],[SA_ERR_USE])
NAU_SA_VALUEOF_TEST([SA_ERR_MEM],[SA_ERR_MEM])
NAU_SA_VALUEOF_TEST([SA_ERR_MTC],[SA_ERR_MTC])
NAU_SA_VALUEOF_TEST([SA_ERR_EOF],[SA_ERR_EOF])
NAU_SA_VALUEOF_TEST([SA_ERR_TMT],[SA_ERR_TMT])
NAU_SA_VALUEOF_TEST([SA_ERR_SYS],[SA_ERR_SYS])
NAU_SA_VALUEOF_TEST([SA_ERR_NET],[SA_ERR_NET])
NAU_SA_VALUEOF_TEST([SA_ERR_FMT],[SA_ERR_FMT])
NAU_SA_VALUEOF_TEST([SA_ERR_IMP],[SA_ERR_IMP])
NAU_SA_VALUEOF_TEST([SA_ERR_INT],[SA_ERR_INT])

dnl Socket connection types.
NAU_SA_INSPECT_TYPE([SA_TYPE_T],[sa_type_t],[signed-int])
NAU_SA_VALUEOF_TEST([SA_TYPE_STREAM],[SA_TYPE_STREAM])
NAU_SA_VALUEOF_TEST([SA_TYPE_DATAGRAM],[SA_TYPE_DATAGRAM])

dnl Timeouts.
NAU_SA_INSPECT_TYPE([SA_TIMEOUT_T],[sa_timeout_t],[signed-int])
NAU_SA_VALUEOF_TEST([SA_TIMEOUT_ALL],[SA_TIMEOUT_ALL])
NAU_SA_VALUEOF_TEST([SA_TIMEOUT_ACCEPT],[SA_TIMEOUT_ACCEPT])
NAU_SA_VALUEOF_TEST([SA_TIMEOUT_CONNECT],[SA_TIMEOUT_CONNECT])
NAU_SA_VALUEOF_TEST([SA_TIMEOUT_READ],[SA_TIMEOUT_READ])
NAU_SA_VALUEOF_TEST([SA_TIMEOUT_WRITE],[SA_TIMEOUT_WRITE])

dnl Buffering.
NAU_SA_INSPECT_TYPE([SA_BUFFER_T],[sa_buffer_t],[signed-int])
NAU_SA_VALUEOF_TEST([SA_BUFFER_READ],[SA_BUFFER_READ])
NAU_SA_VALUEOF_TEST([SA_BUFFER_WRITE],[SA_BUFFER_WRITE])

dnl Options.
NAU_SA_INSPECT_TYPE([SA_OPTION_T],[sa_option_t],[signed-int])
NAU_SA_VALUEOF_TEST([SA_OPTION_NAGLE],[SA_OPTION_NAGLE])
NAU_SA_VALUEOF_TEST([SA_OPTION_LINGER],[SA_OPTION_LINGER])
NAU_SA_VALUEOF_TEST([SA_OPTION_REUSEADDR],[SA_OPTION_REUSEADDR])
NAU_SA_VALUEOF_TEST([SA_OPTION_REUSEPORT],[SA_OPTION_REUSEPORT])
NAU_SA_VALUEOF_TEST([SA_OPTION_NONBLOCK],[SA_OPTION_NONBLOCK])

dnl System calls.
NAU_SA_INSPECT_TYPE([SA_SYSCALL_T],[sa_syscall_t],[signed-int])
NAU_SA_VALUEOF_TEST([SA_SYSCALL_CONNECT],[SA_SYSCALL_CONNECT])
NAU_SA_VALUEOF_TEST([SA_SYSCALL_ACCEPT],[SA_SYSCALL_ACCEPT])
NAU_SA_VALUEOF_TEST([SA_SYSCALL_SELECT],[SA_SYSCALL_SELECT])
NAU_SA_VALUEOF_TEST([SA_SYSCALL_READ],[SA_SYSCALL_READ])
NAU_SA_VALUEOF_TEST([SA_SYSCALL_WRITE],[SA_SYSCALL_WRITE])
NAU_SA_VALUEOF_TEST([SA_SYSCALL_RECVFROM],[SA_SYSCALL_RECVFROM])
NAU_SA_VALUEOF_TEST([SA_SYSCALL_SENDTO],[SA_SYSCALL_SENDTO])

dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../libraries/ossp-sa/sizeof.sls:libraries/ossp-sa/sizeof.sls.in])
NAUSICAA_END


### end of file
