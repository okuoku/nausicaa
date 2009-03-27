dnl
dnl Part of: Nausicaa/Stubs
dnl Contents: configuration template file
dnl Date: Fri Dec 19, 2008
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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
dnl ------------------------------------------------------------
dnl Setup.
dnl ------------------------------------------------------------

AC_INIT([nausicaa-stubs],[0.1d0],[mrc.mgg@gna.org])
DS_BEGIN([./src/nausicaa-posix.c])

DS_ENABLE_OPTION([nausicaa_ENABLE_POSIX],[posix],[yes],
  [whether the POSIX stub library will be built and installed],
  [disable building the POSIX stub library])

DS_C99_LANGUAGE_COMMON_SETUP
DS_EXCLUDE_SECTION([DOC_RULES])
DS_EXCLUDE_SECTION([DEV_RULES])
DS_EXCLUDE_SECTION([CONFIG_INSPECTION_SCRIPT])
DS_EXCLUDE_SECTION([AUTOCONF])
DS_EXCLUDE_SECTION([PKGCONFIG])
DS_C_LANGUAGE_LIBRARY([posix],1,0,[nausicaa-posix])

#DS_CONFIG_FILE_PKGCONFIG
#DS_CONFIG_FILE_INSPECTION_SCRIPT
DS_CONFIG_FILE_SLACKWARE_BIN_DESC
#DS_CONFIG_FILE_SLACKWARE_DOC_DESC
#DS_CONFIG_FILE_SLACKWARE_DEV_DESC
DS_END

dnl end of file
dnl Local Variables:
dnl mode: autoconf
dnl End:
