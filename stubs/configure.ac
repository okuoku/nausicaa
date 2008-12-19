dnl
dnl Part of: Nausicaa/Stubs
dnl Contents: configuration template file
dnl Date: Fri Dec 19, 2008
dnl Time-stamp: <2008-12-19 18:29:47 marco>
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
dnl ------------------------------------------------------------
dnl Setup.
dnl ------------------------------------------------------------

AC_INIT([nausicaa-stubs],[0.1d0])
AC_PREREQ(2.60)
AC_CONFIG_AUX_DIR([../infrastructure])
AC_CONFIG_SRCDIR([Makefile.in])

AC_SYS_INTERPRETER
AC_SYS_LARGEFILE
AC_SYS_LONG_FILE_NAMES
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

PKG_ID=${PACKAGE_NAME}-${PACKAGE_VERSION}
AC_SUBST([PKG_ID])


dnl page
dnl --------------------------------------------------------------------
dnl Options.
dnl --------------------------------------------------------------------

dnl 1 VARIABLE
dnl 2 IDENTIFIER
dnl 3 DEFAULT
dnl 4 CHECKING-DESCRIPTION
dnl 5 OPTION-DESCRIPTION
AC_DEFUN([NAUSICAA_ENABLE_OPTION],[AC_MSG_CHECKING([$4])
AC_ARG_ENABLE([$2],
   AC_HELP_STRING([--enable-$2],[$5 (default: $3)]),
   [if test "$enableval" = yes ; then $1=yes ; else $1=no ; fi],
   [$1=$3])
AC_MSG_RESULT([$[]$1])
AC_SUBST([$1])
])

NAUSICAA_ENABLE_OPTION([nausicaa_ENABLE_POSIX],[posix],[yes],
  [whether the POSIX stub library will be built and installed],
  [disable building the POSIX stub library])


dnl page
dnl ------------------------------------------------------------
dnl Common programs.
dnl ------------------------------------------------------------

AC_PROG_INSTALL
AC_PROG_MAKE_SET

dnl 1 VARIABLE
dnl 2 PROGRAM_NAME
dnl 3 PROGRAM_DESCRIPTION
AC_DEFUN([NAUSICAA_PROGRAM],[
AC_PATH_PROG([$1],[$2],[:])
AC_ARG_VAR([$1],[$3])
])

NAUSICAA_PROGRAM([BASH_PROGRAM],[bash],[the GNU bash shell])
NAUSICAA_PROGRAM([BZIP],[bzip2],[the bzip2 compressor program])
NAUSICAA_PROGRAM([CAT],[cat],[the GNU cat program])
NAUSICAA_PROGRAM([CP],[cp],[copies files])
NAUSICAA_PROGRAM([DATE],[date],[a program that prints the current date])
NAUSICAA_PROGRAM([FIND],[find],[the GNU find program])
NAUSICAA_PROGRAM([GAWK],[gawk],[the GNU awk program])
NAUSICAA_PROGRAM([GREP],[grep],[the GNU grep program])
NAUSICAA_PROGRAM([GZIP],[gzip],[the gzip compressor program])
NAUSICAA_PROGRAM([M4],[m4],[the GNU m4 preprocessor])
NAUSICAA_PROGRAM([MAKEINFO],[makeinfo],[builds docs from Texinfo source])
NAUSICAA_PROGRAM([MKDIR],[mkdir],[creates directories recursively])
NAUSICAA_PROGRAM([MV],[mv],[move files around])
NAUSICAA_PROGRAM([RM],[rm],[deletes files and directories recursively])
NAUSICAA_PROGRAM([RMDIR],[rmdir],[deletes empty directories])
NAUSICAA_PROGRAM([SED],[sed],[the GNU sed program])
NAUSICAA_PROGRAM([SORT],[sort],[the GNU sort program])
NAUSICAA_PROGRAM([SUDO],[sudo],[the sudo superuser executor])
NAUSICAA_PROGRAM([SYMLINK],[ln],[program used create symbolic links])
NAUSICAA_PROGRAM([TAR],[tar],[the GNU tar program])

dnl --------------------------------------------------------------------

AC_DEFUN([NAUSICAA_SLACKWARE_TOOLS],[
NAUSICAA_ENABLE_OPTION([ds_slackware_USE_PREFIX_TOOLS],
   [slackware-prefix-tools],[no],
   [whether Slackware tools under installation prefix will be used],
   [use Slackware tools under installation prefix])
nausicaa_private_PATH=${PATH}
if test "${ds_slackware_USE_PREFIX_TOOLS}" = yes ; then
  PATH=${prefix}/sbin:${PATH}
else
  PATH=/sbin:${PATH}
fi
NAUSICAA_PROGRAM([slack_MAKEPKG_PROGRAM],[makepkg],[the Slackware package maker])
NAUSICAA_PROGRAM([slack_INSTALLPKG_PROGRAM],[installpkg],[the Slackware package installer])
NAUSICAA_PROGRAM([slack_REMOVEPKG_PROGRAM],[removepkg],[the Slackware package remover])
NAUSICAA_PROGRAM([slack_UPGRADEPKG_PROGRAM],[upgradepkg],[the Slackware package upgrader])
PATH=${nausicaa_private_PATH}
])

AC_DEFUN([NAUSICAA_REDHAT_TOOLS],[
NAUSICAA_PROGRAM([redhat_BUILD_PROGRAM],[rpmbuild],[the RedHat package maker])
NAUSICAA_PROGRAM([redhat_CORE_PROGRAM],[rpm],[the RedHat package manager])
])

AC_DEFUN([NAUSICAA_PACMAN_TOOLS],[
NAUSICAA_PROGRAM([pacman_PROGRAM],[pacman],[the Pacman package manager])
])

NAUSICAA_SLACKWARE_TOOLS
NAUSICAA_REDHAT_TOOLS
NAUSICAA_PACMAN_TOOLS


dnl page
dnl --------------------------------------------------------------------
dnl Common directories.
dnl --------------------------------------------------------------------

AC_SUBST([PKG_ID],[\${PACKAGE_NAME}-\${PACKAGE_VERSION}])
AC_SUBST([PKG_DIR],[\${PACKAGE_NAME}/\${PACKAGE_VERSION}])
AC_SUBST([pkgdatadir],[\${datadir}/\${PKG_DIR}])
AC_SUBST([pkgdocdir],[\${docdir}/\${PKG_DIR}])
AC_SUBST([pkgexampledir],[\${pkgdocdir}/examples])
AC_SUBST([pkghtmldir],[\${pkgdocdir}/HTML])
AC_SUBST([pkginfodir],[\${pkgdocdir}/info])
AC_SUBST([pkgincludedir],[\${includedir}/\${PKG_DIR}])
AC_SUBST([pkglibdir],[\${libdir}/\${PKG_DIR}])
AC_SUBST([pkglibexecdir],[\${libexecdir}/\${PKG_DIR}])
AC_SUBST([pkgsysconfdir],[\${sysconfdir}/\${PKG_DIR}])


dnl page
dnl ------------------------------------------------------------
dnl C language basic stuff.
dnl ------------------------------------------------------------

AC_CONFIG_HEADERS(config.h)

AC_DEFINE([_GNU_SOURCE],[1],[GNU libc symbol, see its documentation])

dnl ------------------------------------------------------------

AC_PROG_CC
AC_PROG_CC_C_O
AC_PROG_CPP
AC_PROG_RANLIB
AC_PATH_PROG([AR],[ar],:)
AC_PATH_PROG([STRIP], [strip],:)
AC_PATH_PROG([GDB],[GDB],:)
AC_PROG_CC_C99

dnl  Headers and C compiler features.
AC_HEADER_STDC
AC_CHECK_HEADERS([assert.h])
AC_C_BIGENDIAN
AC_C_CHAR_UNSIGNED
AC_C_CONST
AC_C_FLEXIBLE_ARRAY_MEMBER
AC_C_INLINE
AC_C_TYPEOF
AC_C_RESTRICT
AC_C_VARARRAYS
AC_C_VOLATILE
AC_CHECK_TYPES([ptrdiff_t])
AC_TYPE_SIZE_T
AC_FUNC_MALLOC
AC_SUBST([NO_MINUS_C_MINUS_O])
AC_ARG_VAR([GNU_C_FLAGS],[fixed GNU C compiler flags])

AC_ARG_VAR([INCLUDES],[directory options for include files])
AC_ARG_VAR([LDFLAGS],[fixed linker flags])

AC_CACHE_SAVE

dnl page
dnl ------------------------------------------------------------
dnl Done.
dnl ------------------------------------------------------------

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([meta.d/slackware/slack-desc:meta/slackware/slack-desc.in])
AC_CONFIG_FILES([meta.d/redhat/spec-file:meta/redhat/spec-file.in])
AC_OUTPUT

dnl end of file
dnl Local Variables:
dnl mode: autoconf
dnl End:
