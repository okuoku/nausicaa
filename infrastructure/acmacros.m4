# @configure_input@
#
# Library file for the "configure.ac".  This file should be
# loaded  by  "configure.ac"  by  putting the  following  in
# "aclocal.m4":
#
#       m4_include(../infrastructure/configure.nau)
#

#page
## ------------------------------------------------------------
## Setup.
## ------------------------------------------------------------

# Notice that this one defines '_GNU_SOURCE' and others.
AC_USE_SYSTEM_EXTENSIONS

AC_SYS_INTERPRETER
AC_SYS_LARGEFILE
AC_SYS_LONG_FILE_NAMES
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Options.
## ------------------------------------------------------------

AC_MSG_CHECKING([whether compiled files will be built and installed])

AC_ARG_ENABLE([fasl],
    AC_HELP_STRING([--disable-fasl],
        [enable/disable installation of compiled libraries]),[
if test "$enableval" = yes ; then
  nausicaa_ENABLE_FASL=yes
else
  nausicaa_ENABLE_FASL=no
fi
],[nausicaa_ENABLE_FASL=yes])

AC_MSG_RESULT([$nausicaa_ENABLE_FASL])
AC_SUBST([nausicaa_ENABLE_FASL])

## ------------------------------------------------------------

AC_MSG_CHECKING([whether source files will be installed])

AC_ARG_ENABLE([sls],
    AC_HELP_STRING([--disable-sls],
        [enable/disable installation of source files libraries]),[
if test "$enableval" = yes ; then
  nausicaa_ENABLE_SLS=yes
else
  nausicaa_ENABLE_SLS=no
fi
],[nausicaa_ENABLE_SLS=yes])

AC_MSG_RESULT([$nausicaa_ENABLE_SLS])
AC_SUBST([nausicaa_ENABLE_SLS])

## ------------------------------------------------------------

AC_MSG_CHECKING([whether documentation files will be installed])

AC_ARG_ENABLE([doc],
    AC_HELP_STRING([--disable-doc],
        [enable/disable installation of documentation files]),[
if test "$enableval" = yes ; then
  nausicaa_ENABLE_DOC=yes
else
  nausicaa_ENABLE_DOC=no
fi
],[nausicaa_ENABLE_DOC=yes])

AC_MSG_RESULT([$nausicaa_ENABLE_DOC])
AC_SUBST([nausicaa_ENABLE_DOC])

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Common directories.
## ------------------------------------------------------------

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

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Common programs.
## ------------------------------------------------------------

AC_PROG_INSTALL
AC_PROG_MAKE_SET

## ------------------------------------------------------------

AC_DEFUN([DS_P_MAKE_PROGRAM_DEFUN],[
AC_DEFUN([DS_PROGRAM_$1],[AC_PATH_PROG([$1],[$2],[:])
AC_ARG_VAR([$1],[$3])])])

## ------------------------------------------------------------

DS_P_MAKE_PROGRAM_DEFUN([BASH_PROGRAM],[bash],[the GNU bash shell])
DS_P_MAKE_PROGRAM_DEFUN([BZIP],[bzip2],[the bzip2 compressor program])
DS_P_MAKE_PROGRAM_DEFUN([CAT],[cat],[the GNU cat program])
DS_P_MAKE_PROGRAM_DEFUN([CP],[cp],[copies files])
DS_P_MAKE_PROGRAM_DEFUN([DATE],[date],[a program that prints the current date])
DS_P_MAKE_PROGRAM_DEFUN([FIND],[find],[the GNU find program])
DS_P_MAKE_PROGRAM_DEFUN([GAWK],[gawk],[the GNU awk program])
DS_P_MAKE_PROGRAM_DEFUN([GREP],[grep],[the GNU grep program])
DS_P_MAKE_PROGRAM_DEFUN([GZIP],[gzip],[the gzip compressor program])
DS_P_MAKE_PROGRAM_DEFUN([M4],[m4],[the GNU m4 preprocessor])
DS_P_MAKE_PROGRAM_DEFUN([MAKEINFO],[makeinfo],[builds docs from Texinfo source])
DS_P_MAKE_PROGRAM_DEFUN([MKDIR],[mkdir],[creates directories recursively])
DS_P_MAKE_PROGRAM_DEFUN([MV],[mv],[move files around])
DS_P_MAKE_PROGRAM_DEFUN([RM],[rm],[deletes files and directories recursively])
DS_P_MAKE_PROGRAM_DEFUN([RMDIR],[rmdir],[deletes empty directories])
DS_P_MAKE_PROGRAM_DEFUN([SED],[sed],[the GNU sed program])
DS_P_MAKE_PROGRAM_DEFUN([SORT],[sort],[the GNU sort program])
DS_P_MAKE_PROGRAM_DEFUN([SUDO],[sudo],[the sudo superuser executor])
DS_P_MAKE_PROGRAM_DEFUN([SYMLINK],[ln],[program used create symbolic links])
DS_P_MAKE_PROGRAM_DEFUN([TAR],[tar],[the GNU tar program])

DS_P_MAKE_PROGRAM_DEFUN([IKARUS],[ikarus],[the Ikarus Scheme executable])
DS_P_MAKE_PROGRAM_DEFUN([SCHEME_SCRIPT],[scheme-script],[the scheme-script executable])

## ------------------------------------------------------------

DS_PROGRAM_BASH_PROGRAM
DS_PROGRAM_BZIP
DS_PROGRAM_CAT
DS_PROGRAM_CP
DS_PROGRAM_DATE
DS_PROGRAM_FIND
DS_PROGRAM_GAWK
DS_PROGRAM_GREP
DS_PROGRAM_GZIP
DS_PROGRAM_M4
DS_PROGRAM_MAKEINFO
DS_PROGRAM_MKDIR
DS_PROGRAM_MV
DS_PROGRAM_RM
DS_PROGRAM_RMDIR
DS_PROGRAM_SED
DS_PROGRAM_SORT
DS_PROGRAM_SUDO
DS_PROGRAM_SYMLINK
DS_PROGRAM_TAR

DS_PROGRAM_IKARUS
DS_PROGRAM_SCHEME_SCRIPT

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Done.
## ------------------------------------------------------------

AC_CONFIG_FILES([meta.d/slackware/slack-desc:meta/slackware/slack-desc.in])
AC_CONFIG_FILES([Makefile.begin:${srcdir}/infrastructure/Makefile.begin.in])
AC_CONFIG_FILES([Makefile.end:${srcdir}/infrastructure/Makefile.end.in])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT


### end of file
# Local Variables:
# mode: sh
# End:
