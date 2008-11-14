# 
# Part of: Nausicaa
# Contents: autoconf macros
# Date: Thu Nov 13, 2008
# 
# Abstract
# 
# 
# 
# Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
# 
# This  program  is free  software:  you  can redistribute  it
# and/or modify it  under the terms of the  GNU General Public
# License as published by the Free Software Foundation, either
# version  3 of  the License,  or (at  your option)  any later
# version.
# 
# This  program is  distributed in  the hope  that it  will be
# useful, but  WITHOUT ANY WARRANTY; without  even the implied
# warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
# PURPOSE.   See  the  GNU  General Public  License  for  more
# details.
# 
# You should  have received a  copy of the GNU  General Public
# License   along   with    this   program.    If   not,   see
# <http://www.gnu.org/licenses/>.
# 

#page
## ------------------------------------------------------------
## Begin of NAUSICAA_BEGIN.
## ------------------------------------------------------------

AC_DEFUN([NAUSICAA_BEGIN],[

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Setup.
## ------------------------------------------------------------

AC_PREREQ(2.60)
AC_CONFIG_AUX_DIR([../infrastructure])

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
        [disable installation of precompiled libraries (default: enabled)]),[
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
    AC_HELP_STRING([--enable-sls],
        [enable installation of source files (default: disabled)]),[
if test "$enableval" = yes ; then
  nausicaa_ENABLE_SLS=yes
else
  nausicaa_ENABLE_SLS=no
fi
],[nausicaa_ENABLE_SLS=no])

AC_MSG_RESULT([$nausicaa_ENABLE_SLS])
AC_SUBST([nausicaa_ENABLE_SLS])

## ------------------------------------------------------------

AC_MSG_CHECKING([whether documentation files will be installed])

AC_ARG_ENABLE([doc],
    AC_HELP_STRING([--disable-doc],
        [disable installation of documentation files (default: enabled)]),[
if test "$enableval" = yes ; then
  nausicaa_ENABLE_DOC=yes
else
  nausicaa_ENABLE_DOC=no
fi
],[nausicaa_ENABLE_DOC=yes])

AC_MSG_RESULT([$nausicaa_ENABLE_DOC])
AC_SUBST([nausicaa_ENABLE_DOC])

## ------------------------------------------------------------

AC_MSG_CHECKING([whether documentation in Info format will be installed])

AC_ARG_ENABLE([doc-info],
    AC_HELP_STRING([--disable-doc-info],
        [disable installation of Info documentation (default: enabled)]),[
if test "$enableval" = yes ; then
  nausicaa_ENABLE_INFO_DOC=yes
else
  nausicaa_ENABLE_INFO_DOC=no
fi
],[nausicaa_ENABLE_INFO_DOC=yes])

AC_MSG_RESULT([$nausicaa_ENABLE_INFO_DOC])
AC_SUBST([nausicaa_ENABLE_INFO_DOC])

## ------------------------------------------------------------

AC_MSG_CHECKING([whether documentation in HTML format will be installed])

AC_ARG_ENABLE([doc-html],
    AC_HELP_STRING([--enable-doc-html],
        [enable installation of HTML documentation (default: disabled)]),[
if test "$enableval" = yes ; then
  nausicaa_ENABLE_HTML_DOC=yes
else
  nausicaa_ENABLE_HTML_DOC=no
fi
],[nausicaa_ENABLE_HTML_DOC=no])

AC_MSG_RESULT([$nausicaa_ENABLE_HTML_DOC])
AC_SUBST([nausicaa_ENABLE_HTML_DOC])

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

AC_PATH_PROG([BASH_PROGRAM],[bash],[:])
AC_ARG_VAR([BASH_PROGRAM],[the GNU bash shell])

AC_PATH_PROG([BZIP],[bzip2],[:])
AC_ARG_VAR([BZIP],[the bzip2 compressor program])

AC_PATH_PROG([CAT],[cat],[:])
AC_ARG_VAR([CAT],[the GNU cat program])

AC_PATH_PROG([CP],[cp],[:])
AC_ARG_VAR([CP],[copies files])

AC_PATH_PROG([DATE],[date],[:])
AC_ARG_VAR([DATE],[a program that prints the current date])

AC_PATH_PROG([FIND],[find],[:])
AC_ARG_VAR([FIND],[the GNU find program])

AC_PATH_PROG([GAWK],[gawk],[:])
AC_ARG_VAR([GAWK],[the GNU awk program])

AC_PATH_PROG([GREP],[grep],[:])
AC_ARG_VAR([GREP],[the GNU grep program])

AC_PATH_PROG([GZIP],[gzip],[:])
AC_ARG_VAR([GZIP],[the gzip compressor program])

AC_PATH_PROG([M4],[m4],[:])
AC_ARG_VAR([M4],[the GNU m4 preprocessor])

AC_PATH_PROG([MAKEINFO],[makeinfo],[:])
AC_ARG_VAR([MAKEINFO],[builds docs from Texinfo source])

AC_PATH_PROG([MKDIR],[mkdir],[:])
AC_ARG_VAR([MKDIR],[creates directories recursively])

AC_PATH_PROG([MV],[mv],[:])
AC_ARG_VAR([MV],[move files around])

AC_PATH_PROG([RM],[rm],[:])
AC_ARG_VAR([RM],[deletes files and directories recursively])

AC_PATH_PROG([RMDIR],[rmdir],[:])
AC_ARG_VAR([RMDIR],[deletes empty directories])

AC_PATH_PROG([SED],[sed],[:])
AC_ARG_VAR([SED],[the GNU sed program])

AC_PATH_PROG([SORT],[sort],[:])
AC_ARG_VAR([SORT],[the GNU sort program])

AC_PATH_PROG([SUDO],[sudo],[:])
AC_ARG_VAR([SUDO],[the sudo superuser executor])

AC_PATH_PROG([SYMLINK],[ln],[:])
AC_ARG_VAR([SYMLINK],[program used create symbolic links])

AC_PATH_PROG([TAR],[tar],[:])
AC_ARG_VAR([TAR],[the GNU tar program])

## ------------------------------------------------------------

AC_PATH_PROG([IKARUS],[ikarus],[:])
AC_ARG_VAR([IKARUS],[the Ikarus Scheme executable])

AC_PATH_PROG([SCHEME_SCRIPT],[scheme-script],[:])
AC_ARG_VAR([SCHEME_SCRIPT],[the scheme-script executable])

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## End of NAUSICAA_BEGIN.
## ------------------------------------------------------------

]) 

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Done.
## ------------------------------------------------------------

AC_DEFUN([NAUSICAA_END],[

AC_CONFIG_FILES([meta.d/slackware/slack-desc:meta/slackware/slack-desc.in])
AC_CONFIG_FILES([Makefile.begin:${srcdir}/infrastructure/Makefile.begin.in])
AC_CONFIG_FILES([Makefile.end:${srcdir}/infrastructure/Makefile.end.in])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT

])

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Other macros.
## ------------------------------------------------------------

# Synopsis:
#
#   IKARUS_CHECK_LIBRARY(<NAME>, [IMPORT-SPEC],
#                        [ACTION-IF-FOUND],
#                        [ACTION-IF-NOT-FOUND])
#
# Description:
#
#   Check the availability of the Ikarus library IMPORT-SPEC.
#   If found set the output variable 'HAS_IKARUS_LIB_<NAME>'
#   to 'yes', else set that variable to 'no'.
#
#   ACTION-IF-FOUND is executed if the library is found.
#
#   ACTION-IF-FOUND is executed if the library is not found.
#
# Prerequisites
#
#   The  variable 'IKARUS'  must  hold the  pathname of  the
#   'ikarus' executable.
#
# Example:
#
#   To test if '(list-lib)' is available:
#
#       IKARUS_CHECK_LIBRARY([LIST],[(list-lib)])
#
#   if it is: the output variable 'HAS_IKARUS_LIB_LIST' is
#   set to 'yes'.
#   
AC_DEFUN([IKARUS_CHECK_LIBRARY],[
AC_MSG_CHECKING([availability of Ikarus library $2])

# This comes from the documentation of GNU Autoconf.
#
# Create a temporary directory "$nausicaa_TMPDIR" in "$TMPDIR"
# (default "/tmp").  Use mktemp if possible; otherwise fall
# back on mkdir, with $RANDOM to make collisions less likely.
: ${TMPDIR=/tmp}
{
    nausicaa_TMPDIR=`
    (umask 077 && mktemp -d "$TMPDIR/fooXXXXXX") 2>/dev/null
    ` &&
    test -n "$nausicaa_TMPDIR" && test -d "$nausicaa_TMPDIR"
} || {
    nausicaa_TMPDIR=$TMPDIR/foo$$-$RANDOM
    (umask 077 && mkdir "$nausicaa_TMPDIR")
} || exit $?

nausicaa_TMPFILE=${nausicaa_TMPDIR}/find-lib.sls

nausicaa_ANSWER=`echo '(import (ikarus))

(let ((lib-name (read (open-string-input-port
                        (cadr (command-line))))))
  (with-exception-handler
    (lambda (ex)
      (display "no\n")
      (exit 1))
    (lambda ()
      (environment lib-name)))
  (display "yes\n")
  (exit))

' >"${nausicaa_TMPFILE}"

"${IKARUS}" --r6rs-script "${nausicaa_TMPFILE}" '$2'`
rm -fr "${nausicaa_TMPDIR}"

AC_MSG_RESULT([${nausicaa_ANSWER}])
if test "${nausicaa_ANSWER}" = yes ; then
# Action if found.
:
$3
else
# Action if not found.
:
$4
fi

AC_SUBST([HAS_IKARUS_LIB_$1],[$nausicaa_ANSWER])
])

## ------------------------------------------------------------


### end of file
# Local Variables:
# mode: sh
# End:
