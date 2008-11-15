dnl 
dnl Part of: Nausicaa
dnl Contents: autoconf macros
dnl Date: Thu Nov 13, 2008
dnl 
dnl Abstract
dnl 
dnl    This is  a library of GNU Autoconf  macros to be used  by all the
dnl    Nausicaa  "configure.ac" templates.   It  is enough  to create  a
dnl    symbolic link from the project directory to this file.
dnl 
dnl Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
dnl 
dnl This program is free software: you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free  Software Foundation, either  version 3 of the  License, or
dnl (at your option) any later version.
dnl 
dnl This program is distributed in the  hope that it will be useful, but
dnl WITHOUT  ANY   WARRANTY;  without  even  the   implied  warranty  of
dnl MERCHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE.  See  the GNU
dnl General Public License for more details.
dnl 
dnl You should  have received a copy  of the GNU  General Public License
dnl along      with      this       program.       If      not,      see
dnl <http://www.gnu.org/licenses/>.
dnl 

dnl page
dnl --------------------------------------------------------------------
dnl Helper macros.
dnl --------------------------------------------------------------------

dnl Synopsis:
dnl
dnl   NAUSICAA_ENABLE_OPTION(<1 variable>,<2 identifier>,<3 default>,
dnl                          <4 checking-description>,
dnl                          <5 option-description>)
dnl
dnl Description:
dnl   
dnl   Define an enable/disable command line option for "configure".  The
dnl   side effect is an output variable that is meant to be set to "yes"
dnl   or "no".
dnl
dnl Usage example:
dnl
dnl	NAUSICAA_ENABLE_OPTION([nausicaa_ENABLE_FASL],[fasl],[yes],
dnl	  [whether compiled files will be built and installed],
dnl	  [disable installation of precompiled libraries])
dnl
AC_DEFUN([NAUSICAA_ENABLE_OPTION],[
AC_MSG_CHECKING([$4])
AC_ARG_ENABLE([$2],AC_HELP_STRING([--disable-$2],[$5 (default: $3)]),[
if test "$enableval" = yes ; then
  $1=yes
else
  $1=no
fi
],[$1=$3])
AC_MSG_RESULT([$[]$1])
AC_SUBST([$1])
])

dnl Synopsis:
dnl
dnl   NAUSICAA_PROGRAM(<1 variable>,<2 program-name>,<3 description>)
dnl
dnl Description:
dnl
dnl   Find the full pathname of a program.  The side effect is an output
dnl   variable.
dnl
dnl Usage example:
dnl
dnl   NAUSICAA_PROGRAM([BASH_PROGRAM],[bash],[the GNU bash shell])
dnl   
AC_DEFUN([NAUSICAA_PROGRAM],[
AC_PATH_PROG([$1],[$2],[:])
AC_ARG_VAR([$1],[$3])
])

dnl Synopsis:
dnl
dnl	NAUSICAA_WITH_TMPFILE(<1 with-temp-file-chunk>,<2 after-chunk>)
dnl
dnl Description:
dnl
dnl	Execute a slab of code that uses a temporary file.
dnl
dnl	  The chunk in <with-temp-file-chunk> can use the temporary file
dnl	whose full pathname is  in the variable "nausicaa_TMPFILE".
dnl
dnl	  The  chunk  in  <after-chunk>  will  be  evaluated  after  the
dnl	temporary file has been removed, so it can safely report errors.
dnl
AC_DEFUN([NAUSICAA_WITH_TMPFILE],[

dnl Create  a   temporary  file  and   store  its  full   pathname  into
dnl "nausicaa_TMPFILE".  This chunk of code comes from the documentation
dnl of GNU Autoconf (so blame them, not me!).
dnl
dnl   Create  a  temporary  directory  "$nausicaa_TMPDIR"  in  "$TMPDIR"
dnl (default "/tmp").  Use "mktemp"  if possible; otherwise fall back on
dnl "mkdir", with $RANDOM to make collisions less likely.
: ${TMPDIR=/tmp}
{
    nausicaa_TMPDIR=`
    (umask 077 && mktemp -d "$TMPDIR/fooXXXXXX") 2>/dev/null
    ` &&
    test -n "${nausicaa_TMPDIR}" && test -d "${nausicaa_TMPDIR}"
} || {
    nausicaa_TMPDIR=${TMPDIR}/foo$$-$RANDOM
    (umask 077 && mkdir "${nausicaa_TMPDIR}")
} || exit $?

nausicaa_TMPFILE=${nausicaa_TMPDIR}/temporary

dnl --------------------------------------------------------------------
dnl Chunk with temporary file usage.

$1

rm -fr "${nausicaa_TMPDIR}"

dnl --------------------------------------------------------------------
dnl Chunk after temporary file usage.

$2

])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Begin of NAUSICAA_BEGIN.
dnl --------------------------------------------------------------------

AC_DEFUN([NAUSICAA_BEGIN],[

AC_PREREQ(2.60)
AC_CONFIG_AUX_DIR([../infrastructure])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Options.
dnl --------------------------------------------------------------------

NAUSICAA_ENABLE_OPTION([nausicaa_ENABLE_FASL],[fasl],[yes],
  [whether compiled files will be built and installed],
  [disable installation of precompiled libraries])

NAUSICAA_ENABLE_OPTION([nausicaa_ENABLE_SLS],[sls],[yes],
  [whether source files will be installed],
  [enable installation of source files])

NAUSICAA_ENABLE_OPTION([nausicaa_ENABLE_DOC],[doc],[yes],
  [whether documentation files will be installed],
  [disable installation of documentation files])

NAUSICAA_ENABLE_OPTION([nausicaa_ENABLE_INFO_DOC],[doc-info],[yes],
  [whether documentation in Info format will be installed],
  [disable installation of Info documentation])

NAUSICAA_ENABLE_OPTION([nausicaa_ENABLE_HTML_DOC],[doc-html],[yes],
  [whether documentation in HTML format will be installed],
  [disable installation of HTML documentation])

dnl --------------------------------------------------------------------

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

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Programs.
dnl --------------------------------------------------------------------

AC_PROG_INSTALL
AC_PROG_MAKE_SET

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

NAUSICAA_PROGRAM([IKARUS],[ikarus],[the Ikarus Scheme executable])
NAUSICAA_PROGRAM([SCHEME_SCRIPT],[scheme-script],[the scheme-script executable])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Packaging tools.
dnl --------------------------------------------------------------------

dnl Find Slackware package management tools.

nausicaa_PATH=${PATH}
PATH=/sbin:${PATH}

NAUSICAA_PROGRAM([slack_MAKEPKG_PROGRAM],[makepkg],[the Slackware package maker])
NAUSICAA_PROGRAM([slack_INSTALLPKG_PROGRAM],[installpkg],[the Slackware package installer])
NAUSICAA_PROGRAM([slack_REMOVEPKG_PROGRAM],[removepkg],[the Slackware package remover])
NAUSICAA_PROGRAM([slack_UPGRADEPKG_PROGRAM],[upgradepkg],[the Slackware package upgrader])

PATH=${nausicaa_PATH}

dnl --------------------------------------------------------------------
dnl Find RedHat package management tools.

NAUSICAA_PROGRAM([redhat_BUILD_PROGRAM],[rpmbuild],[the RedHat package maker])
NAUSICAA_PROGRAM([redhat_CORE_PROGRAM],[rpm],[the RedHat package manager])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl End of NAUSICAA_BEGIN.
dnl --------------------------------------------------------------------

]) 

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_DEFUN([NAUSICAA_END],[

AC_CONFIG_FILES([meta.d/slackware/slack-desc:meta/slackware/slack-desc.in])
AC_CONFIG_FILES([meta.d/redhat/spec-file:meta/redhat/spec-file.in])

AC_CONFIG_FILES([Makefile.begin:${srcdir}/infrastructure/Makefile.begin.in])
AC_CONFIG_FILES([Makefile.end:${srcdir}/infrastructure/Makefile.end.in])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT

])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Other macros.
dnl --------------------------------------------------------------------

dnl Synopsis:
dnl
dnl   IKARUS_CHECK_LIBRARY(<NAME>, [IMPORT-SPEC],
dnl                        [ACTION-IF-FOUND],
dnl                        [ACTION-IF-NOT-FOUND])
dnl
dnl Description:
dnl
dnl   Check  the availability  of  the Ikarus  library IMPORT-SPEC.   If
dnl   found  set the output  variable 'HAS_IKARUS_LIB_<NAME>'  to 'yes',
dnl   else set that variable to 'no'.
dnl
dnl   ACTION-IF-FOUND is executed if the library is found.
dnl
dnl   ACTION-IF-FOUND is executed if the library is not found.
dnl
dnl Prerequisites
dnl
dnl   The  variable 'IKARUS'  must  hold the  pathname  of the  'ikarus'
dnl   executable.
dnl
dnl Example:
dnl
dnl   To test if '(list-lib)' is available:
dnl
dnl       IKARUS_CHECK_LIBRARY([LIST],[(list-lib)])
dnl
dnl   if  it is:  the output  variable 'HAS_IKARUS_LIB_LIST'  is  set to
dnl   'yes'.
dnl   
AC_DEFUN([IKARUS_CHECK_LIBRARY],[
AC_MSG_CHECKING([availability of Ikarus library $2])

NAUSICAA_WITH_TMPFILE([
dnl Chunk with with temporary file pathname in "${nausicaa_TMPFILE}".

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
],[
dnl Chunk after remporary file removal.

AC_MSG_RESULT([${nausicaa_ANSWER}])
if test "${nausicaa_ANSWER}" = yes ; then
dnl Action if found.
:
$3
else
dnl Action if not found.
:
$4
fi

AC_SUBST([HAS_IKARUS_LIB_$1],[$nausicaa_ANSWER])
])

])

dnl --------------------------------------------------------------------


dnl end of file
