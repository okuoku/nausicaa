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
dnl   DS_ENABLE_OPTION(<1 variable>,<2 identifier>,<3 default>,
dnl                    <4 checking-description>,
dnl                    <5 option-description>)
dnl
dnl Description:
dnl   
dnl   Define an enable/disable command line option for "configure".  The
dnl   side effect is an output variable that is meant to be set to "yes"
dnl   or "no".
dnl
dnl Usage example:
dnl
dnl	DS_ENABLE_OPTION([nausicaa_ENABLE_FASL],[fasl],[yes],
dnl	  [whether compiled files will be built and installed],
dnl	  [disable installation of precompiled libraries])
dnl
AC_DEFUN([DS_ENABLE_OPTION],[
AC_MSG_CHECKING([$4])
AC_ARG_ENABLE([$2],AC_HELP_STRING([--enable-$2],[$5 (default: $3)]),[
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
dnl   DS_PROGRAM(<1 variable>,<2 program-name>,<3 description>)
dnl
dnl Description:
dnl
dnl   Find the full pathname of a program.  The side effect is an output
dnl   variable.
dnl
dnl Usage example:
dnl
dnl   DS_PROGRAM([BASH_PROGRAM],[bash],[the GNU bash shell])
dnl   
AC_DEFUN([DS_PROGRAM],[
AC_PATH_PROG([$1],[$2],[:])
AC_ARG_VAR([$1],[$3])
])

dnl Synopsis:
dnl
dnl   DS_WITH_TMPFILE(<1 with-temp-file-chunk>,<2 after-chunk>)
dnl
dnl Description:
dnl
dnl   Execute a chunk of code that  uses a temporary file.  The chunk in
dnl   <with-temp-file-chunk>  can  use  the  temporary file  whose  full
dnl   pathname is  "${ds_TMPFILE}".  The chunk in  <after-chunk> will be
dnl   evaluated after  the temporary  file has been  removed, so  it can
dnl   safely report errors and terminate the script.
dnl
dnl     The  code  that  creates  the  temporary  file  comes  from  the
dnl   documentation of GNU Autoconf (so blame them, not me!); it makes a
dnl   temporary  directory under "$TMPDIR"  (which defaults  to "/tmp").
dnl   Use  "mktemp" if possible;  otherwise fall  back on  "mkdir", with
dnl   "$RANDOM" to make collisions less likely.
dnl
AC_DEFUN([DS_WITH_TMPFILE],[

dnl This initialises TMPDIR to "/tmp" if not already set to something.
: ${TMPDIR=/tmp}
{
    ds_TMPDIR=`
    (umask 077 && mktemp -d "$TMPDIR/fooXXXXXX") 2>/dev/null
    ` &&
    test -n "${ds_TMPDIR}" && test -d "${ds_TMPDIR}"
} || {
    ds_TMPDIR=${TMPDIR}/foo$$-$RANDOM
    (umask 077 && mkdir "${ds_TMPDIR}")
} || exit $?

ds_TMPFILE=${ds_TMPDIR}/temporary.txt

dnl --------------------------------------------------------------------
dnl Chunk with temporary file usage.

$1

rm -fr "${ds_TMPDIR}"

dnl --------------------------------------------------------------------
dnl Chunk after temporary file usage.

$2

])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Common blocks.
dnl --------------------------------------------------------------------

dnl Synopsis:
dnl
dnl   DS_COMMON_PROGRAMS()
dnl
dnl Description:
dnl
dnl   Initialises  a  set  of  variables  to  the  pathnames  of  common
dnl   programs.   Even  if  not  all  these programs  are  used  in  the
dnl   Makefile, it does not hurt to check for them.
dnl
AC_DEFUN([DS_COMMON_PROGRAMS],[
AC_PROG_INSTALL
AC_PROG_MAKE_SET
DS_PROGRAM([BASH_PROGRAM],[bash],[the GNU bash shell])
DS_PROGRAM([BZIP],[bzip2],[the bzip2 compressor program])
DS_PROGRAM([CAT],[cat],[the GNU cat program])
DS_PROGRAM([CP],[cp],[copies files])
DS_PROGRAM([DATE],[date],[a program that prints the current date])
DS_PROGRAM([FIND],[find],[the GNU find program])
DS_PROGRAM([GAWK],[gawk],[the GNU awk program])
DS_PROGRAM([GREP],[grep],[the GNU grep program])
DS_PROGRAM([GZIP],[gzip],[the gzip compressor program])
DS_PROGRAM([M4],[m4],[the GNU m4 preprocessor])
DS_PROGRAM([MAKEINFO],[makeinfo],[builds docs from Texinfo source])
DS_PROGRAM([MKDIR],[mkdir],[creates directories recursively])
DS_PROGRAM([MV],[mv],[move files around])
DS_PROGRAM([RM],[rm],[deletes files and directories recursively])
DS_PROGRAM([RMDIR],[rmdir],[deletes empty directories])
DS_PROGRAM([SED],[sed],[the GNU sed program])
DS_PROGRAM([SORT],[sort],[the GNU sort program])
DS_PROGRAM([SUDO],[sudo],[the sudo superuser executor])
DS_PROGRAM([SYMLINK],[ln],[program used create symbolic links])
DS_PROGRAM([TAR],[tar],[the GNU tar program])
])

dnl Synopsis:
dnl
dnl   DS_COMMON_DIRECTORIES()
dnl
dnl Description:
dnl
dnl   Initialises a bunch  of variables representing useful installation
dnl   pathnames.
dnl
AC_DEFUN([DS_COMMON_DIRECTORIES],[
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
])

dnl Synopsis:
dnl
dnl   DS_SLACKWARE_TOOLS()
dnl
dnl Description:
dnl
dnl   Find Slackware package management tools.
dnl
AC_DEFUN([DS_SLACKWARE_TOOLS],[

DS_ENABLE_OPTION(ds_slackware_USE_PREFIX_TOOLS,slackware-prefix-tools,no,
		 [whether Slackware tools under installation prefix will be used],
                 [use Slackware tools under installation prefix])

ds_PATH=${PATH}
if test "${ds_slackware_USE_PREFIX_TOOLS}" = yes ; then
  PATH=${prefix}/sbin:${PATH}
else
  PATH=/sbin:${PATH}
fi

DS_PROGRAM([slack_MAKEPKG_PROGRAM],[makepkg],[the Slackware package maker])
DS_PROGRAM([slack_INSTALLPKG_PROGRAM],[installpkg],[the Slackware package installer])
DS_PROGRAM([slack_REMOVEPKG_PROGRAM],[removepkg],[the Slackware package remover])
DS_PROGRAM([slack_UPGRADEPKG_PROGRAM],[upgradepkg],[the Slackware package upgrader])

PATH=${ds_PATH}
])

dnl Synopsis:
dnl
dnl   DS_REDHAT_TOOLS()
dnl
dnl Description:
dnl
dnl   Find RedHat package management tools.
dnl
AC_DEFUN([DS_REDHAT_TOOLS],[
DS_PROGRAM([redhat_BUILD_PROGRAM],[rpmbuild],[the RedHat package maker])
DS_PROGRAM([redhat_CORE_PROGRAM],[rpm],[the RedHat package manager])
])

dnl Synopsis:
dnl
dnl   DS_PACMAN_TOOLS()
dnl
dnl Description:
dnl
dnl   Find Pacman package management tools.
dnl
AC_DEFUN([DS_PACMAN_TOOLS],[
DS_PROGRAM([pacman_PROGRAM],[pacman],[the Pacman package manager])
])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Nausicaa specific blocks.
dnl --------------------------------------------------------------------

dnl Synopsis:
dnl
dnl   NAUSICAA_BEGIN()
dnl   ... your directives ...
dnl   NAUSICAA_END()
dnl
dnl Description
AC_DEFUN([NAUSICAA_BEGIN],[
AC_PREREQ(2.60)
AC_CONFIG_AUX_DIR([../infrastructure])
AC_CONFIG_SRCDIR([libraries/compile-all.sps])
DS_OPTIONS()
NAUSICAA_OPTIONS()
DS_COMMON_PROGRAMS()
DS_COMMON_DIRECTORIES()
DS_PROGRAM([IKARUS],[ikarus],[the Ikarus Scheme executable])
DS_PROGRAM([SCHEME_SCRIPT],[scheme-script],[the scheme-script executable])
DS_SLACKWARE_TOOLS()
DS_REDHAT_TOOLS()
DS_PACMAN_TOOLS()
]) 
AC_DEFUN([NAUSICAA_END],[
AC_CONFIG_FILES([meta.d/slackware/slack-desc:meta/slackware/slack-desc.in])
AC_CONFIG_FILES([meta.d/redhat/spec-file:meta/redhat/spec-file.in])
AC_CONFIG_FILES([Makefile.begin:${srcdir}/infrastructure/Makefile.begin.in])
AC_CONFIG_FILES([Makefile.end:${srcdir}/infrastructure/Makefile.end.in])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
])

dnl Synopsis:
dnl
dnl   DS_OPTIONS()
dnl
dnl Description:
dnl
dnl   Define the "configure" command  line option for the Nausicaa build
dnl   infrastructure.
dnl
AC_DEFUN([DS_OPTIONS],[
DS_ENABLE_OPTION([ds_config_ENABLE_DOC],[doc],[yes],
  [whether documentation files will be installed],
  [enable installation of documentation files])
DS_ENABLE_OPTION([ds_config_ENABLE_DOC_INFO],[doc-info],[yes],
  [whether documentation in Info format will be installed],
  [enable installation of Info documentation])
DS_ENABLE_OPTION([ds_config_ENABLE_DOC_HTML],[doc-html],[yes],
  [whether documentation in HTML format will be installed],
  [enable installation of HTML documentation])
])

dnl Synopsis:
dnl
dnl   NAUSICAA_OPTIONS()
dnl
dnl Description:
dnl
dnl   Define the "configure" command  line option for the Nausicaa build
dnl   infrastructure.
dnl
AC_DEFUN([NAUSICAA_OPTIONS],[
DS_ENABLE_OPTION([nausicaa_ENABLE_FASL],[fasl],[yes],
  [whether compiled files will be built and installed],
  [enable installation of precompiled libraries])
DS_ENABLE_OPTION([nausicaa_ENABLE_SLS],[sls],[yes],
  [whether source files will be installed],
  [enable installation of source files])
])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Macros for Ikarus Scheme.
dnl --------------------------------------------------------------------

dnl Synopsis:
dnl
dnl   DS_WITH_OUTPUT_FROM_IKARUS_SCRIPT(<1 script>,
dnl                                     <2 command line>,<3 code>)
dnl
dnl Description:
dnl
dnl   Evaluate an Ikarus <script> by  storing it in a temporary file and
dnl   invoking  "${IKARUS}  --r6rs-script"  with  <command  line>,  then
dnl   evaluate <code>.  The script is expected to output a line of text,
dnl   which will be stored in "${ds_ANSWER}", so that <code> can examine
dnl   it.
dnl
dnl     The script must have no quotes.
dnl
AC_DEFUN([DS_WITH_OUTPUT_FROM_IKARUS_SCRIPT],[
DS_WITH_TMPFILE([
dnl Chunk with with temporary file pathname in "${ds_TMPFILE}".

ds_ANSWER=`echo '$1' >"${ds_TMPFILE}"

"${IKARUS}" --r6rs-script "${ds_TMPFILE}" $2`
],[
dnl Chunk that can read the output in "${ds_ANSWER}".
$3
])
])

dnl Synopsis:
dnl
dnl   DS_IKARUS_CHECK_LIBRARY(<NAME>, [IMPORT-SPEC],
dnl                           [ACTION-IF-FOUND],
dnl                           [ACTION-IF-NOT-FOUND])
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
AC_DEFUN([DS_IKARUS_CHECK_LIBRARY],[
AC_MSG_CHECKING([availability of Ikarus library $2])

DS_WITH_OUTPUT_FROM_IKARUS_SCRIPT([(import (ikarus))

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

],['$2'],

AC_MSG_RESULT([${ds_ANSWER}])
if test "${ds_ANSWER}" = yes ; then
dnl Action if found.
:
$3
else
dnl Action if not found.
:
$4
fi

AC_SUBST([HAS_IKARUS_LIB_$1],[$ds_ANSWER])
)
])


dnl --------------------------------------------------------------------


dnl end of file
dnl Local Variables:
dnl mode: autoconf
dnl End:
