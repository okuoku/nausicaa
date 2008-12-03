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
dnl AC_SUBST([pkglibdir],[\${libdir}/\${PKG_DIR}])
AC_SUBST([pkglibdir],[\${libdir}/scheme])
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
DS_PROGRAM([YPSILON],[ypsilon],[another R6RS Scheme])
DS_SLACKWARE_TOOLS()
DS_REDHAT_TOOLS()
DS_PACMAN_TOOLS()
])
AC_DEFUN([NAUSICAA_END],[
AC_CONFIG_FILES([meta.d/slackware/slack-desc:meta/slackware/slack-desc.in])
AC_CONFIG_FILES([meta.d/redhat/spec-file:meta/redhat/spec-file.in])
dnl AC_CONFIG_FILES([Makefile.begin:${srcdir}/infrastructure/Makefile.begin.in])
dnl AC_CONFIG_FILES([Makefile.end:${srcdir}/infrastructure/Makefile.end.in])
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
DS_ENABLE_OPTION([ds_config_ENABLE_DOC_HTML],[doc-html],[no],
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
dnl       DS_IKARUS_CHECK_LIBRARY([LIST],[(list-lib)])
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

dnl page
dnl --------------------------------------------------------------------
dnl System inspection.
dnl --------------------------------------------------------------------

dnl Synopsis:
dnl
dnl   NAUSICAA_SYSTEM_SETUP()
dnl
dnl Description:
dnl
dnl   Setup what is needed to  inspect the system.  Useful (but probably
dnl   not  mandatory) when  we need  to use  the C  compiler  to inspect
dnl   system facilities; not needed in the other cases.
dnl
AC_DEFUN([NAUSICAA_SYSTEM_SETUP],[
# Notice that this one defines '_GNU_SOURCE' and others.
AC_USE_SYSTEM_EXTENSIONS

AC_SYS_INTERPRETER
#AC_SYS_LARGEFILE
AC_SYS_LONG_FILE_NAMES
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
])

dnl Synopsis:
dnl
dnl   NAUSICAA_C_LANGUAGE()
dnl
dnl Description:
dnl
dnl   Setup the C language environment.   Useful when we need to use the
dnl   C compiler to inspect system and libraries facilities.
dnl
AC_DEFUN([NAUSICAA_C_LANGUAGE],[
AC_PROG_CC
AC_PROG_CC_C99
AC_HEADER_STDC
])

dnl Synopsis:
dnl
dnl   NAUSICAA_VALUEOF_TEST(<1 SUFFIX>,<2 EXPR>,
dnl                         <3 DEFAULT>,<4 HEADERS>)
dnl
dnl Description:
dnl
dnl   A wrapper for "AC_COMPUTE_INT" that acquires the value of a C
dnl   language expression, which must be an integer.  It is typically
dnl   used to acquire the value of a constant like "INT_MAX".
dnl
dnl   The output variable "VALUEOF_<SUFFIX>" is set to the result.
dnl   If the test fails: the value of the output variable will be
dnl   "<DEFAULT>".
dnl
dnl   If not empty: <HEADERS> must be a chunk of code including header
dnl   files.
dnl
AC_DEFUN([NAUSICAA_VALUEOF_TEST],[
AC_MSG_CHECKING([the value of '$2'])
AC_COMPUTE_INT([VALUEOF_$1],[$2],[AC_INCLUDES_DEFAULT
#include <limits.h>
#include <stdint.h>
$4],[VALUEOF_$1=$3])
AC_SUBST([VALUEOF_$1])
AC_MSG_RESULT([${VALUEOF_$1}])
])

dnl Synopsis:
dnl
dnl   NAUSICAA_SIZEOF_TEST(<SUFFIX>,<TYPEDEF>,<DEFAULT>)
dnl
dnl Description:
dnl
dnl   A wrapper for "AC_COMPUTE_INT" that acquires the size of a C
dnl   language type.  The output variable "SIZEOF_<SUFFIX>" will be
dnl   set to the result.  If the test fails the value of the output
dnl   variable will be "<DEFAULT>".
dnl
AC_DEFUN([NAUSICAA_SIZEOF_TEST],[
AC_MSG_CHECKING([the sizeof of '$2'])
AC_COMPUTE_INT([SIZEOF_$1],[sizeof($2)],[AC_INCLUDES_DEFAULT
#include <limits.h>
#include <stdint.h>],[SIZEOF_$1=$3])
AC_SUBST([SIZEOF_$1])
AC_MSG_RESULT([${SIZEOF_$1}])
])

dnl Synopsis:
dnl
dnl   NAUSICAA_SIZEOF()
dnl
dnl Description:
dnl
dnl   Perform  a  series of  tests  to  determine characteristic  system
dnl   constants.
dnl
AC_DEFUN([NAUSICAA_SIZEOF],[
NAUSICAA_SIZEOF_TEST([SHORT_INT],[short int],[#f])
NAUSICAA_SIZEOF_TEST([INT],[int],[#f])
NAUSICAA_SIZEOF_TEST([LONG],[long],[#f])
NAUSICAA_SIZEOF_TEST([LLONG],[long long],[#f])
NAUSICAA_SIZEOF_TEST([POINTER],[void *],[#f])

NAUSICAA_VALUEOF_TEST([CHAR_MAX],[CHAR_MAX],[#f])
NAUSICAA_VALUEOF_TEST([CHAR_MIN],[CHAR_MIN],[#f])
NAUSICAA_VALUEOF_TEST([SCHAR_MAX],[SCHAR_MAX],[#f])
NAUSICAA_VALUEOF_TEST([SCHAR_MIN],[SCHAR_MIN],[#f])
NAUSICAA_VALUEOF_TEST([UCHAR_MAX],[UCHAR_MAX],[#f])
NAUSICAA_VALUEOF_TEST([SHRT_MAX],[SHRT_MAX],[#f])
NAUSICAA_VALUEOF_TEST([SHRT_MIN],[SHRT_MIN],[#f])
NAUSICAA_VALUEOF_TEST([USHRT_MAX],[USHRT_MAX],[#f])
NAUSICAA_VALUEOF_TEST([INT_MAX],[INT_MAX],[#f])
NAUSICAA_VALUEOF_TEST([INT_MIN],[INT_MIN],[#f])
NAUSICAA_VALUEOF_TEST([UINT_MAX],[UINT_MAX],[#f])
NAUSICAA_VALUEOF_TEST([LONG_MAX],[LONG_MAX],[#f])
NAUSICAA_VALUEOF_TEST([LONG_MIN],[LONG_MIN],[#f])
NAUSICAA_VALUEOF_TEST([ULONG_MAX],[ULONG_MAX],[#f])
NAUSICAA_VALUEOF_TEST([LLONG_MAX],[LLONG_MAX],[#f])
NAUSICAA_VALUEOF_TEST([LLONG_MIN],[LLONG_MIN],[#f])
NAUSICAA_VALUEOF_TEST([ULLONG_MAX],[ULLONG_MAX],[#f])
NAUSICAA_VALUEOF_TEST([WCHAR_MAX],[WCHAR_MAX],[#f])
NAUSICAA_VALUEOF_TEST([SSIZE_MAX],[SSIZE_MAX],[#f])

AC_C_BIGENDIAN([AC_SUBST(WORDS_BIGENDIAN,1)],[AC_SUBST(WORDS_BIGENDIAN,0)])
])

dnl Synopsis:
dnl
dnl   NAUSICAA_INTTYPE_TEST(<SUFFIX>,<TYPEDEF>)
dnl
dnl Description:
dnl
dnl   Determines the equivalent integer type of the C language type
dnl   "<TYPEDEF>" among the set: "short int", "int", "long",
dnl   "long long".
dnl
dnl   The search is performed by comparing the value of the variable
dnl   "SIZEOF_<SUFFIX>" with the values of the variables "SIZEOF_INT",
dnl   "SIZEOF_SHORT_INT", "SIZEOF_LONG", "SIZEOF_LLONG".
dnl
dnl   The variable "SIZEOF_<SUFFIX>" should have been set in precedence
dnl   using "NAUSICAA_SIZEOF_TEST" or an equivalent macro.  The other
dnl   variables are defined by "NAUSICAA_SIZEOF".
dnl
dnl   The output variable "TYPEOF_<SUFFIX>" is set to the result.
dnl
AC_DEFUN([NAUSICAA_INTTYPE_TEST],[
AC_REQUIRE([NAUSICAA_SIZEOF])
AC_MSG_CHECKING([equivalent integer type of '$2'])
if test "${SIZEOF_$1}" = "${SIZEOF_INT}" ; then
   TYPEOF_$1=int
elif test "${SIZEOF_$1}" = "${SIZEOF_SHORT_INT}" ; then
   TYPEOF_$1='short int'
elif test "${SIZEOF_$1}" = "${SIZEOF_LONG}" ; then
   TYPEOF_$1=long
elif test "${SIZEOF_$1}" = "${SIZEOF_LLONG}" ; then
   TYPEOF_$1='long long'
else
   AC_MSG_FAILURE([cannot equivalent integer type of '$2'],[2])
fi
AC_SUBST([TYPEOF_$1])
AC_MSG_RESULT([${TYPEOF_$1}])
])


dnl Synopsis:
dnl
dnl   NAUSICAA_C_ERRNO()
dnl
dnl Description:
dnl
dnl   Determine the values of the "errno" constants.
dnl
AC_DEFUN([NAUSICAA_C_ERRNO],[
NAUSICAA_VALUEOF_TEST([EPERM],[EPERM],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOENT],[ENOENT],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ESRCH],[ESRCH],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EINTR],[EINTR],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EIO],[EIO],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENXIO],[ENXIO],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([E2BIG],[E2BIG],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOEXEC],[ENOEXEC],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EBADF],[EBADF],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ECHILD],[ECHILD],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EAGAIN],[EAGAIN],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOMEM],[ENOMEM],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EACCES],[EACCES],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EFAULT],[EFAULT],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOTBLK],[ENOTBLK],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EBUSY],[EBUSY],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EEXIST],[EEXIST],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EXDEV],[EXDEV],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENODEV],[ENODEV],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOTDIR],[ENOTDIR],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EISDIR],[EISDIR],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EINVAL],[EINVAL],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENFILE],[ENFILE],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EMFILE],[EMFILE],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOTTY],[ENOTTY],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ETXTBSY],[ETXTBSY],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EFBIG],[EFBIG],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOSPC],[ENOSPC],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ESPIPE],[ESPIPE],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EROFS],[EROFS],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EMLINK],[EMLINK],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EPIPE],[EPIPE],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EDOM],[EDOM],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ERANGE],[ERANGE],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EDEADLK],[EDEADLK],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENAMETOOLONG],[ENAMETOOLONG],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOLCK],[ENOLCK],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOSYS],[ENOSYS],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOTEMPTY],[ENOTEMPTY],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ELOOP],[ELOOP],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EWOULDBLOCK],[EWOULDBLOCK],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOMSG],[ENOMSG],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EIDRM],[EIDRM],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ECHRNG],[ECHRNG],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EL2NSYNC],[EL2NSYNC],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EL3HLT],[EL3HLT],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EL3RST],[EL3RST],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ELNRNG],[ELNRNG],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EUNATCH],[EUNATCH],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOCSI],[ENOCSI],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EL2HLT],[EL2HLT],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EBADE],[EBADE],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EBADR],[EBADR],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EXFULL],[EXFULL],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOANO],[ENOANO],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EBADRQC],[EBADRQC],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EBADSLT],[EBADSLT],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EDEADLOCK],[EDEADLOCK],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EBFONT],[EBFONT],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOSTR],[ENOSTR],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENODATA],[ENODATA],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ETIME],[ETIME],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOSR],[ENOSR],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENONET],[ENONET],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOPKG],[ENOPKG],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EREMOTE],[EREMOTE],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOLINK],[ENOLINK],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EADV],[EADV],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ESRMNT],[ESRMNT],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ECOMM],[ECOMM],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EPROTO],[EPROTO],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EMULTIHOP],[EMULTIHOP],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EDOTDOT],[EDOTDOT],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EBADMSG],[EBADMSG],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EOVERFLOW],[EOVERFLOW],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOTUNIQ],[ENOTUNIQ],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EBADFD],[EBADFD],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EREMCHG],[EREMCHG],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ELIBACC],[ELIBACC],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ELIBBAD],[ELIBBAD],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ELIBSCN],[ELIBSCN],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ELIBMAX],[ELIBMAX],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ELIBEXEC],[ELIBEXEC],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EILSEQ],[EILSEQ],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ERESTART],[ERESTART],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ESTRPIPE],[ESTRPIPE],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EUSERS],[EUSERS],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOTSOCK],[ENOTSOCK],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EDESTADDRREQ],[EDESTADDRREQ],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EMSGSIZE],[EMSGSIZE],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EPROTOTYPE],[EPROTOTYPE],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOPROTOOPT],[ENOPROTOOPT],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EPROTONOSUPPORT],[EPROTONOSUPPORT],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ESOCKTNOSUPPORT],[ESOCKTNOSUPPORT],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EOPNOTSUPP],[EOPNOTSUPP],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EPFNOSUPPORT],[EPFNOSUPPORT],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EAFNOSUPPORT],[EAFNOSUPPORT],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EADDRINUSE],[EADDRINUSE],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EADDRNOTAVAIL],[EADDRNOTAVAIL],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENETDOWN],[ENETDOWN],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENETUNREACH],[ENETUNREACH],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENETRESET],[ENETRESET],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ECONNABORTED],[ECONNABORTED],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ECONNRESET],[ECONNRESET],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOBUFS],[ENOBUFS],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EISCONN],[EISCONN],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOTCONN],[ENOTCONN],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ESHUTDOWN],[ESHUTDOWN],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ETOOMANYREFS],[ETOOMANYREFS],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ETIMEDOUT],[ETIMEDOUT],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ECONNREFUSED],[ECONNREFUSED],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EHOSTDOWN],[EHOSTDOWN],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EHOSTUNREACH],[EHOSTUNREACH],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EALREADY],[EALREADY],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EINPROGRESS],[EINPROGRESS],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ESTALE],[ESTALE],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EUCLEAN],[EUCLEAN],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOTNAM],[ENOTNAM],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENAVAIL],[ENAVAIL],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EISNAM],[EISNAM],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EREMOTEIO],[EREMOTEIO],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EDQUOT],[EDQUOT],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOMEDIUM],[ENOMEDIUM],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EMEDIUMTYPE],[EMEDIUMTYPE],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ECANCELED],[ECANCELED],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOKEY],[ENOKEY],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EKEYEXPIRED],[EKEYEXPIRED],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EKEYREVOKED],[EKEYREVOKED],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EKEYREJECTED],[EKEYREJECTED],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([EOWNERDEAD],[EOWNERDEAD],[#f],[#include <errno.h>])
NAUSICAA_VALUEOF_TEST([ENOTRECOVERABLE],[ENOTRECOVERABLE],[#f],[#include <errno.h>])
])

dnl end of file
dnl Local Variables:
dnl mode: autoconf
dnl End:
