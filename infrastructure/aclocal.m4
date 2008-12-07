dnl
dnl Part of: Nausicaa
dnl Contents: autoconf macros
dnl Date: Thu Nov 13, 2008
dnl
dnl Abstract
dnl
dnl   This is  a library of GNU Autoconf  macros to be used  by all the
dnl   Nausicaa "configure.ac" templates.  To use this file it is enough
dnl   o create a symbolic link from the project directory to this file.
dnl
dnl     Full  documentation  for the  macros  in  this  file is  in  the
dnl   Nausicaa general documentation, under the "doc" directory of the
dnl   top source tree.
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

dnl 1 VARIABLE
dnl 2 IDENTIFIER
dnl 3 DEFAULT
dnl 4 CHECKING-DESCRIPTION
dnl 5 OPTION-DESCRIPTION
AC_DEFUN([NAUSICAA_WITH_OPTION],[AC_MSG_CHECKING([$4])
AC_ARG_WITH([$2],
   AC_HELP_STRING([--with-$2],[$5 (default: $3)]),
   [if test -n "$withval" ; then $1=$withval ; else $1=$withval ; fi],
   [$1=$3])
AC_MSG_RESULT([$[]$1])
AC_SUBST([$1])
])

dnl 1 VARIABLE
dnl 2 PROGRAM_NAME
dnl 3 PROGRAM_DESCRIPTION
AC_DEFUN([NAUSICAA_PROGRAM],[
AC_PATH_PROG([$1],[$2],[:])
AC_ARG_VAR([$1],[$3])
])

dnl 1 WITH_TEMP_FILE_CHUNK
dnl 2 AFTER_CHUNK
AC_DEFUN([NAUSICAA_WITH_TMPFILE],[: ${TMPDIR=/tmp}
{
    nausicaa_private_TMPDIR=`
    (umask 077 && mktemp -d "$TMPDIR/fooXXXXXX") 2>/dev/null
    ` &&
    test -n "${nausicaa_private_TMPDIR}" && test -d "${nausicaa_private_TMPDIR}"
} || {
    nausicaa_private_TMPDIR=${TMPDIR}/foo$$-$RANDOM
    (umask 077 && mkdir "${nausicaa_private_TMPDIR}")
} || exit $?
nausicaa_TMPFILE=${nausicaa_private_TMPDIR}/temporary.txt
dnl Chunk with temporary file usage.
$1
rm -fr "${nausicaa_private_TMPDIR}"
dnl Chunk after temporary file usage.
$2
])

dnl 1 variable_suffix
dnl 2 default_value
AC_DEFUN([NAUSICAA_DEFAULT_VALUE],[
nausicaa_default_$1=$3
test -z "$nausicaa_default_$1" && nausicaa_default_$1='#f'
])


dnl page
dnl --------------------------------------------------------------------
dnl Common blocks.
dnl --------------------------------------------------------------------

AC_DEFUN([NAUSICAA_COMMON_PROGRAMS],[
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
AC_CACHE_SAVE
])

AC_DEFUN([NAUSICAA_SCHEME_PROGRAMS],[
NAUSICAA_PROGRAM([IKARUS],[ikarus],[the Ikarus Scheme executable])
NAUSICAA_PROGRAM([SCHEME_SCRIPT],[scheme-script],[the scheme-script executable])
NAUSICAA_PROGRAM([YPSILON],[ypsilon],[another R6RS Scheme])
AC_CACHE_SAVE
])

AC_DEFUN([NAUSICAA_COMMON_DIRECTORIES],[
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

AC_DEFUN([NAUSICAA_OPTIONS],[
NAUSICAA_ENABLE_OPTION([nausicaa_ENABLE_DOC],[doc],[yes],
  [whether documentation files will be installed],
  [enable installation of documentation files])
NAUSICAA_ENABLE_OPTION([nausicaa_ENABLE_DOC_INFO],[doc-info],[yes],
  [whether documentation in Info format will be installed],
  [enable installation of Info documentation])
NAUSICAA_ENABLE_OPTION([nausicaa_ENABLE_DOC_HTML],[doc-html],[no],
  [whether documentation in HTML format will be installed],
  [enable installation of HTML documentation])
NAUSICAA_ENABLE_OPTION([nausicaa_ENABLE_FASL],[fasl],[no],
  [whether compiled files will be built and installed],
  [enable installation of precompiled libraries])
NAUSICAA_ENABLE_OPTION([nausicaa_ENABLE_SLS],[sls],[yes],
  [whether source files will be installed],
  [enable installation of source files])
])

dnl page
dnl --------------------------------------------------------------------
dnl Main macros.
dnl --------------------------------------------------------------------

AC_DEFUN([NAUSICAA_BEGIN],[
AC_PREREQ(2.60)
AC_CONFIG_AUX_DIR([../infrastructure])
AC_CONFIG_SRCDIR([libraries/compile-all.sps])
NAUSICAA_OPTIONS()
NAUSICAA_COMMON_PROGRAMS()
NAUSICAA_SCHEME_PROGRAMS()
NAUSICAA_COMMON_DIRECTORIES()
NAUSICAA_SLACKWARE_TOOLS()
NAUSICAA_REDHAT_TOOLS()
NAUSICAA_PACMAN_TOOLS()
AC_CACHE_SAVE
])

AC_DEFUN([NAUSICAA_END],[
dnl Notice that AC_OUTPUT automatically calls AC_CACHE_SAVE.
AC_CONFIG_FILES([meta.d/slackware/slack-desc:meta/slackware/slack-desc.in])
AC_CONFIG_FILES([meta.d/redhat/spec-file:meta/redhat/spec-file.in])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
])


dnl page
dnl --------------------------------------------------------------------
dnl Macros for Ikarus Scheme.
dnl --------------------------------------------------------------------

dnl 1 SCHEME_CODE
dnl 2 ADDITIONAL_IKARUS_OPTIONS
dnl 3 AFTER_SHELL_CODE
AC_DEFUN([NAUSICAA_WITH_OUTPUT_FROM_IKARUS_SCRIPT],[
NAUSICAA_WITH_TMPFILE([
nausicaa_ANSWER=`echo '$1' >"${nausicaa_TMPFILE}"
"${IKARUS}" --r6rs-script "${nausicaa_TMPFILE}" $2`
],[$3])
])

dnl 1 OUTPUT_VARIABLE_COMPONENT_NAME
dnl 2 LIBRARY_IMPORT_SPEC
dnl 3 OPTIONAL_ACTION_IF_FOUND
dnl 4 OPTIONAL_ACTION_IF_FOUND
AC_DEFUN([NAUSICAA_IKARUS_CHECK_LIBRARY],[AC_MSG_CHECKING([availability of Ikarus library $2])
NAUSICAA_WITH_OUTPUT_FROM_IKARUS_SCRIPT([(import (rnrs) (rnrs eval (6)))
(with-exception-handler
  (lambda (ex)
    (display "no\n")
    (exit))
  (lambda ()
    (environment (quote $2))
    (display "yes\n")))
],,[AC_MSG_RESULT([$nausicaa_ANSWER])
AC_SUBST([HAS_IKARUS_LIB_$1],[$nausicaa_ANSWER])
if test "$nausicaa_ANSWER" = yes ; then
dnl action if found
:
$3
else
dnl action if not found
AC_MSG_WARN([Ikarus Scheme could not find library])
$4
fi
])
])

dnl page
dnl --------------------------------------------------------------------
dnl Macros for Ypsilon Scheme.
dnl --------------------------------------------------------------------

dnl 1 SCHEME_CODE
dnl 2 ADDITIONAL_YPSILON_OPTIONS
dnl 3 AFTER_SHELL_CODE
AC_DEFUN([NAUSICAA_WITH_OUTPUT_FROM_YPSILON_SCRIPT],[
NAUSICAA_WITH_TMPFILE([
nausicaa_ANSWER=`echo '$1' >"${nausicaa_TMPFILE}"
"${YPSILON}" --r6rs "${nausicaa_TMPFILE}" $2`
],[$3])
])

dnl 1 OUTPUT_VARIABLE_COMPONENT_NAME
dnl 2 LIBRARY_IMPORT_SPEC
dnl 3 OPTIONAL_ACTION_IF_FOUND
dnl 4 OPTIONAL_ACTION_IF_FOUND
AC_DEFUN([NAUSICAA_YPSILON_CHECK_LIBRARY],[AC_MSG_CHECKING([availability of Ypsilon library $2])
NAUSICAA_WITH_OUTPUT_FROM_YPSILON_SCRIPT([(import (rnrs) (rnrs eval (6)))
(with-exception-handler
  (lambda (ex)
    (display "no\n")
    (exit))
  (lambda ()
    (environment (quote $2))
    (display "yes\n")))
],[--compatible],[AC_MSG_RESULT([$nausicaa_ANSWER])
AC_SUBST([HAS_YPSILON_LIB_$1],[$nausicaa_ANSWER])
if test "$nausicaa_ANSWER" = yes ; then
dnl action if found.
:
$3
else
dnl action if not found.
:
AC_MSG_WARN([Ypsilon Scheme could not find library '$2'])
$4
fi
])
])


dnl page
dnl --------------------------------------------------------------------
dnl C language system setup.
dnl --------------------------------------------------------------------

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

AC_DEFUN([NAUSICAA_C_LANGUAGE],[
AC_PROG_CC
AC_PROG_CC_C99
AC_HEADER_STDC
AC_CHECK_HEADERS([errno.h])
AC_CHECK_HEADERS([fcntl.h])
AC_CHECK_HEADERS([inttypes.h])
AC_CHECK_HEADERS([limits.h])
AC_CHECK_HEADERS([stdint.h])
AC_CHECK_HEADERS([unistd.h])
])


dnl page
dnl --------------------------------------------------------------------
dnl C language basic tests.
dnl --------------------------------------------------------------------

m4_define([NAUSICAA_INCLUDES_DEFAULT],[AC_INCLUDES_DEFAULT
#ifdef HAVE_ERRNO_H
#  include <errno.h>
#endif
#ifdef HAVE_FCNTL_H
#  include <fcntl.h>
#endif
#ifdef HAVE_INTTYPES_H
#  include <inttypes.h>
#endif
#ifdef HAVE_LIMITS_H
#  include <limits.h>
#endif
#ifdef HAVE_STDINT_H
#  include <stdint.h>
#endif
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
$1
])

dnl 1 output-variable-suffix
dnl 2 the-expression
dnl 3 optional-default-value
dnl 4 optional headers
AC_DEFUN([NAUSICAA_VALUEOF_TEST],[
NAUSICAA_DEFAULT_VALUE([valueof_$1],[$3])
AC_CACHE_CHECK([the value of '$2'],
   [nausicaa_cv_valueof_$1],
   [AC_COMPUTE_INT([nausicaa_cv_valueof_$1],
       [$2],
       [NAUSICAA_INCLUDES_DEFAULT([$4])],
       [nausicaa_cv_valueof_$1="$nausicaa_default_valueof_$1"])])
VALUEOF_$1="$nausicaa_cv_valueof_$1"
AC_SUBST([VALUEOF_$1])
])

dnl 1 output-variable-suffix
dnl 2 the-typedef
dnl 3 optional-default-value
dnl 4 optional headers
AC_DEFUN([NAUSICAA_SIZEOF_TEST],[
NAUSICAA_DEFAULT_VALUE([sizeof_$1],[$3])
AC_CACHE_CHECK([the size of '$2'],
  [nausicaa_cv_sizeof_$1],
  [AC_COMPUTE_INT([nausicaa_cv_sizeof_$1],
      [sizeof($2)],
      [NAUSICAA_INCLUDES_DEFAULT([$4])],
      [nausicaa_cv_valueof_$1="$nausicaa_default_valueof_$1"])])
SIZEOF_$1="$nausicaa_cv_sizeof_$1"
AC_SUBST([SIZEOF_$1])
])

dnl 1 variable-suffix
dnl 2 struct-typedef
dnl 3 struct-field-name
dnl 4 optional-headers
AC_DEFUN([NAUSICAA_OFFSETOF_FIELD_TEST],[
NAUSICAA_DEFAULT_VALUE([sizeof_$1],[#f])
AC_CACHE_CHECK([the offset of field '$3' in '$2'],
   [nausicaa_cv_offsetof_$1],
   [AC_COMPUTE_INT([nausicaa_cv_offsetof_$1],
       [offsetof($2,$3)],
       [NAUSICAA_INCLUDES_DEFAULT([$4])],
       [nausicaa_cv_offsetof_$1="$nausicaa_default_offsetof_$1"])])
OFFSETOF_$1="$nausicaa_cv_offsetof_$1"
AC_SUBST([OFFSETOF_$1])
])

dnl 1 variable-suffix
dnl 2 struct-typedef
dnl 3 struct-field-name
dnl 4 default-value
dnl 5 optional-headers
AC_DEFUN([NAUSICAA_SIZEOF_FIELD_TEST],[
NAUSICAA_DEFAULT_VALUE([sizeof_field_$1],[#f])
AC_CACHE_CHECK([the size of field '$3' in struct '$2'],
   [nausicaa_cv_sizeof_field_$1],
   [AC_RUN_IFELSE([AC_LANG_PROGRAM([NAUSICAA_INCLUDES_DEFAULT([$5])],[
$2 s;
FILE *f = fopen ("conftest.val", "w");
fprintf(f, "%d", sizeof(s.$3));
return ferror (f) || fclose (f) != 0;
])],
   [nausicaa_cv_sizeof_field_$1=`cat conftest.val`],
   [nausicaa_cv_sizeof_field_$1="$nausicaa_default_sizeof_field_$1"])
   rm -f conftest.val])
SIZEOF_$1="$nausicaa_cv_sizeof_field_$1"
AC_SUBST([SIZEOF_$1])
])


dnl page
dnl --------------------------------------------------------------------
dnl C language type inference from size.
dnl --------------------------------------------------------------------

dnl *** WARNING ***
dnl
dnl Remember that the parentheses have special meaning for m4, so we
dnl cannot use them in the shell 'case' construct.  This is why we use
dnl all these 'if' statements.

dnl 1 variable-suffix
dnl 2 type-definition
dnl 3 type-guess
AC_DEFUN([NAUSICAA_BASE_TYPE_TEST],[
if test "$3" = signed-integer ; then
    NAUSICAA_INTTYPE_TEST([$1],[$2])
elif test "$3" = unsigned-integer ; then
    NAUSICAA_UINTTYPE_TEST([$1],[$2])
elif test "$3" = float ; then
    NAUSICAA_FLOATTYPE_TEST([$1],[$2])
elif test "$3" = pointer ; then
    TYPEOF_$1=pointer
AC_SUBST([TYPEOF_$1])
fi])

dnl 1 variable-suffix
dnl 2 type-definition
AC_DEFUN([NAUSICAA_INTTYPE_TEST],[
AC_REQUIRE([NAUSICAA_SIZEOF])
NAUSICAA_DEFAULT_VALUE([typeof_$1],[#f])
AC_CACHE_CHECK([equivalent integer type of '$2'],
  [nausicaa_cv_typeof_$1],
  [if test "${SIZEOF_$1}" = "${SIZEOF_INT}" ; then
     nausicaa_cv_typeof_$1=signed-int
   elif test "${SIZEOF_$1}" = "${SIZEOF_CHAR}" ; then
     nausicaa_cv_typeof_$1=signed-char
   elif test "${SIZEOF_$1}" = "${SIZEOF_SHORT_INT}" ; then
     nausicaa_cv_typeof_$1=signed-short
   elif test "${SIZEOF_$1}" = "${SIZEOF_LONG}" ; then
     nausicaa_cv_typeof_$1=signed-long
   elif test "${SIZEOF_$1}" = "${SIZEOF_LLONG}" ; then
     nausicaa_cv_typeof_$1=signed-long-long
   else
     AC_MSG_WARN([cannot determine signed integer type of '$2'])
     nausicaa_cv_typeof_$1="$nausicaa_default_typeof_$1"
   fi])
TYPEOF_$1="$nausicaa_cv_typeof_$1"
AC_SUBST([TYPEOF_$1])
])

dnl 1 variable-suffix
dnl 2 type-definition
AC_DEFUN([NAUSICAA_UINTTYPE_TEST],[
AC_REQUIRE([NAUSICAA_SIZEOF])
NAUSICAA_DEFAULT_VALUE([typeof_$1],[#f])
AC_CACHE_CHECK([equivalent unsigned integer type of '$2'],
  [nausicaa_cv_typeof_$1],
  [if test "${SIZEOF_$1}" = "${SIZEOF_UINT}" ; then
     nausicaa_cv_typeof_$1=unsigned-int
   elif test "${SIZEOF_$1}" = "${SIZEOF_CHAR}" ; then
     nausicaa_cv_typeof_$1=unsigned-char
   elif test "${SIZEOF_$1}" = "${SIZEOF_SHORT_UINT}" ; then
     nausicaa_cv_typeof_$1=unsigned-short
   elif test "${SIZEOF_$1}" = "${SIZEOF_ULONG}" ; then
     nausicaa_cv_typeof_$1=unsigned-long
   elif test "${SIZEOF_$1}" = "${SIZEOF_ULLONG}" ; then
     nausicaa_cv_typeof_$1=unsigned-long-long
   else
     AC_MSG_WARN([cannot determine unsigned integer type of '$2'])
     nausicaa_cv_typeof_$1="$nausicaa_default_typeof_$1"
   fi])
TYPEOF_$1="$nausicaa_cv_typeof_$1"
AC_SUBST([TYPEOF_$1])
])

dnl 1 variable-suffix
dnl 2 type-definition
AC_DEFUN([NAUSICAA_FLOATTYPE_TEST],[
AC_REQUIRE([NAUSICAA_SIZEOF])
NAUSICAA_DEFAULT_VALUE([typeof_$1],[#f])
AC_CACHE_CHECK([equivalent floating point type of '$2'],
  [nausicaa_cv_typeof_$1],
  [if test "${SIZEOF_$1}" = "${SIZEOF_FLOAT}" ; then
     nausicaa_cv_typeof_$1=float
   elif test "${SIZEOF_$1}" = "${SIZEOF_DOUBLE}" ; then
     nausicaa_cv_typeof_$1=double
   elif test "${SIZEOF_$1}" = "${SIZEOF_LONG_DOUBLE}" ; then
     nausicaa_cv_typeof_$1=long-double
   else
     AC_MSG_WARN([cannot determine floating point type of '$2'])
     nausicaa_cv_typeof_$1="$nausicaa_default_typeof_$1"
   fi])
TYPEOF_$1="$nausicaa_cv_typeof_$1"
AC_MSG_RESULT([${TYPEOF_$1}])
])

dnl page
dnl --------------------------------------------------------------------
dnl C language types accessors.
dnl --------------------------------------------------------------------

dnl *** WARNING ***
dnl
dnl Remember that the parentheses have special meaning for m4, so we
dnl cannot use them in the shell 'case' construct.  This is why we use
dnl all these 'if' statements.

AC_DEFUN([NAUSICAA_ACCESSORS_TEST],[
NAUSICAA_GETTER_TEST([$1],[$2])
NAUSICAA_SETTER_TEST([$1],[$2])
])

AC_DEFUN([NAUSICAA_GETTER_TEST],[
NAUSICAA_DEFAULT_VALUE([getterof_$1],[#f])
AC_CACHE_CHECK([getter for type '$2'],
  [nausicaa_cv_getterof_$1],
  [if   test "${TYPEOF_$1}" = signed-char ; then
     nausicaa_cv_getterof_$1="pointer-ref-c-signed-char"
   elif test "${TYPEOF_$1}" = unsigned-char ; then
     nausicaa_cv_getterof_$1="pointer-ref-c-unsigned-char"
   elif test "${TYPEOF_$1}" = signed-int ; then
     nausicaa_cv_getterof_$1="pointer-ref-c-signed-int"
   elif test "${TYPEOF_$1}" = unsigned-int ; then
     nausicaa_cv_getterof_$1="pointer-ref-c-unsigned-int"
   elif test "${TYPEOF_$1}" = signed-short ; then
     nausicaa_cv_getterof_$1="pointer-ref-c-signed-short"
   elif test "${TYPEOF_$1}" = unsigned-short ; then
     nausicaa_cv_getterof_$1="pointer-ref-c-unsigned-short"
   elif test "${TYPEOF_$1}" = signed-long ; then
     nausicaa_cv_getterof_$1="pointer-ref-c-signed-long"
   elif test "${TYPEOF_$1}" = unsigned-long ; then
     nausicaa_cv_getterof_$1="pointer-ref-c-unsigned-long"
   elif test "${TYPEOF_$1}" = signed-long-long ; then
     nausicaa_cv_getterof_$1="pointer-ref-c-signed-long-long"
   elif test "${TYPEOF_$1}" = unsigned-long-long ; then
     nausicaa_cv_getterof_$1="pointer-ref-c-unsigned-long-long"
   elif test "${TYPEOF_$1}" = float ; then
     nausicaa_cv_getterof_$1="pointer-ref-c-float"
   elif test "${TYPEOF_$1}" = double ; then
     nausicaa_cv_getterof_$1="pointer-ref-c-double"
   elif test "${TYPEOF_$1}" = long-double ; then
     nausicaa_cv_getterof_$1="pointer-ref-c-long-double"
   elif test "${TYPEOF_$1}" = pointer ; then
     nausicaa_cv_getterof_$1="pointer-ref-c-pointer"
   else
     AC_MSG_WARN([cannot determine getter for '$2' from '$TYPEOF_$1'])
     nausicaa_cv_getterof_$1="$nausicaa_default_getterof_$1"
   fi])
GETTEROF_$1="$nausicaa_cv_getterof_$1"
AC_SUBST([GETTEROF_$1])
])

AC_DEFUN([NAUSICAA_SETTER_TEST],[
NAUSICAA_DEFAULT_VALUE([setterof_$1],[#f])
AC_CACHE_CHECK([setter for type '$2'],
  [nausicaa_cv_setterof_$1],
  [if   test "${TYPEOF_$1}" = signed-char       ; then
     nausicaa_cv_setterof_$1="pointer-set-c-char!"
   elif test "${TYPEOF_$1}" = unsigned-char     ; then
     nausicaa_cv_setterof_$1="pointer-set-c-char!"
   elif test "${TYPEOF_$1}" = signed-int        ; then
     nausicaa_cv_setterof_$1="pointer-set-c-int!"
   elif test "${TYPEOF_$1}" = unsigned-int      ; then
     nausicaa_cv_setterof_$1="pointer-set-c-int!"
   elif test "${TYPEOF_$1}" = signed-short      ; then
     nausicaa_cv_setterof_$1="pointer-set-c-short!"
   elif test "${TYPEOF_$1}" = unsigned-short    ; then
     nausicaa_cv_setterof_$1="pointer-set-c-short!"
   elif test "${TYPEOF_$1}" = signed-long       ; then
     nausicaa_cv_setterof_$1="pointer-set-c-long!"
   elif test "${TYPEOF_$1}" = unsigned-long     ; then
     nausicaa_cv_setterof_$1="pointer-set-c-long!"
   elif test "${TYPEOF_$1}" = signed-long-long  ; then
     nausicaa_cv_setterof_$1="pointer-set-c-long-long!"
   elif test "${TYPEOF_$1}" = unsigned-long-long; then
     nausicaa_cv_setterof_$1="pointer-set-c-long-long!"
   elif test "${TYPEOF_$1}" = float             ; then
     nausicaa_cv_setterof_$1="pointer-set-c-float!"
   elif test "${TYPEOF_$1}" = double            ; then
     nausicaa_cv_setterof_$1="pointer-set-c-double!"
   elif test "${TYPEOF_$1}" = long-double       ; then
     nausicaa_cv_setterof_$1="pointer-set-c-long-double!"
   elif test "${TYPEOF_$1}" = pointer           ; then
     nausicaa_cv_setterof_$1="pointer-set-c-pointer!"
   else
     AC_MSG_WARN([cannot determine setter for '${TYPEOF_$1}'],[2])
     nausicaa_cv_setterof_$1="$nausicaa_default_setterof_$1"
   fi])
SETTEROF_$1="$nausicaa_cv_setterof_$1"
AC_SUBST([SETTEROF_$1])
])

dnl page
dnl --------------------------------------------------------------------
dnl C language type full inspection.
dnl --------------------------------------------------------------------

dnl 1 variable-suffix
dnl 2 typedef
dnl 3 type-guess
dnl 4 default-value
dnl 5 optional-headers
AC_DEFUN([NAUSICAA_INSPECT_TYPE],[
NAUSICAA_SIZEOF_TEST([$1],[$2],[$4],[$5])
NAUSICAA_BASE_TYPE_TEST([$1],[$2],[$3])
NAUSICAA_ACCESSORS_TEST([$1],[$2])
])

dnl 1 variable-suffix
dnl 2 struct-typedef
dnl 3 field-name
dnl 4 type-guess
dnl 5 default-value
dnl 6 optional-headers
AC_DEFUN([NAUSICAA_INSPECT_FIELD_TYPE],[
NAUSICAA_OFFSETOF_FIELD_TEST([$1],[$2],[$3],[$6])
NAUSICAA_SIZEOF_FIELD_TEST([$1],[$2],[$3],[$5],[$6])
NAUSICAA_BASE_TYPE_TEST([$1],[$2.$3],[$4])
NAUSICAA_ACCESSORS_TEST([$1],[$2.$3])
])


dnl page
dnl --------------------------------------------------------------------
dnl Common sets of C language tests.
dnl --------------------------------------------------------------------

AC_DEFUN([NAUSICAA_SIZEOF],[
NAUSICAA_SIZEOF_TEST([CHAR],[char])
NAUSICAA_SIZEOF_TEST([SHORT_INT],[short int])
NAUSICAA_SIZEOF_TEST([SHORT_UINT],[unsigned short int])
NAUSICAA_SIZEOF_TEST([INT],[int])
NAUSICAA_SIZEOF_TEST([UINT],[unsigned int])
NAUSICAA_SIZEOF_TEST([LONG],[long])
NAUSICAA_SIZEOF_TEST([ULONG],[unsigned long])
NAUSICAA_SIZEOF_TEST([LLONG],[long long])
NAUSICAA_SIZEOF_TEST([ULLONG],[unsigned long long])
NAUSICAA_SIZEOF_TEST([FLOAT],[float])
NAUSICAA_SIZEOF_TEST([DOUBLE],[double])
NAUSICAA_SIZEOF_TEST([LONG_DOUBLE],[long double])
NAUSICAA_SIZEOF_TEST([POINTER],[void *])
AC_CACHE_SAVE
])

AC_DEFUN([NAUSICAA_C_TYPES_LIMITS],[
NAUSICAA_VALUEOF_TEST([CHAR_MAX],[CHAR_MAX])
NAUSICAA_VALUEOF_TEST([CHAR_MIN],[CHAR_MIN])
NAUSICAA_VALUEOF_TEST([SCHAR_MAX],[SCHAR_MAX])
NAUSICAA_VALUEOF_TEST([SCHAR_MIN],[SCHAR_MIN])
NAUSICAA_VALUEOF_TEST([UCHAR_MAX],[UCHAR_MAX])
NAUSICAA_VALUEOF_TEST([SHRT_MAX],[SHRT_MAX])
NAUSICAA_VALUEOF_TEST([SHRT_MIN],[SHRT_MIN])
NAUSICAA_VALUEOF_TEST([USHRT_MAX],[USHRT_MAX])
NAUSICAA_VALUEOF_TEST([INT_MAX],[INT_MAX])
NAUSICAA_VALUEOF_TEST([INT_MIN],[INT_MIN])
NAUSICAA_VALUEOF_TEST([UINT_MAX],[UINT_MAX])
NAUSICAA_VALUEOF_TEST([LONG_MAX],[LONG_MAX])
NAUSICAA_VALUEOF_TEST([LONG_MIN],[LONG_MIN])
NAUSICAA_VALUEOF_TEST([ULONG_MAX],[ULONG_MAX])
NAUSICAA_VALUEOF_TEST([LLONG_MAX],[LLONG_MAX])
NAUSICAA_VALUEOF_TEST([LLONG_MIN],[LLONG_MIN])
NAUSICAA_VALUEOF_TEST([ULLONG_MAX],[ULLONG_MAX])
NAUSICAA_VALUEOF_TEST([WCHAR_MAX],[WCHAR_MAX])
NAUSICAA_VALUEOF_TEST([SSIZE_MAX],[SSIZE_MAX])

AC_C_BIGENDIAN([AC_SUBST(WORDS_BIGENDIAN,[#t])],[AC_SUBST(WORDS_BIGENDIAN,[#f])])
AC_CACHE_SAVE
])

AC_DEFUN([NAUSICAA_C_ERRNO],[
NAUSICAA_VALUEOF_TEST([EPERM],[EPERM])
NAUSICAA_VALUEOF_TEST([ENOENT],[ENOENT])
NAUSICAA_VALUEOF_TEST([ESRCH],[ESRCH])
NAUSICAA_VALUEOF_TEST([EINTR],[EINTR])
NAUSICAA_VALUEOF_TEST([EIO],[EIO])
NAUSICAA_VALUEOF_TEST([ENXIO],[ENXIO])
NAUSICAA_VALUEOF_TEST([E2BIG],[E2BIG])
NAUSICAA_VALUEOF_TEST([ENOEXEC],[ENOEXEC])
NAUSICAA_VALUEOF_TEST([EBADF],[EBADF])
NAUSICAA_VALUEOF_TEST([ECHILD],[ECHILD])
NAUSICAA_VALUEOF_TEST([EAGAIN],[EAGAIN])
NAUSICAA_VALUEOF_TEST([ENOMEM],[ENOMEM])
NAUSICAA_VALUEOF_TEST([EACCES],[EACCES])
NAUSICAA_VALUEOF_TEST([EFAULT],[EFAULT])
NAUSICAA_VALUEOF_TEST([ENOTBLK],[ENOTBLK])
NAUSICAA_VALUEOF_TEST([EBUSY],[EBUSY])
NAUSICAA_VALUEOF_TEST([EEXIST],[EEXIST])
NAUSICAA_VALUEOF_TEST([EXDEV],[EXDEV])
NAUSICAA_VALUEOF_TEST([ENODEV],[ENODEV])
NAUSICAA_VALUEOF_TEST([ENOTDIR],[ENOTDIR])
NAUSICAA_VALUEOF_TEST([EISDIR],[EISDIR])
NAUSICAA_VALUEOF_TEST([EINVAL],[EINVAL])
NAUSICAA_VALUEOF_TEST([ENFILE],[ENFILE])
NAUSICAA_VALUEOF_TEST([EMFILE],[EMFILE])
NAUSICAA_VALUEOF_TEST([ENOTTY],[ENOTTY])
NAUSICAA_VALUEOF_TEST([ETXTBSY],[ETXTBSY])
NAUSICAA_VALUEOF_TEST([EFBIG],[EFBIG])
NAUSICAA_VALUEOF_TEST([ENOSPC],[ENOSPC])
NAUSICAA_VALUEOF_TEST([ESPIPE],[ESPIPE])
NAUSICAA_VALUEOF_TEST([EROFS],[EROFS])
NAUSICAA_VALUEOF_TEST([EMLINK],[EMLINK])
NAUSICAA_VALUEOF_TEST([EPIPE],[EPIPE])
NAUSICAA_VALUEOF_TEST([EDOM],[EDOM])
NAUSICAA_VALUEOF_TEST([ERANGE],[ERANGE])
NAUSICAA_VALUEOF_TEST([EDEADLK],[EDEADLK])
NAUSICAA_VALUEOF_TEST([ENAMETOOLONG],[ENAMETOOLONG])
NAUSICAA_VALUEOF_TEST([ENOLCK],[ENOLCK])
NAUSICAA_VALUEOF_TEST([ENOSYS],[ENOSYS])
NAUSICAA_VALUEOF_TEST([ENOTEMPTY],[ENOTEMPTY])
NAUSICAA_VALUEOF_TEST([ELOOP],[ELOOP])
NAUSICAA_VALUEOF_TEST([EWOULDBLOCK],[EWOULDBLOCK])
NAUSICAA_VALUEOF_TEST([ENOMSG],[ENOMSG])
NAUSICAA_VALUEOF_TEST([EIDRM],[EIDRM])
NAUSICAA_VALUEOF_TEST([ECHRNG],[ECHRNG])
NAUSICAA_VALUEOF_TEST([EL2NSYNC],[EL2NSYNC])
NAUSICAA_VALUEOF_TEST([EL3HLT],[EL3HLT])
NAUSICAA_VALUEOF_TEST([EL3RST],[EL3RST])
NAUSICAA_VALUEOF_TEST([ELNRNG],[ELNRNG])
NAUSICAA_VALUEOF_TEST([EUNATCH],[EUNATCH])
NAUSICAA_VALUEOF_TEST([ENOCSI],[ENOCSI])
NAUSICAA_VALUEOF_TEST([EL2HLT],[EL2HLT])
NAUSICAA_VALUEOF_TEST([EBADE],[EBADE])
NAUSICAA_VALUEOF_TEST([EBADR],[EBADR])
NAUSICAA_VALUEOF_TEST([EXFULL],[EXFULL])
NAUSICAA_VALUEOF_TEST([ENOANO],[ENOANO])
NAUSICAA_VALUEOF_TEST([EBADRQC],[EBADRQC])
NAUSICAA_VALUEOF_TEST([EBADSLT],[EBADSLT])
NAUSICAA_VALUEOF_TEST([EDEADLOCK],[EDEADLOCK])
NAUSICAA_VALUEOF_TEST([EBFONT],[EBFONT])
NAUSICAA_VALUEOF_TEST([ENOSTR],[ENOSTR])
NAUSICAA_VALUEOF_TEST([ENODATA],[ENODATA])
NAUSICAA_VALUEOF_TEST([ETIME],[ETIME])
NAUSICAA_VALUEOF_TEST([ENOSR],[ENOSR])
NAUSICAA_VALUEOF_TEST([ENONET],[ENONET])
NAUSICAA_VALUEOF_TEST([ENOPKG],[ENOPKG])
NAUSICAA_VALUEOF_TEST([EREMOTE],[EREMOTE])
NAUSICAA_VALUEOF_TEST([ENOLINK],[ENOLINK])
NAUSICAA_VALUEOF_TEST([EADV],[EADV])
NAUSICAA_VALUEOF_TEST([ESRMNT],[ESRMNT])
NAUSICAA_VALUEOF_TEST([ECOMM],[ECOMM])
NAUSICAA_VALUEOF_TEST([EPROTO],[EPROTO])
NAUSICAA_VALUEOF_TEST([EMULTIHOP],[EMULTIHOP])
NAUSICAA_VALUEOF_TEST([EDOTDOT],[EDOTDOT])
NAUSICAA_VALUEOF_TEST([EBADMSG],[EBADMSG])
NAUSICAA_VALUEOF_TEST([EOVERFLOW],[EOVERFLOW])
NAUSICAA_VALUEOF_TEST([ENOTUNIQ],[ENOTUNIQ])
NAUSICAA_VALUEOF_TEST([EBADFD],[EBADFD])
NAUSICAA_VALUEOF_TEST([EREMCHG],[EREMCHG])
NAUSICAA_VALUEOF_TEST([ELIBACC],[ELIBACC])
NAUSICAA_VALUEOF_TEST([ELIBBAD],[ELIBBAD])
NAUSICAA_VALUEOF_TEST([ELIBSCN],[ELIBSCN])
NAUSICAA_VALUEOF_TEST([ELIBMAX],[ELIBMAX])
NAUSICAA_VALUEOF_TEST([ELIBEXEC],[ELIBEXEC])
NAUSICAA_VALUEOF_TEST([EILSEQ],[EILSEQ])
NAUSICAA_VALUEOF_TEST([ERESTART],[ERESTART])
NAUSICAA_VALUEOF_TEST([ESTRPIPE],[ESTRPIPE])
NAUSICAA_VALUEOF_TEST([EUSERS],[EUSERS])
NAUSICAA_VALUEOF_TEST([ENOTSOCK],[ENOTSOCK])
NAUSICAA_VALUEOF_TEST([EDESTADDRREQ],[EDESTADDRREQ])
NAUSICAA_VALUEOF_TEST([EMSGSIZE],[EMSGSIZE])
NAUSICAA_VALUEOF_TEST([EPROTOTYPE],[EPROTOTYPE])
NAUSICAA_VALUEOF_TEST([ENOPROTOOPT],[ENOPROTOOPT])
NAUSICAA_VALUEOF_TEST([EPROTONOSUPPORT],[EPROTONOSUPPORT])
NAUSICAA_VALUEOF_TEST([ESOCKTNOSUPPORT],[ESOCKTNOSUPPORT])
NAUSICAA_VALUEOF_TEST([EOPNOTSUPP],[EOPNOTSUPP])
NAUSICAA_VALUEOF_TEST([EPFNOSUPPORT],[EPFNOSUPPORT])
NAUSICAA_VALUEOF_TEST([EAFNOSUPPORT],[EAFNOSUPPORT])
NAUSICAA_VALUEOF_TEST([EADDRINUSE],[EADDRINUSE])
NAUSICAA_VALUEOF_TEST([EADDRNOTAVAIL],[EADDRNOTAVAIL])
NAUSICAA_VALUEOF_TEST([ENETDOWN],[ENETDOWN])
NAUSICAA_VALUEOF_TEST([ENETUNREACH],[ENETUNREACH])
NAUSICAA_VALUEOF_TEST([ENETRESET],[ENETRESET])
NAUSICAA_VALUEOF_TEST([ECONNABORTED],[ECONNABORTED])
NAUSICAA_VALUEOF_TEST([ECONNRESET],[ECONNRESET])
NAUSICAA_VALUEOF_TEST([ENOBUFS],[ENOBUFS])
NAUSICAA_VALUEOF_TEST([EISCONN],[EISCONN])
NAUSICAA_VALUEOF_TEST([ENOTCONN],[ENOTCONN])
NAUSICAA_VALUEOF_TEST([ESHUTDOWN],[ESHUTDOWN])
NAUSICAA_VALUEOF_TEST([ETOOMANYREFS],[ETOOMANYREFS])
NAUSICAA_VALUEOF_TEST([ETIMEDOUT],[ETIMEDOUT])
NAUSICAA_VALUEOF_TEST([ECONNREFUSED],[ECONNREFUSED])
NAUSICAA_VALUEOF_TEST([EHOSTDOWN],[EHOSTDOWN])
NAUSICAA_VALUEOF_TEST([EHOSTUNREACH],[EHOSTUNREACH])
NAUSICAA_VALUEOF_TEST([EALREADY],[EALREADY])
NAUSICAA_VALUEOF_TEST([EINPROGRESS],[EINPROGRESS])
NAUSICAA_VALUEOF_TEST([ESTALE],[ESTALE])
NAUSICAA_VALUEOF_TEST([EUCLEAN],[EUCLEAN])
NAUSICAA_VALUEOF_TEST([ENOTNAM],[ENOTNAM])
NAUSICAA_VALUEOF_TEST([ENAVAIL],[ENAVAIL])
NAUSICAA_VALUEOF_TEST([EISNAM],[EISNAM])
NAUSICAA_VALUEOF_TEST([EREMOTEIO],[EREMOTEIO])
NAUSICAA_VALUEOF_TEST([EDQUOT],[EDQUOT])
NAUSICAA_VALUEOF_TEST([ENOMEDIUM],[ENOMEDIUM])
NAUSICAA_VALUEOF_TEST([EMEDIUMTYPE],[EMEDIUMTYPE])
NAUSICAA_VALUEOF_TEST([ECANCELED],[ECANCELED])
NAUSICAA_VALUEOF_TEST([ENOKEY],[ENOKEY])
NAUSICAA_VALUEOF_TEST([EKEYEXPIRED],[EKEYEXPIRED])
NAUSICAA_VALUEOF_TEST([EKEYREVOKED],[EKEYREVOKED])
NAUSICAA_VALUEOF_TEST([EKEYREJECTED],[EKEYREJECTED])
NAUSICAA_VALUEOF_TEST([EOWNERDEAD],[EOWNERDEAD])
NAUSICAA_VALUEOF_TEST([ENOTRECOVERABLE],[ENOTRECOVERABLE])
AC_CACHE_SAVE
])


dnl end of file
dnl Local Variables:
dnl mode: autoconf
dnl End:
