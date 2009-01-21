dnl configure.ac --
dnl

AC_INIT([nausicaa-scheme],[0.1d0])
NAUSICAA_BEGIN
AC_CANONICAL_TARGET
AC_SUBST([NAUSICAA_OS],[$target_os])
NAUSICAA_ENABLE_OPTION([nau_scheme_ENABLE_BINFMT],[binfmt],[no],
   [whether usage of the binfmt_misc linux kernel module is enabled],
   [enable usage of the binfmt_misc linux kernel module])
AC_SUBST([nau_scheme_ENABLE_BINFMT])
AC_CONFIG_FILES([bin.d/larceny-scheme-script:bin/larceny-scheme-script.in])
AC_CONFIG_FILES([bin.d/ikarus-scheme-script:bin/ikarus-scheme-script.in])
AC_CONFIG_FILES([bin.d/ypsilon-scheme-script:bin/ypsilon-scheme-script.in])
AC_CONFIG_FILES([bin.d/mosh-scheme-script:bin/mosh-scheme-script.in])
AC_CONFIG_FILES([fasl.d/scheme/registry.sls:libraries/scheme/registry.sls.in])
AC_CONFIG_FILES([Makefile.custom])
NAUSICAA_END


dnl end of file
