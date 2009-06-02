dnl configure.ac --
dnl

AC_INIT([nausicaa-scheme],[0.1d0],[marcomaggi@gna.org])
DS_BEGIN([./src/libraries/scheme.sls])
AC_CANONICAL_TARGET
AC_SUBST([NAUSICAA_OS],[$target_os])
NAUSICAA_BEGIN
DS_EXCLUDE_SECTION([CONFIG_INSPECTION_SCRIPT])
DS_EXCLUDE_SECTION([AUTOCONF])
DS_EXCLUDE_SECTION([PKGCONFIG])
DS_ENABLE_OPTION([nau_scheme_ENABLE_BINFMT],[binfmt],[no],
   [whether usage of the binfmt_misc linux kernel module is enabled],
   [enable usage of the binfmt_misc linux kernel module])

AC_CONFIG_FILES([../src/libraries/scheme/registry.sls:../src/libraries/scheme/registry.sls.in])
DS_CONFIG_FILE_SLACKWARE_BIN_DESC
DS_CONFIG_FILE_SLACKWARE_DOC_DESC
NAUSICAA_END
DS_END

dnl end of file
