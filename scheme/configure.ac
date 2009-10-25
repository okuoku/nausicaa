dnl configure.ac --
dnl

AC_INIT([nausicaa],[0.1a1],[marcomaggi@gna.org])
DS_BEGIN([./src/libraries/nausicaa.ikarus.sls])
AC_CANONICAL_TARGET
AC_SUBST([NAUSICAA_OS],[$target_os])
NAUSICAA_BEGIN
DS_EXCLUDE_SECTION([CONFIG_INSPECTION_SCRIPT])
DS_EXCLUDE_SECTION([AUTOCONF])
DS_EXCLUDE_SECTION([PKGCONFIG])
DS_ENABLE_OPTION([nau_scheme_ENABLE_BINFMT],[binfmt],[no],
   [whether usage of the binfmt_misc linux kernel module is enabled],
   [enable usage of the binfmt_misc linux kernel module])
NAUSICAA_C_LANGUAGE
NAUSICAA_SIZEOF
NAUSICAA_C_TYPES_LIMITS
NAUSICAA_C_ERRNO

AC_CONFIG_FILES([../src/libraries/foreign/ffi/sizeof.sls:../src/libraries/foreign/ffi/sizeof.sls.in])
AC_CONFIG_FILES([../src/libraries/foreign/errno.sls:../src/libraries/foreign/errno.sls.in])
AC_CONFIG_FILES([../src/libraries/nausicaa/registry.sls:../src/libraries/nausicaa/registry.sls.in])
DS_CONFIG_FILE_SLACKWARE_BIN_DESC
DS_CONFIG_FILE_SLACKWARE_DOC_DESC
NAUSICAA_END
DS_END

dnl end of file
