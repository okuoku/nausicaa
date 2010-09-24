dnl configure.ac --
dnl

AC_INIT([nausicaa],[0.2d2],[marco.maggi-ipsu@poste.it])
DS_BEGIN([./src/libraries/nausicaa.vicare.sls])
AC_CANONICAL_TARGET
AC_SUBST([NAUSICAA_OS],[$target_os])
NAUSICAA_BEGIN
NAUSICAA_SYSTEM_SETUP
DS_EXCLUDE_SECTION([CONFIG_INSPECTION_SCRIPT])
DS_EXCLUDE_SECTION([AUTOCONF])
DS_EXCLUDE_SECTION([PKGCONFIG])
DS_ENABLE_OPTION([nau_scheme_ENABLE_BINFMT],[binfmt],[no],
   [whether usage of the binfmt_misc linux kernel module is enabled],
   [enable usage of the binfmt_misc linux kernel module])
NAUSICAA_C_LANGUAGE
NAUSICAA_SIZEOF
NAUSICAA_SIZEOF_AUX
NAUSICAA_C_TYPES_LIMITS
NAUSICAA_C_ERRNO

NAU_DS_WITH_OPTION([LIBC_SHARED_OBJECT_SPEC],[libc-shared-object],[],
  [C library shared object string],[select C library shared object string])

AC_CONFIG_FILES([./meta.d/makefiles/doc.make:../configuration/doc.make])
AC_CONFIG_FILES([../src/libraries/ffi/sizeof.sls:../src/libraries/ffi/sizeof.sls.in])
AC_CONFIG_FILES([../src/libraries/ffi/clang-data-types.sls:../src/libraries/ffi/clang-data-types.sls.in])
AC_CONFIG_FILES([../src/libraries/ffi/errno.sls:../src/libraries/ffi/errno.sls.in])
AC_CONFIG_FILES([../src/libraries/cond-expand/registry.sls:../src/libraries/cond-expand/registry.sls.in])
DS_CONFIG_FILE_SLACKWARE_BIN_DESC
DS_CONFIG_FILE_SLACKWARE_DOC_DESC
NAUSICAA_END
DS_END

dnl end of file
