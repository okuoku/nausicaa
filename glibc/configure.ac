# configure.ac --
#

AC_INIT([nausicaa-glibc],[0.1d0])
NAUSICAA_BEGIN
NAUSICAA_SCHEME_CHECK_LIBRARY([URIEL_FFI],[(uriel ffi)])
NAUSICAA_SYSTEM_SETUP
NAUSICAA_C_LANGUAGE
NAUSICAA_SIZEOF
NAUSICAA_VALUEOF_TEST([EOF],[EOF],,[#include <stdio.h>])
NAUSICAA_VALUEOF_TEST([SEEK_SET],[SEEK_SET],,[#include <stdio.h>])
NAUSICAA_VALUEOF_TEST([SEEK_CUR],[SEEK_CUR],,[#include <stdio.h>])
NAUSICAA_VALUEOF_TEST([SEEK_END],[SEEK_END],,[#include <stdio.h>])
AC_CONFIG_FILES([../libraries/glibc/sizeof.sls:libraries/glibc/sizeof.sls.in])
NAUSICAA_END


### end of file
