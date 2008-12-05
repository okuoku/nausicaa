# configure.ac --
#

AC_INIT([nausicaa-posix],[0.1d0])
AC_PREREQ(2.60)
AC_CONFIG_SRCDIR([libraries/compile-all.sps])
NAUSICAA_BEGIN()
DS_IKARUS_CHECK_LIBRARY([URIEL_FFI],[(uriel ffi)])
NAUSICAA_SYSTEM_SETUP()
NAUSICAA_C_LANGUAGE()
NAUSICAA_SIZEOF()

dnl page
dnl --------------------------------------------------------------------
dnl Inspection: type definitions and sizes.
dnl --------------------------------------------------------------------

NAUSICAA_SIZEOF_TEST([MODE_T],[mode_t],[#f],[
#include <fcntl.h>
#include <unistd.h>
])
NAUSICAA_UINTTYPE_TEST([MODE_T],[mode_t])

NAUSICAA_SIZEOF_TEST([OFF_T],[off_t],[#f],[
#include <fcntl.h>
#include <unistd.h>
])
NAUSICAA_UINTTYPE_TEST([OFF_T],[off_t])


NAUSICAA_SIZEOF_TEST([STRUCT_FLOCK],[struct flock],[#f],[
#include <fcntl.h>
])


dnl page
dnl --------------------------------------------------------------------
dnl Inspection: fields.
dnl --------------------------------------------------------------------

AC_DEFUN([NAU_POSIX_OFFSETOF],[
NAUSICAA_OFFSETOF_TEST([$1],[$2],[$3],[
#include <fcntl.h>
#include <unistd.h>
])
])

NAU_POSIX_OFFSETOF([STRUCT_FLOCK_L_TYPE],[struct flock],[l_type])
NAU_POSIX_OFFSETOF([STRUCT_FLOCK_L_WHENCE],[struct flock],[l_whence])
NAU_POSIX_OFFSETOF([STRUCT_FLOCK_L_START],[struct flock],[l_start])
NAU_POSIX_OFFSETOF([STRUCT_FLOCK_L_LEN],[struct flock],[l_len])
NAU_POSIX_OFFSETOF([STRUCT_FLOCK_L_PID],[struct flock],[l_pid])


dnl page
dnl --------------------------------------------------------------------
dnl Inspection: file descriptors related constants.
dnl --------------------------------------------------------------------

AC_DEFUN([NAU_POSIX_VALUEOF_O],[
NAUSICAA_VALUEOF_TEST([$1],[$1],[#f],[
#include <fcntl.h>
#include <unistd.h>
])
])

dnl In alphabetical order.
NAU_POSIX_VALUEOF_O([O_ACCMODE])
NAU_POSIX_VALUEOF_O([O_APPEND])
NAU_POSIX_VALUEOF_O([O_ASYNC])
NAU_POSIX_VALUEOF_O([O_CREAT])
NAU_POSIX_VALUEOF_O([O_EXCL])
NAU_POSIX_VALUEOF_O([O_EXEC])
NAU_POSIX_VALUEOF_O([O_EXLOCK])
NAU_POSIX_VALUEOF_O([O_FSYNC])
NAU_POSIX_VALUEOF_O([O_IGNORE_CTTY])
NAU_POSIX_VALUEOF_O([O_NDELAY])
NAU_POSIX_VALUEOF_O([O_NOATIME])
NAU_POSIX_VALUEOF_O([O_NOCTTY])
NAU_POSIX_VALUEOF_O([O_NOLINK])
NAU_POSIX_VALUEOF_O([O_NONBLOCK])
NAU_POSIX_VALUEOF_O([O_NOTRANS])
NAU_POSIX_VALUEOF_O([O_RDONLY])
NAU_POSIX_VALUEOF_O([O_RDWR])
NAU_POSIX_VALUEOF_O([O_READ])
NAU_POSIX_VALUEOF_O([O_SHLOCK])
NAU_POSIX_VALUEOF_O([O_SYNC])
NAU_POSIX_VALUEOF_O([O_TRUNC])
NAU_POSIX_VALUEOF_O([O_WRITE])
NAU_POSIX_VALUEOF_O([O_WRONLY])

NAU_POSIX_VALUEOF_O([FD_CLOEXEC])

NAU_POSIX_VALUEOF_O([F_DUPFD])
NAU_POSIX_VALUEOF_O([F_GETFD])
NAU_POSIX_VALUEOF_O([F_GETFL])
NAU_POSIX_VALUEOF_O([F_GETLK])
NAU_POSIX_VALUEOF_O([F_GETOWN])
NAU_POSIX_VALUEOF_O([F_SETFD])
NAU_POSIX_VALUEOF_O([F_SETFL])
NAU_POSIX_VALUEOF_O([F_SETLKW])
NAU_POSIX_VALUEOF_O([F_SETLK])
NAU_POSIX_VALUEOF_O([F_SETOWN])

dnl locking
NAU_POSIX_VALUEOF_O([F_RDLCK])
NAU_POSIX_VALUEOF_O([F_WRLCK])
NAU_POSIX_VALUEOF_O([F_UNLCK])



dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../libraries/posix/sizeof.sls:libraries/posix/sizeof.sls.in])
NAUSICAA_END()


### end of file
