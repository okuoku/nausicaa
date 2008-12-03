# configure.ac --
#

AC_INIT([proofs],[0.1d0])
AC_PREREQ(2.60)
AC_CONFIG_SRCDIR([libraries/compile-all.sps])
NAUSICAA_BEGIN()
DS_IKARUS_CHECK_LIBRARY([URIEL_PRINTING],
        [(uriel printing)],,
        [AC_MSG_FAILURE(cannot find library 'uriel printing')])
DS_IKARUS_CHECK_LIBRARY([URIEL_TEST],
        [(uriel test)],,
        [AC_MSG_FAILURE(cannot find library 'uriel test')])
DS_IKARUS_CHECK_LIBRARY([SRFI_PARAMETERS],
        [(srfi parameters)],,
        [AC_MSG_FAILURE(cannot find library 'srfi parameters')])
NAUSICAA_END()

### end of file
