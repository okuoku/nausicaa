#!/bin/sh
#
# Run the MPFR test.

export YPSILON_SITELIB=../libraries:${YPSILON_SITELIB}

exec ypsilon --r6rs ../tests/mpfr.sps

### end of file
