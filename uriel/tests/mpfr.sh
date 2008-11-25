#!/bin/sh
#
# Run the MPFR test.

export IKARUS_LIBRARY_PATH=../libraries:${IKARUS_LIBRARY_PATH}

exec scheme-script ../tests/mpfr.sps

### end of file
