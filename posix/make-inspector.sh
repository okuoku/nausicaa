#!/bin/bash
#

set -x

TOP_DIR=~/src/devel/scheme/nausicaa/posix
export SCHEME_LIBPATH=${TOP_DIR}/src/libraries:$SCHEME_LIBPATH

cd "$TOP_DIR"
vicare-script make-inspector.sps

### end of file
