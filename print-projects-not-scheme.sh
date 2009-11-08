#!/bin/bash
## print-projects-not-scheme.sh --
##
##

find -maxdepth 2                        \
    -name configuration                 \
    -and -not -wholename '*scheme*'     \
    -and -print | sed -e 's%/configuration%%' | sort

### end of file
