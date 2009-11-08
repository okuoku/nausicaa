#!/bin/bash
## print-projects-not-template.sh --
##
##

echo ./scheme
find -maxdepth 2                        \
    -name configuration                 \
    -and -not -wholename '*scheme*'     \
    -and -not -wholename '*template*'   \
    -and -print | sed -e 's%/configuration%%' | sort

### end of file
