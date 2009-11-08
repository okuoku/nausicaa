#!/bin/bash
## print-projects.sh --
##
##

echo ./scheme
find -maxdepth 2 \
    -name configuration -and -not -wholename '*scheme*' \
    -and -print | sed -e 's%/configuration%%' | sort

### end of file
