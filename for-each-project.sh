#!/bin/bash
## for-each-project.sh --
##
##

SCRIPT=${1:?missing script}
shift
FULL_SCRIPT=$(type -path "$SCRIPT")

if test -x "$SCRIPT" -o "$FULL_SCRIPT"
then
    ./print-projects.sh | while read
    do "$SCRIPT" "$@" "$REPLY"
    done
else
    echo "*** selected script not executable ($SCRIPT, $FULL_SCRIPT)"
    exit 2
fi

exit 0

### end of file
