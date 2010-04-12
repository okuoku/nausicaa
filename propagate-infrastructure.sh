#!/bin/bash
## propagate-infrastructure.sh --
##
##

top_srcdir=$PWD
scheme_dir=${top_srcdir}/scheme/infrastructure

# Do NOT change this script to use "print-projects.sh" !!!
# Here we  are searching  for "infrastructure" directories,
# not project top directories.

find -maxdepth 2 \
    \( -name infrastructure -and -not -wholename '*scheme*' \) \
    -and -print0 | while IFS= read -d $'\x00'
    do
        echo "processing $REPLY ..."
        rm --force "$top_srcdir/$REPLY"/*
        pushd .
        {
            cd "$top_srcdir/$REPLY"
            cp --verbose "$scheme_dir"/* .
        }
        popd
    done

### end of file
