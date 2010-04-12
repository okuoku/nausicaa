#!/bin/bash
## propagate-common-configuration.sh --
##
##

top_srcdir=$PWD
scheme_dir=${top_srcdir}/scheme/configuration

# Do NOT change this script to use "print-projects.sh" !!!
# Here we are searching for "configuration" directories,
# not project top directories.

find -maxdepth 2 \
    \( -name configuration -and -not -wholename '*scheme*' \) \
    -and -print0 | while IFS= read -d $'\x00'
    do
        echo "processing $REPLY ..."
        rm --force "$top_srcdir/$REPLY"/nausicaa.*
        pushd .
        {
            cd "$top_srcdir/$REPLY"
            cp --verbose "$scheme_dir"/nausicaa.* .
        }
        popd
    done

### end of file
