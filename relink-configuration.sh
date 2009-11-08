#!/bin/bash
## relink-configuration.sh --
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
        cd "$top_srcdir/$REPLY"
        ln --force "$scheme_dir"/nausicaa.* .
    done

### end of file
