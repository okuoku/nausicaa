#!/bin/bash
## propagate-readme-files.sh --
##
##

top_srcdir=$PWD
scheme_dir=${top_srcdir}/scheme

./print-projects-not-scheme.sh | while read
do
    echo "processing $REPLY ..."
    for item in README.nausicaa README.build README.rules
    do
        readme="$top_srcdir/$REPLY/$item"
        cp --force --verbose "$scheme_dir/$item" "$readme"
    done
done

### end of file
