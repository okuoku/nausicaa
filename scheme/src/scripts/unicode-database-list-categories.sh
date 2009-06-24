#!/bin/sh
# unicode-database-list-categories.sh --
#
# This script  extracts from  "UnicodeData.txt" all the  category codes;
# the "Cs" category (surrogate characters) is excluded.

DATABASE=${1:?'missing UnicodeData.txt pathname'}

cut -d';' -f3 <"$DATABASE" | sort | uniq | grep -v Cs

### end of file
