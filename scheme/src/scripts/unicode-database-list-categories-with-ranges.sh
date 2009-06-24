#!/bin/sh
# unicode-database-list-categories-with-ranges.sh --
#
# This  script extracts  from "UnicodeData.txt"  all the  category codes
# whose  categories  use  ranges  in  the database;  the  "Cs"  category
# (surrogate characters) is excluded.

DATABASE=${1:?'missing UnicodeData.txt pathname'}

grep ', *\(First\|Last\)>' <"$DATABASE" | \
    grep -v Cs | cut -d';' -f3 | sort | uniq

### end of file
