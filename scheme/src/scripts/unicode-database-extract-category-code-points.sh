#!/bin/sh
# unicode-database-extract-category-code-points.sh --
#
# This script extracts  from "UnicodeData.txt" all the lines  of a given
# category, and prints  a chunk of Scheme code defining  a mixed list of
# characters and pairs representing double--closed ranges of characters.
#
# For example,  the output  for the @code{Co}  category, which  has only
# ranges is (reformatted to look human readable):
#
#   (define category-Co (quote ((#\xE000 . #\xF8FF)
#                               (#\xF0000 . #\xFFFFD)
#                               (#\x100000 . #\x10FFFD))))
#   (display "(define char-set:category/Co")(newline)
#   (char-set-write (apply char-set category-Co))(newline)
#   (display ")")(newline)


DATABASE=${1:?'missing UnicodeData.txt pathname'}

CATEGORY_CODES=$(cut -d';' -f3 <"$DATABASE" | sort | uniq | grep -v Cs)

echo '(import (rnrs) (char-sets))'

for CATEGORY in $CATEGORY_CODES
do
    echo processing category $CATEGORY >&2
    echo -n "(define category-$CATEGORY (quote ("
    {
        grep ";$CATEGORY;" <"$DATABASE"   | \
            grep -v ', *\(First\|Last\)>' | \
            cut -d';' -f1                 | \
            while read
        do echo -n "#\x$REPLY "
        done

        grep ";$CATEGORY;" <"$DATABASE"   | \
            grep ', *\(First\|Last\)>'    | \
            cut -d';' -f1                 | \
            while read
        do
            FIRST=$REPLY
            read
            LAST=$REPLY
            echo -n "(#\x$FIRST . #\x$LAST) "
        done
    }
    echo ')))'
    echo "(display \"(define char-set:category/$CATEGORY\")(newline)"
    echo "(char-set-write (apply char-set category-$CATEGORY))(newline)"
    echo '(display ")")(newline)'
    echo
done


### end of file
