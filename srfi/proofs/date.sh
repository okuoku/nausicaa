#
# Part of: Nausicaa/SRFI
# Contents: time tests with GNU date
# Date: Wed Jan  7, 2009
#
# Abstract
#
#
#
# Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
#
# This  program  is free  software:  you  can redistribute  it
# and/or modify it  under the terms of the  GNU General Public
# License as published by the Free Software Foundation, either
# version  3 of  the License,  or (at  your option)  any later
# version.
#
# This  program is  distributed in  the hope  that it  will be
# useful, but  WITHOUT ANY WARRANTY; without  even the implied
# warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
# PURPOSE.   See  the  GNU  General Public  License  for  more
# details.
#
# You should  have received a  copy of the GNU  General Public
# License   along   with    this   program.    If   not,   see
# <http://www.gnu.org/licenses/>.
#

function write () {
    local FORMAT=$1
    local DESCR=$2
    local THING=$(date --date="${DATE}" "+${FORMAT}")

    printf '%-40s %s = "%s"\n' "${DESCR}" "${FORMAT}" "${THING}"
}

function doit () {
    echo '------------------------------------------------------------'
    printf "$1"
    date --date="$DATE" --rfc-2822
    write %a 'weekday abbreviated'
    write %A 'weekday full'
    write %b 'month abbreviated'
    write %B 'month full'
    write %c 'date and time'
    write %d 'day of month (zero padded)'
    write %D 'date'
    write %e 'day of month (blank padded)'
    write %f 'UNIMPLEMENTED (seconds and fractional)'
    write %h 'month abbreviated'
    write %H 'hour, zero padded, 24-hour clock'
    write %I 'hour, zero padded, 12-hour clock'
    write %j 'day of year, zero padded'
    write %k 'hour, blank padded, 24-hour clock'
    write %l 'hour, blank padded, 12-hour clock'
    write %m 'month, zero padded'
    write %M 'minute, zero padded'
    write %n 'newline (invisible)'
    write %N 'nanosecond (unset)'
    write %p 'AM or PM, uppercase'
    write %P 'AM or PM, lowercase'
    write %r 'time, 12-hour clock'
    write %s 'count of seconds since the epoch'
    write %S 'seconds, zero padded'
    write %t 'tab'
    write %T 'time, 24-hour clock'
    write %U 'week number of year (Sunday first)'
    write %V 'week number of year (Monday first)'
    write %w 'day of week'
    write %W 'week number of year (Monday first)'
    write %x "locale's date"
    write %X "locale's time"
    write %y "last digits of year"
    write %Y "year"
    write %z "time zone, +hhmm"
    write %Z "alphabetic time zone"
}

DATE='1950-06-05 04:03:02+0100'
doit 'Inspecting date with single digits: '

DATE='2000-10-11 12:13:14+0000'
doit 'Inspecting date with 2 digits: '




### end of file
