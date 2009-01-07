#
# Part of: Nausicaa/SRFI
# Contents: time tests with GNU date
# Date: Wed Jan  7, 2009
#
# Abstract
#
#   This shell scripts exercises  GNU date to acquire values
#   that can be used to  validate the results of the SRFI-19
#   (time and date functions) implementation.
#
#     Notice  that  without settin  LC_TIME  to "en_US"  the
#   AM/PM indicator will not  be printed by GNU date.  Dunno
#   why.
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

#page
## ------------------------------------------------------------
## Helper functions.
## ------------------------------------------------------------

function write () {
    local FORMAT=$1
    local DESCR=$2
    local THING=$(LC_TIME=en_US date --date="${DATE}" "+${FORMAT}" --utc)

    printf '%-40s %s = "%s"\n' "${DESCR}" "${FORMAT}" "${THING}"
}

#page
## ------------------------------------------------------------
## The test.
## ------------------------------------------------------------

function doit () {
    echo '------------------------------------------------------------'
    printf "$1"
    LC_TIME=en_US date --date="$DATE" --rfc-2822 --utc
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

#page
## ------------------------------------------------------------
## The dates.
## ------------------------------------------------------------

DATE='1950-06-05 04:03:02+0100'
doit 'Inspecting date with single digits: '

DATE='2000-10-11 12:13:14'
doit 'Inspecting date with 2 digits: '


#page
## ------------------------------------------------------------
## The results.
## ------------------------------------------------------------

# ------------------------------------------------------------
# Inspecting date with single digits: Mon, 05 Jun 1950 03:03:02 +0000
# weekday abbreviated                      %a = "Mon"
# weekday full                             %A = "Monday"
# month abbreviated                        %b = "Jun"
# month full                               %B = "June"
# date and time                            %c = "Mon 05 Jun 1950 03:03:02 AM UTC"
# day of month (zero padded)               %d = "05"
# date                                     %D = "06/05/50"
# day of month (blank padded)              %e = " 5"
# UNIMPLEMENTED (seconds and fractional)   %f = "%f"
# month abbreviated                        %h = "Jun"
# hour, zero padded, 24-hour clock         %H = "03"
# hour, zero padded, 12-hour clock         %I = "03"
# day of year, zero padded                 %j = "156"
# hour, blank padded, 24-hour clock        %k = " 3"
# hour, blank padded, 12-hour clock        %l = " 3"
# month, zero padded                       %m = "06"
# minute, zero padded                      %M = "03"
# newline (invisible)                      %n = ""
# nanosecond (unset)                       %N = "000000000"
# AM or PM, uppercase                      %p = "AM"
# AM or PM, lowercase                      %P = "am"
# time, 12-hour clock                      %r = "03:03:02 AM"
# count of seconds since the epoch         %s = "-617749018"
# seconds, zero padded                     %S = "02"
# tab                                      %t = "	"
# time, 24-hour clock                      %T = "03:03:02"
# week number of year (Sunday first)       %U = "23"
# week number of year (Monday first)       %V = "23"
# day of week                              %w = "1"
# week number of year (Monday first)       %W = "23"
# locale's date                            %x = "06/05/1950"
# locale's time                            %X = "03:03:02 AM"
# last digits of year                      %y = "50"
# year                                     %Y = "1950"
# time zone, +hhmm                         %z = "+0000"
# alphabetic time zone                     %Z = "UTC"

# ------------------------------------------------------------
# Inspecting date with 2 digits: Wed, 11 Oct 2000 12:13:14 +0000
# weekday abbreviated                      %a = "Wed"
# weekday full                             %A = "Wednesday"
# month abbreviated                        %b = "Oct"
# month full                               %B = "October"
# date and time                            %c = "Wed 11 Oct 2000 12:13:14 PM UTC"
# day of month (zero padded)               %d = "11"
# date                                     %D = "10/11/00"
# day of month (blank padded)              %e = "11"
# UNIMPLEMENTED (seconds and fractional)   %f = "%f"
# month abbreviated                        %h = "Oct"
# hour, zero padded, 24-hour clock         %H = "12"
# hour, zero padded, 12-hour clock         %I = "12"
# day of year, zero padded                 %j = "285"
# hour, blank padded, 24-hour clock        %k = "12"
# hour, blank padded, 12-hour clock        %l = "12"
# month, zero padded                       %m = "10"
# minute, zero padded                      %M = "13"
# newline (invisible)                      %n = ""
# nanosecond (unset)                       %N = "000000000"
# AM or PM, uppercase                      %p = "PM"
# AM or PM, lowercase                      %P = "pm"
# time, 12-hour clock                      %r = "12:13:14 PM"
# count of seconds since the epoch         %s = "971266394"
# seconds, zero padded                     %S = "14"
# tab                                      %t = "	"
# time, 24-hour clock                      %T = "12:13:14"
# week number of year (Sunday first)       %U = "41"
# week number of year (Monday first)       %V = "41"
# day of week                              %w = "3"
# week number of year (Monday first)       %W = "41"
# locale's date                            %x = "10/11/2000"
# locale's time                            %X = "12:13:14 PM"
# last digits of year                      %y = "00"
# year                                     %Y = "2000"
# time zone, +hhmm                         %z = "+0000"
# alphabetic time zone                     %Z = "UTC"


### end of file
