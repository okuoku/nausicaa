#
# Part of: Nausicaa/SRFI
# Contents: time tests
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

# For no reason I can find in the manual page years before 1902 cannot
# be "clock scan"ned.  So here we stick to year>1902

set d [clock scan "19500605T04:03:02" -gmt no]

proc write {fmt descr} {
    global	d
    puts "$descr: [clock format $d -format $fmt -gmt no]"
}

puts "------------------------------------------------------------"
write %Z "Inspecting date"



puts "day name: [clock format $d -format %A -gmt yes]"
puts "number of seconds since the epoch: [clock format $d -format %s -gmt yes]"
puts "week number of year, Sunday first: [clock format $d -format %U]"
puts "week number of year, Monday first: [clock format $d -format %V]"

### end of file
