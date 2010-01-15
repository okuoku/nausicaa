#
# Part of: Nausicaa/Gcrypt
# Contents: project specific makefile
# Date: Sat Dec 26, 2009
#
# Abstract
#
#
#
# Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
#
# This program is  free software: you can redistribute  it and/or modify
# it under the  terms of the GNU General Public  License as published by
# the Free Software Foundation, either  version 3 of the License, or (at
# your option) any later version.
#
# This program  is distributed in the  hope that it will  be useful, but
# WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
# MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
# General Public License for more details.
#
# You  should have received  a copy  of the  GNU General  Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#page
## --------------------------------------------------------------------
## Installation of source and fasl libraries.
## --------------------------------------------------------------------

$(eval $(call nau-libraries,crypto,foreign/crypto))
$(eval $(call nau-libraries,gcrypt,foreign/crypto/gcrypt))



### end of file
# Local Variables:
# mode: makefile-gmake
# End:
