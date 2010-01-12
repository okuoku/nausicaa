#
# Part of: Nausicaa/Nettle
# Contents: project specific makefile
# Date: Thu Jan  7, 2010
#
# Abstract
#
#
#
# Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
## Documentation.
## --------------------------------------------------------------------

$(ds_texi_BUILDDIR)/nausicaa-nettle.info: $(ds_texi_SRCDIR)/nettle.texiinc
$(ds_texi_BUILDDIR)/nausicaa-nettle.html: $(ds_texi_SRCDIR)/nettle.texiinc
$(ds_texi_BUILDDIR)/nausicaa-nettle.dvi:  $(ds_texi_SRCDIR)/nettle.texiinc
$(ds_texi_BUILDDIR)/nausicaa-nettle.pdf:  $(ds_texi_SRCDIR)/nettle.texiinc


#page
## --------------------------------------------------------------------
## Installation of source and fasl libraries.
## --------------------------------------------------------------------

$(eval $(call nau-libraries,crypto,foreign/crypto))
$(eval $(call nau-libraries,nettle,foreign/crypto/nettle))
$(eval $(call nau-libraries,hodweed,foreign/crypto/hogweed))


### end of file
# Local Variables:
# mode: makefile-gmake
# End:
