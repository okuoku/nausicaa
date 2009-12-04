#
# Part of: Nausicaa/Bzlib
# Contents: project specific makefile
# Date: Fri Dec  4, 2009
#
# Abstract
#
#
#
# Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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
## Documentation targets.
## --------------------------------------------------------------------

$(ds_texi_BUILDDIR)/nausicaa-bzlib.info: $(ds_texi_SRCDIR)/bzlib.texiinc
$(ds_texi_BUILDDIR)/nausicaa-bzlib.html: $(ds_texi_SRCDIR)/bzlib.texiinc
$(ds_texi_BUILDDIR)/nausicaa-bzlib.dvi:  $(ds_texi_SRCDIR)/bzlib.texiinc
$(ds_texi_BUILDDIR)/nausicaa-bzlib.pdf:  $(ds_texi_SRCDIR)/bzlib.texiinc

#page
## --------------------------------------------------------------------
## Installation of source and fasl libraries.
## --------------------------------------------------------------------

$(eval $(call nau-libraries,compression,foreign/compression))
$(eval $(call nau-libraries,bzlib,foreign/compression/bzlib))


### end of file
# Local Variables:
# mode: makefile-gmake
# End:
