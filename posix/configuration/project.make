#
# Part of: Nausicaa/POSIX
# Contents: project specific makefile
# Date: Sun Jul 26, 2009
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

$(eval $(call nau-libraries,posix,posix))
$(eval $(call nau-libraries,posix_fd,posix/fd))
$(eval $(call nau-libraries,posix_file,posix/file))
$(eval $(call nau-libraries,posix_process,posix/process))
$(eval $(call nau-libraries,posix_sockets,posix/sockets))
$(eval $(call nau-libraries,posix_signals,posix/signals))
$(eval $(call nau-libraries,posix_system,posix/system))
$(eval $(call nau-libraries,posix_time,posix/time))

$(eval $(call nau-libraries,glibc,glibc))
$(eval $(call nau-libraries,glibc_cstrings,glibc/cstrings))
$(eval $(call nau-libraries,glibc_file,glibc/file))
$(eval $(call nau-libraries,glibc_signals,glibc/signals))
$(eval $(call nau-libraries,glibc_sockets,glibc/sockets))
$(eval $(call nau-libraries,glibc_streams,glibc/streams))
$(eval $(call nau-libraries,glibc_system,glibc/system))
$(eval $(call nau-libraries,glibc_time,glibc/time))


### end of file
# Local Variables:
# mode: makefile-gmake
# End:
