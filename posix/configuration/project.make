#
# Part of: Nausicaa/POSIX
# Contents: project specific makefile
# Date: Sun Jul 26, 2009
#
# Abstract
#
#
#
# Copyright (c) 2009, 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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

$(eval $(call nau-libraries,posix,nausicaa/posix))
$(eval $(call nau-libraries,posix_clang,nausicaa/posix/clang))
$(eval $(call nau-libraries,posix_fd,nausicaa/posix/fd))
$(eval $(call nau-libraries,posix_file,nausicaa/posix/file))
$(eval $(call nau-libraries,posix_process,nausicaa/posix/process))
$(eval $(call nau-libraries,posix_sockets,nausicaa/posix/sockets))
$(eval $(call nau-libraries,posix_signals,nausicaa/posix/signals))
$(eval $(call nau-libraries,posix_system,nausicaa/posix/system))
$(eval $(call nau-libraries,posix_time,nausicaa/posix/time))

$(eval $(call nau-libraries,glibc,nausicaa/glibc))
$(eval $(call nau-libraries,glibc_cstrings,nausicaa/glibc/cstrings))
$(eval $(call nau-libraries,glibc_file,nausicaa/glibc/file))
$(eval $(call nau-libraries,glibc_signals,nausicaa/glibc/signals))
$(eval $(call nau-libraries,glibc_sockets,nausicaa/glibc/sockets))
$(eval $(call nau-libraries,glibc_streams,nausicaa/glibc/streams))
$(eval $(call nau-libraries,glibc_system,nausicaa/glibc/system))
$(eval $(call nau-libraries,glibc_time,nausicaa/glibc/time))

$(eval $(call nau-libraries,linux,nausicaa/linux))
$(eval $(call nau-libraries,linux_fd,nausicaa/linux/fd))

### end of file
# Local Variables:
# mode: makefile-gmake
# End:
