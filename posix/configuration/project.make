#
# Part of: Nausicaa/POSIX
# Contents: project specific makefile
# Date: Sun Jul 26, 2009
#
# Abstract
#
#
#
# Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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

$(eval $(call nau-libraries,posix,foreign/posix))
$(eval $(call nau-libraries,posix_environment,foreign/posix/environment))
$(eval $(call nau-libraries,posix_fd,foreign/posix/fd))
$(eval $(call nau-libraries,posix_file,foreign/posix/file))
$(eval $(call nau-libraries,posix_job,foreign/posix/job))
$(eval $(call nau-libraries,posix_process,foreign/posix/process))
$(eval $(call nau-libraries,posix_stat,foreign/posix/stat))
$(eval $(call nau-libraries,posix_time,foreign/posix/time))
##$(eval $(call nau-libraries,posix_users,foreign/posix/users))

$(eval $(call nau-libraries,glibc,foreign/glibc))
$(eval $(call nau-libraries,glibc_cstrings,foreign/glibc/cstrings))
$(eval $(call nau-libraries,glibc_environment,foreign/glibc/environment))
$(eval $(call nau-libraries,glibc_file,foreign/glibc/file))
$(eval $(call nau-libraries,glibc_streams,foreign/glibc/streams))
$(eval $(call nau-libraries,glibc_time,foreign/glibc/time))


### end of file
# Local Variables:
# mode: makefile-gmake
# End:
