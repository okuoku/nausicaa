dnl (foreign crypto nettle sizeof) --
dnl
dnl Part of: Nausicaa
dnl Contents: foreign library inspection generation
dnl Date: Thu Jan  7, 2010
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
dnl
dnl This program is free software:  you can redistribute it and/or modify
dnl it under the terms of the  GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or (at
dnl your option) any later version.
dnl
dnl This program is  distributed in the hope that it  will be useful, but
dnl WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
dnl MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
dnl General Public License for more details.
dnl
dnl You should  have received  a copy of  the GNU General  Public License
dnl along with this program.  If not, see <http://www.gnu.org/licenses/>.
dnl


NAU_DS_WITH_OPTION([NETTLE_SHARED_OBJECT],[nettle-shared-object],[libnettle.so],
  [Nettle shared library file],[select Nettle shared library file])
NAU_DS_WITH_OPTION([HOGWEED_SHARED_OBJECT],[hogweed-shared-object],[libhogweed.so],
  [Hogweed shared library file],[select Hogweed shared library file])

dnl end of file
