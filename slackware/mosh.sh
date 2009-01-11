#
# Part of: Nausicaa/Slackware
# Contents: unofficial Slackware build script for Mosh Scheme
# Date: Sat Jan 10, 2009
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

set -ex

## ------------------------------------------------------------
## Setup.

srcdir=${PWD}

: ${TMPDIR:=/tmp}
: ${prefix:=/usr/local}
: ${ARCHITECTURE:=i686}
: ${CXXFLAGS:='-O3 -march=i686 -mtune=i686'}
: ${CONFIG_OPTIONS:=}
# : ${CPPFLAGS:=}

NAME=mosh
VERSION=${1:?'missing version parameter'}
BUILD_VERSION=${2:?'missing build version parameter'}

PACKAGE_NAME=${NAME}-${VERSION}-${ARCHITECTURE}-${BUILD_VERSION}.tgz

DESTDIR=${TMPDIR}/${NAME}-${VERSION}

## ------------------------------------------------------------
## Programs.

: ${CHMOD:=/usr/bin/chmod}
: ${CHOWN:=/usr/bin/chown}
: ${CP:=/bin/cp}
: ${FIND:=/usr/bin/find}
: ${MAKE:=/usr/bin/make}
: ${MAKEPKG:=/sbin/makepkg}
: ${RM:=/bin/rm}
: ${SUDO:=/usr/bin/sudo}
: ${STRIP:=/usr/bin/strip}
: ${XARGS:=/usr/bin/xargs}

## ------------------------------------------------------------
## Configuration.

./configure \
    --prefix="${prefix}"                        \
    ${CONFIG_OPTIONS}                           \
    CXXFLAGS="${CXXFLAGS}"

## ------------------------------------------------------------
## Building.

"${MAKE}" all PREFIX="${prefix}"
"${SUDO}" "${RM}" -frv "${DESTDIR}"
"${MAKE}" install PREFIX="${prefix}" DESTDIR="${DESTDIR}"

cd "${DESTDIR}${prefix}"
{
    "${FIND}"        -type d | "${XARGS}" "${CHMOD}" 0755
    "${RM}" -fv bin/{test_port,test_vm,test_fasl}
    ("${FIND}" bin   -type f | "${XARGS}" "${STRIP}" -s) || true
    ("${FIND}" bin   -type f | "${XARGS}" "${CHMOD}" 0555) || true
    ("${FIND}" share -type f | "${XARGS}" "${CHMOD}" 0444) || true
}

cd "${DESTDIR}"
{
    "${SUDO}" "${CHOWN}" root.root * --recursive
    "${SUDO}" "${MAKEPKG}" --chown y "${PACKAGE_NAME}"
    "${SUDO}" "${CHMOD}" 0666 "${PACKAGE_NAME}"
    "${CP}" "${PACKAGE_NAME}" "${srcdir}"
    "${SUDO}" "${RM}" -frv "${DESTDIR}"
}

"${MAKE}" clean distclean

exit 0

### end of file
