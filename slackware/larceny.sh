#
# Part of: Nausicaa/Slackware
# Contents: unofficial Slackware build script for Larceny Scheme
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
# echo this script is unfinished!!!
# exit 2

## ------------------------------------------------------------
## Setup.

srcdir=${PWD}

: ${TMPDIR:=/tmp}
: ${prefix:=/usr/local}
: ${ARCHITECTURE:=i686}

NAME=larceny
VERSION=${1:?'missing version parameter'}
BUILD_VERSION=${2:?'missing build version parameter'}

PACKAGE_NAME=${NAME}-${VERSION}-${ARCHITECTURE}-${BUILD_VERSION}.tgz

PKG_DIR=${NAME}-${VERSION}
DESTDIR=${TMPDIR}/${PKG_DIR}

bindir=${DESTDIR}${prefix}/bin
pkglibdir=${DESTDIR}${prefix}/lib/${NAME}/${VERSION}
pkglibexecdir=${DESTDIR}${prefix}/libexec/${NAME}/${VERSION}

## ------------------------------------------------------------
## Programs.

: ${SCHEME='/opt/larceny/5877/larceny -r5rs'}
: ${CHMOD:=/usr/bin/chmod}
: ${CHOWN:=/usr/bin/chown}
: ${CP:=/bin/cp}
: ${FIND:=/usr/bin/find}
: ${INSTALL:=/usr/bin/install}
: ${MAKEPKG:=/sbin/makepkg}
: ${RM:=/bin/rm}
: ${SUDO:=/usr/bin/sudo}
: ${STRIP:=/usr/bin/strip}
: ${TAR:=/usr/bin/tar}
: ${XARGS:=/usr/bin/xargs}

## ------------------------------------------------------------
## Configuration.


## ------------------------------------------------------------
## Building.

if false ; then
    export LARCENY_LIBPATH=
    echo "
(load \"setup.sch\")
(setup 'scheme: 'larceny 'host: 'linux86 'sassy 'string-rep: 'flat4)
(build-config-files)
(load-compiler)

(build-heap)
(build-runtime)
(build-executable)
(build-larceny-files)
" | ${SCHEME}

    echo "(exit)" | ./larceny.bin -stopcopy -- src/Build/iasn-larceny-heap.fasl
    echo "(exit)" | ./larceny.bin -stopcopy -- src/Build/iasn-twobit-heap.fasl
    cp larceny twobit

    echo "
(require 'r6rsmode)
(larceny:compile-r6rs-runtime)
(exit)
" | ./larceny
fi

## ------------------------------------------------------------

"${SUDO}" "${RM}" -frv "${DESTDIR}" || true
"${INSTALL}" -m 0700 -d "${DESTDIR}${prefix}"
"${INSTALL}" -m 0755 -d "${pkglibdir}"
"${INSTALL}" -m 0755 -d "${pkglibexecdir}"
"${INSTALL}" -m 0755 -d "${bindir}"

"${FIND}" ./lib \
    \( -name \*.sch    -or                          \
       -name \*.fasl   -or                          \
       -name \*.slfasl \)                           \
    -and -not -wholename \*test\*                   \
    -and -not -wholename \*in-progress\*            \
    -and -not -wholename \*Experimental\*           \
    -and -not -wholename \*examples\*               \
    |                                               \
    "${TAR}" --create  --file=- --files-from=- |    \
    "${TAR}" --extract --file=-                     \
    --directory="${pkglibdir}" --verbose

"${INSTALL}" -m 0644 \
    startup.sch sasstrap.heap \
    larceny.heap twobit.heap \
    "${pkglibexecdir}"

"${INSTALL}" -m 0755 \
    compile-stale larceny twobit larceny.bin \
    "${pkglibexecdir}"

echo "#!/bin/bash
#
# Launcher for Larceny Scheme.

DIR=\${0%/*}
LIBDIR=\${DIR}/../lib/${NAME}/${VERSION}
LIBEXECDIR=\${DIR}/../libexec/${NAME}/${VERSION}
export LARCENY_ROOT=\${LIBEXECDIR}
export LARCENY_LIBPATH=\${LIBDIR}:\${LIBDIR}/lib/R6RS:\${LARCENY_LIBPATH}

exec \"\${LARCENY_ROOT}/larceny.bin\" \\
    -heap \"\${LARCENY_ROOT}/larceny.heap\" \\
    \"\$@\"

### end of file
" >"${bindir}/larceny"

cd "${DESTDIR}${prefix}"
{
    "${FIND}"                     -type d | "${XARGS}" "${CHMOD}" 0755
    ("${FIND}" "${bindir}"        -type f | "${XARGS}" "${CHMOD}" 0555) || true
    ("${FIND}" "${pkglibexecdir}" -type f | "${XARGS}" "${CHMOD}" 0555) || true
    ("${FIND}" "${pkglibdir}"     -type f | "${XARGS}" "${CHMOD}" 0444) || true
}

#cd "${DESTDIR}"
cd "${DESTDIR}${prefix}"
{
    "${SUDO}" "${CHOWN}" root.root * --recursive
    "${SUDO}" "${MAKEPKG}" --chown y "${PACKAGE_NAME}"
    "${SUDO}" "${CHMOD}" 0666 "${PACKAGE_NAME}"
    "${CP}" -fv "${PACKAGE_NAME}" "${srcdir}"
    "${SUDO}" "${RM}" -frv "${DESTDIR}"
}

exit 0

### end of file
